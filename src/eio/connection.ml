let src = Logs.Src.create "xobl.eio.reactor"

module Log = (val Logs.src_log src : Logs.LOG)

let seq_tag : int Logs.Tag.def =
  Logs.Tag.def "sequence_number" ~doc:"Sequence number of an X11 request"
    Format.pp_print_int

let seq_ seq = Logs.Tag.(empty |> add seq_tag seq)

let reporter ppf =
  let report (src : Logs.src) level ~over k msgf =
    let k _ =
      over ();
      k ()
    in
    let with_seq h tags k ppf fmt =
      let seq =
        match tags with None -> None | Some tags -> Logs.Tag.find seq_tag tags
      in
      match seq with
      | Some seq ->
          Format.kfprintf k ppf
            ("%a[%s][seq=%d] @[" ^^ fmt ^^ "@]@.")
            Logs.pp_header (level, h) (Logs.Src.name src) seq
      | None ->
          Format.kfprintf k ppf
            ("%a[%s] @[" ^^ fmt ^^ "@]@.")
            Logs.pp_header (level, h) (Logs.Src.name src)
    in
    msgf @@ fun ?header ?tags fmt -> with_seq header tags k ppf fmt
  in
  { Logs.report }

let classify_resp bytes =
  match Bytes.get bytes 0 with
  | '\x01' -> `Reply bytes
  | '\x00' -> `Error bytes
  | _ -> `Event bytes

module Cookie = struct
  type 'a decoder = Bytes.t -> at:int -> ('a * int) option

  type 'a t = {
    sequence_number : int;
    promise : Bytes.t option Eio.Promise.t;
    decode : 'a decoder option;
  }

  let create ?decode sequence_number promise =
    { sequence_number; promise; decode }

  let wait cookie =
    let buf = Eio.Promise.await cookie.promise in
    match (Option.map classify_resp buf, cookie.decode) with
    | Some (`Reply buf), Some decode ->
        Log.debug (fun m ->
            m "cookie: received reply" ~tags:(seq_ cookie.sequence_number));
        (* TODO return a more meaningful exception to the decode error *)
        let reply, _ = decode buf ~at:0 |> Option.get in
        Ok reply
    | Some (`Error buf), _ ->
        Log.debug (fun m ->
            m "cookie: received error" ~tags:(seq_ cookie.sequence_number));
        (* TODO decode error here *)
        Error buf
    | None, None ->
        Log.debug (fun m ->
            m "cookie: received empty reply" ~tags:(seq_ cookie.sequence_number));
        (* TODO Bad Obj.magic, bad! Please remove.
           The issue here is that a request that doesn't have a decode
           function will have a return type of unit in its cookie,
           and honestly I haven't found a way to encode that in the
           type system correctly. *)
        Ok (Obj.magic ())
    | Some (`Reply _), None ->
        Log.err (fun m ->
            m "cookie: received unexpected reply for a request with no decoder"
              ~tags:(seq_ cookie.sequence_number));
        (* TODO return a more meaningful exception *)
        Printf.ksprintf failwith "no reply expected: %d" cookie.sequence_number
    | _, _ ->
        Log.err (fun m ->
            m "cookie: received unexpected event"
              ~tags:(seq_ cookie.sequence_number));
        (* TODO return a more meaningful exception *)
        failwith "sent an event to an mvar"
end

module Cookie_reactor = struct
  type t = {
    promises : Bytes.t option Eio.Promise.u Ring_buffer.t;
    mutable seq : int;
    mutable next_seq_to_process : int;
    lock : Eio.Mutex.t;
  }

  let create ?(size = 100) () =
    {
      promises = Ring_buffer.create size (Obj.magic ());
      seq = 1;
      next_seq_to_process = 1;
      lock = Eio.Mutex.create ();
    }

  let add ~promise t =
    Eio.Mutex.use_rw ~protect:false t.lock (fun () ->
        Ring_buffer.push_back t.promises promise;
        let seq = t.seq in
        t.seq <- t.seq + 1;
        seq)

  let respond ?buf t ~seq:seq_low =
    let rec pop_earlier () =
      let promise = Ring_buffer.pop_front t.promises in
      if t.next_seq_to_process land 0xFFFF <> seq_low then (
        Log.debug (fun m ->
            m "cookie_map: no reply received" ~tags:(seq_ t.next_seq_to_process));
        Eio.Promise.resolve promise None;
        t.next_seq_to_process <- t.next_seq_to_process + 1;
        pop_earlier ())
      else (
        Log.debug (fun m ->
            m "cookie_map: sending reply" ~tags:(seq_ t.next_seq_to_process));
        Eio.Promise.resolve promise buf;
        t.next_seq_to_process <- t.next_seq_to_process + 1)
    in
    Eio.Mutex.use_rw ~protect:false t.lock pop_earlier
end

module Xid_seed = struct
  type t = {
    mutable last : int32;
    lock : Eio.Mutex.t;
    inc : int32;
    base : int32;
    max : int32;
  }

  let make ~base ~mask =
    let inc = Int32.(logand mask (neg mask)) in
    let max = Int32.(add (sub mask inc) 1l) in
    { last = 0l; lock = Eio.Mutex.create (); inc; base; max }

  let generate seed =
    Eio.Mutex.use_rw seed.lock (fun () ->
        if seed.last > 0l && seed.last >= seed.max then
          (* TODO: use xmisc extension to look for unused xids when they run out
             https://gitlab.freedesktop.org/xorg/lib/libxcb/-/blob/master/src/xcb_xid.c *)
          failwith "No more available resource identifiers"
        else (
          seed.last <- Int32.add seed.last seed.inc;
          let xid = Int32.logor seed.last seed.base in
          Xobl_protocol.X11_types.Xid.of_int (Int32.to_int xid)))
end

module Connection = struct
  type t = {
    socket : Eio.Buf_write.t;
    xid_seed : Xid_seed.t;
    cookie_reactor : Cookie_reactor.t;
    event_stream : Bytes.t Eio.Stream.t;
    display_info : Xobl_protocol.Xproto.setup;
    screen : int;
  }

  let screen conn = List.nth conn.display_info.roots conn.screen

  let write ?decode conn buf =
    let p, pu = Eio.Promise.create () in
    let seq = Cookie_reactor.add conn.cookie_reactor ~promise:pu in
    Eio.Buf_write.bytes conn.socket buf;
    let cookie = Cookie.create ?decode seq p in
    cookie
end
