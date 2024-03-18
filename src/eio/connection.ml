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
