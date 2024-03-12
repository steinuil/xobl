let ( let* ) = Lwt.bind

module Cookie = struct
  type 'a decoder = Bytes.t -> at:int -> ('a * int) option

  type 'a t = {
    sequence_number : int;
    mvar : Bytes.t Lwt_mvar.t;
    decode : 'a decoder option;
  }

  let create ?decode sequence_number mvar = { sequence_number; mvar; decode }

  (* TODO we should detect that we're waiting on a request that has no
     reply and either send a get_input_focus request or wait
     for a reply from a request with a higher seq saved in Cookie_map.
     Maybe it'd be better to move this to a wait_cookie function in
     Connection for this purpose. *)
  let wait cookie =
    let* buf = Lwt_mvar.take cookie.mvar in
    match (Bytes.get buf 0, cookie.decode) with
    | '\x01', Some decode ->
        (* TODO return a more meaningful exception to the decode error *)
        let reply, _ = decode buf ~at:0 |> Option.get in
        Lwt.return (Ok reply)
    | '\x01', None ->
        (* TODO return a more meaningful exception *)
        failwith "no reply expected"
    | '\x00', _ ->
        (* TODO decode error here *)
        Lwt.return (Error buf)
    | _, _ ->
        (* TODO return a more meaningful exception *)
        failwith "sent an event to an mvar"
end

module Cookie_map = struct
  type table = (int, Bytes.t Lwt_mvar.t) Hashtbl.t
  type t = { table : table; lock : Lwt_mutex.t }

  let create ?(size = 100) () =
    { table = Hashtbl.create size; lock = Lwt_mutex.create () }

  let add ~sequence_number ~mvar t =
    let sequence_number = sequence_number land 0xFF in
    Lwt_mutex.with_lock t.lock (fun () ->
        Hashtbl.add t.table sequence_number mvar;
        Lwt.return_unit)

  (* TODO requests that don't have a reply should still be activated
     even if we don't receive anything. We must do that by taking all
     of the sequence numbers lower than the one we received and
     put an empty buf in the associated mvar. *)
  let pop ~sequence_number t =
    let sequence_number = sequence_number land 0xFF in
    Lwt_mutex.with_lock t.lock (fun () ->
        match Hashtbl.find_opt t.table sequence_number with
        | Some mvar ->
            Hashtbl.remove t.table sequence_number;
            Lwt.return_some mvar
        | None -> Lwt.return_none)
end

module Xid_seed = struct
  type t = {
    mutable last : int32;
    lock : Lwt_mutex.t;
    inc : int32;
    base : int32;
    max : int32;
  }

  let make ~base ~mask =
    let inc = Int32.(logand mask (neg mask)) in
    let max = Int32.(add (sub mask inc) 1l) in
    { last = 0l; lock = Lwt_mutex.create (); inc; base; max }

  let generate seed =
    Lwt_mutex.with_lock seed.lock (fun () ->
        if seed.last > 0l && seed.last >= seed.max then
          (* TODO: use xmisc extension to look for unused xids when they run out
             https://gitlab.freedesktop.org/xorg/lib/libxcb/-/blob/master/src/xcb_xid.c *)
          failwith "No more available resource identifiers"
        else (
          seed.last <- Int32.add seed.last seed.inc;
          let xid = Int32.logor seed.last seed.base in
          Lwt.return (Xobl_protocol.X11_types.Xid.of_int (Int32.to_int xid))))
end

module Sequence_number = struct
  type t = { mutable seq : int; lock : Lwt_mutex.t }

  let create () = { seq = 1; lock = Lwt_mutex.create () }

  let next t =
    Lwt_mutex.with_lock t.lock (fun () ->
        let seq = t.seq in
        t.seq <- seq + 1;
        Lwt.return seq)
end

module Connection = struct
  type t = {
    socket : Lwt_unix.file_descr;
    xid_seed : Xid_seed.t;
    sequence_number : Sequence_number.t;
    cookie_map : Cookie_map.t;
    event_stream : Bytes.t Lwt_stream.t;
    push_event_stream : Bytes.t option -> unit;
    display_info : Xobl_protocol.Xproto.setup;
    screen : int;
  }

  let screen conn = List.nth conn.display_info.roots conn.screen

  let write ?decode conn buf =
    let* sequence_number = Sequence_number.next conn.sequence_number in
    let mvar = Lwt_mvar.create_empty () in
    let* () = Cookie_map.add conn.cookie_map ~mvar ~sequence_number in
    let length = Bytes.length buf in
    (* TODO use a function that ensures all of the bytes have been written *)
    let* _ = Lwt_unix.write conn.socket buf 0 length in
    let cookie = Cookie.create ?decode sequence_number mvar in
    Lwt.return cookie

  let read_packet sock =
    let* () = Lwt_unix.wait_read sock in
    let buf = Bytes.create 32 in
    (* TODO use a function that ensures all of the bytes have been filled *)
    let* len = Lwt_unix.read sock buf 0 32 in
    if len = 0 then Lwt.return None
    else
      match Bytes.get buf 0 with
      | '\x00' (* error *) -> Lwt.return (Some (`Error buf))
      | '\x01' (* reply *) ->
          let additional_data_length =
            Bytes.get_int32_le buf 4 |> Int32.to_int
          in
          if additional_data_length < 1 then Lwt.return (Some (`Reply buf))
          else
            let whole_buf = Bytes.create (32 + (additional_data_length * 4)) in
            Bytes.blit buf 0 whole_buf 0 32;
            (* TODO use a function that ensures all of the bytes have been filled *)
            let* _ =
              Lwt_unix.read sock whole_buf 32 (additional_data_length * 4)
            in
            Lwt.return (Some (`Reply whole_buf))
      | _ (* event *) ->
          (* TODO handle generic events *)
          Lwt.return (Some (`Event buf))

  (* TODO can we unify read_packet and read_handshake_response? *)
  (* TODO error handling *)
  let read_handshake_response sock =
    let buf = Bytes.create 8 in
    let* _ = Lwt_unix.read sock buf 0 8 in
    let additional_data_length = Bytes.get_uint16_le buf 6 in
    let whole_buf = Bytes.create (8 + (additional_data_length * 4)) in
    Bytes.blit buf 0 whole_buf 0 8;
    let* _ = Lwt_unix.read sock whole_buf 8 (additional_data_length * 4) in
    let open Xobl_protocol in
    match Bytes.get whole_buf 0 with
    | '\x01' ->
        let display_info, _ =
          Xproto.decode_setup whole_buf ~at:0 |> Option.get
        in
        Lwt.return (`Success display_info)
    | '\x00' ->
        let failed, _ =
          Xproto.decode_setup_failed whole_buf ~at:0 |> Option.get
        in
        Lwt.return (`Failed failed)
    | '\x02' ->
        let authenticate, _ =
          Xproto.decode_setup_authenticate whole_buf ~at:0 |> Option.get
        in
        Lwt.return (`Authenticate authenticate)
    | _ -> Lwt.return (`Invalid whole_buf)

  let read conn =
    let* packet = read_packet conn.socket in
    match packet with
    | None -> Lwt.return_true
    | Some (`Reply buf | `Error buf) -> (
        let sequence_number = Bytes.get_uint16_le buf 2 in
        let* mvar = Cookie_map.pop conn.cookie_map ~sequence_number in
        match mvar with
        | Some mvar ->
            let* () = Lwt_mvar.put mvar buf in
            Lwt.return_false
        | None ->
            (* TODO return a more meaningful exception *)
            failwith "Unhandled sequence number")
    | Some (`Event buf) ->
        conn.push_event_stream (Some buf);
        Lwt.return_false

  let wait_for_event conn = Lwt_stream.get conn.event_stream
  let ( let& ) = Option.bind

  let get_socket_params ~display =
    let open Xobl in
    function
    | Display_name.Unix_domain_socket path ->
        let* localhost = Lwt_unix.gethostname () in
        let auth =
          let& xauth_path = Xauth.path_from_env () in
          try
            In_channel.with_open_text xauth_path In_channel.input_all
            |> Xauth.parse
            |> Xauth.select_best ~family:Xauth.Family.Local ~address:localhost
                 ~display
          with Sys_error _ -> None
        in
        let auth = Option.value ~default:Xauth.default_auth auth in
        Lwt.return (Unix.PF_UNIX, Unix.ADDR_UNIX path, auth)
    | Display_name.Internet_domain (family, hostname, port) ->
        let family =
          match family with `Ipv4 -> Unix.PF_INET | `Ipv6 -> Unix.PF_INET6
        in
        let* addresses =
          Lwt_unix.getaddrinfo hostname (string_of_int port)
            [ Unix.AI_SOCKTYPE Unix.SOCK_STREAM; Unix.AI_FAMILY family ]
        in
        (* TODO: we should try to connect to all the results in order
           instead of just picking the first. *)
        let Unix.{ ai_family; ai_addr; _ } = List.hd addresses in
        Lwt.return
          (ai_family, ai_addr, { Xobl.Xauth.auth_name = ""; auth_data = "" })

  let byte_order = 0x6C
  let protocol_version = (11, 0)

  (* TODO make a version of this that takes a connected socket *)
  (* TODO better error handling *)
  let open_display Xobl.Display_name.{ hostname; display; screen } =
    let* domain, address, { auth_name; auth_data } =
      get_socket_params hostname ~display:(Some display)
    in
    let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
    let* () = Lwt_unix.connect socket address in
    let handshake =
      Xobl_protocol.Codec.Encode_buffer.of_buffer (Buffer.create 24)
    in
    let () =
      Xobl_protocol.Xproto.encode_setup_request handshake
        {
          byte_order;
          protocol_major_version = fst protocol_version;
          protocol_minor_version = snd protocol_version;
          authorization_protocol_name = auth_name;
          authorization_protocol_data = auth_data;
        }
    in
    let* _ =
      Lwt_unix.write socket
        (Buffer.to_bytes handshake.buffer)
        0
        (Xobl_protocol.Codec.Encode_buffer.current_offset handshake)
    in
    let* resp = read_handshake_response socket in
    match resp with
    | `Success display_info ->
        let xid_seed =
          Xid_seed.make
            ~base:(Int32.of_int display_info.resource_id_base)
            ~mask:(Int32.of_int display_info.resource_id_mask)
        in
        let sequence_number = Sequence_number.create () in
        let cookie_map = Cookie_map.create () in
        let event_stream, push_event_stream = Lwt_stream.create () in
        Lwt.return
          {
            socket;
            xid_seed;
            sequence_number;
            cookie_map;
            event_stream;
            push_event_stream;
            display_info;
            screen;
          }
    | _ -> failwith "connection failure"
end
