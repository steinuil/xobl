let ( let& ) = Option.bind
let ( let* ) = Lwt.bind

let get_socket_params ~display = function
  | Display_name.Unix_domain_socket path ->
      let* localhost = Lwt_unix.gethostname () in
      let auth =
        let& xauth_path = Xauth.get_path () in
        try
          Xauth.entries_from_file xauth_path
          |> Xauth.get_best ~family:Xauth.Family.Local ~address:localhost
               ~display
        with Sys_error _ -> None
      in
      let auth = Option.value ~default:("", "") auth in
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
      Lwt.return (ai_family, ai_addr, ("", ""))

type setup_response =
  | Success of Xproto.setup
  | Failed of Xproto.setup_failed
  | Authenticate of Xproto.setup_authenticate

let read_handshake_response sock =
  let buf = Bytes.create 8 in
  let* _ = Lwt_unix.read sock buf 0 8 in
  let additional_data_length = Bytes.get_uint16_le buf 6 in
  let whole_buf = Bytes.create (8 + (additional_data_length * 4)) in
  Bytes.blit buf 0 whole_buf 0 8;
  let* _ = Lwt_unix.read sock whole_buf 8 (additional_data_length * 4) in
  match Bytes.get whole_buf 0 with
  | '\x01' ->
      let display_info, _ = Xproto.decode_setup whole_buf ~at:0 |> Option.get in
      Lwt.return (Success display_info)
  | '\x00' ->
      let failed, _ =
        Xproto.decode_setup_failed whole_buf ~at:0 |> Option.get
      in
      Lwt.return (Failed failed)
  | '\x02' ->
      let authenticate, _ =
        Xproto.decode_setup_authenticate whole_buf ~at:0 |> Option.get
      in
      Lwt.return (Authenticate authenticate)
  | _ -> failwith "invalid setup response received"

let read_response_from sock =
  let buf = Bytes.create 32 in
  let* len = Lwt_unix.read sock buf 0 32 in
  if len = 0 then Lwt.return None
  else
    let buf = Bytes.sub buf 0 len in
    match Bytes.get buf 0 with
    | '\x00' (* error *) -> Lwt.return (Some (`Error buf))
    | '\x01' (* reply *) ->
        let additional_data_length = Bytes.get_int32_le buf 4 |> Int32.to_int in
        if additional_data_length < 1 then Lwt.return (Some (`Reply buf))
        else
          let whole_buf = Bytes.create (32 + (additional_data_length * 4)) in
          Bytes.blit buf 0 whole_buf 0 8;
          let* _ =
            Lwt_unix.read sock whole_buf 32 (additional_data_length * 4)
          in
          Lwt.return (Some (`Reply whole_buf))
    | _ (* event *) -> Lwt.return (Some (`Event buf))

let pad n = (if n = 0 then 0 else ((n - 1) lsr 2) + 1) * 4

module Xid_seed = struct
  type seed = { mutable last : int32; inc : int32; base : int32; max : int32 }

  let make ~base ~mask =
    let inc = Int32.(logand mask (neg mask)) in
    let max = Int32.(add (sub mask inc) 1l) in
    { last = 0l; inc; base; max }

  (* TODO: use xmisc extension to look for unused xids when they run out
     https://gitlab.freedesktop.org/xorg/lib/libxcb/-/blob/master/src/xcb_xid.c *)
  let generate seed =
    if seed.last > 0l && seed.last >= seed.max then
      failwith "No more available resource identifiers"
    else (
      seed.last <- Int32.add seed.last seed.inc;
      let xid = Int32.logor seed.last seed.base in
      xid)
end

type connection = {
  socket : Lwt_unix.file_descr;
  display_info : Xproto.setup;
  xid_seed : Xid_seed.seed;
  mutable sequence_number : int;
}

let open_display ~hostname ?display () =
  let* domain, address, (xauth_name, xauth_data) =
    get_socket_params hostname ~display
  in
  let socket = Lwt_unix.socket domain Unix.SOCK_STREAM 0 in
  let* () = Lwt_unix.connect socket address in
  let len =
    12 + pad (String.length xauth_name) + pad (String.length xauth_data)
  in
  let handshake = Bytes.make len '\x00' in
  let _ =
    Xproto.encode_setup_request handshake ~at:0
      {
        byte_order = 0x6C;
        protocol_major_version = 11;
        protocol_minor_version = 0;
        authorization_protocol_name = xauth_name;
        authorization_protocol_data = xauth_data;
      }
    |> Option.get
  in
  let* _ = Lwt_unix.write socket handshake 0 len in
  let* resp = read_handshake_response socket in
  match resp with
  | Success display_info ->
      let xid_seed =
        Xid_seed.make
          ~base:(Int32.of_int display_info.resource_id_base)
          ~mask:(Int32.of_int display_info.resource_id_mask)
      in
      Lwt.return { socket; display_info; xid_seed; sequence_number = 1 }
  | _ -> failwith "connection failure"

let read conn =
  let* () = Lwt_unix.wait_read conn.socket in
  let* buf = read_response_from conn.socket in
  Lwt.return buf

let write conn ?(offset = 0) ?length buf =
  let length = Option.value ~default:(Bytes.length buf) length in
  let* _ = Lwt_unix.write conn.socket buf offset length in
  let seq = conn.sequence_number in
  conn.sequence_number <- seq + 1;
  Lwt.return seq

(** Send after a request that does not have a reply to check whether it succeeded. *)
let check_for_error conn =
  let buf = Bytes.make 4 '\x00' in
  let _ = Xproto.encode_get_input_focus buf ~at:0 |> Option.get in
  let* _ = write conn buf in
  (* TODO read response *)
  Lwt.return_unit
