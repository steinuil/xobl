open Xobl
open Xobl_protocol
open Xobl_lwt.Reactor

let ( let* ) = Lwt.bind
let ( >>= ) = Lwt.( >>= )

let connect () =
  Display_name.from_env ()
  |> Option.value ~default:Display_name.default
  |> Connection.open_display

let encode f =
  let buf = Codec.Encode_buffer.of_buffer (Buffer.create 120) in
  f buf;
  Buffer.to_bytes buf.buffer

let req ?decode conn req =
  let req = encode req in
  Connection.write ?decode conn req

let get_atom ~(conn : Connection.t) name ~only_if_exists =
  let* resp =
    Xproto.encode_intern_atom ~name ~only_if_exists
    |> req ~decode:(Xproto.decode_intern_atom_reply 0) conn
    >>= Cookie.wait
  in
  let atom =
    match resp with
    | Ok (Custom n) -> X11_types.Xid.of_int n
    | _ -> failwith "a"
  in
  let* resp =
    Xproto.encode_get_property ~delete:false
      ~window:(Connection.screen conn).root ~property:atom
      ~type_:(X11_types.E `Any) ~long_offset:0
      ~long_length:(50000 * 3 / 4)
    |> req ~decode:(Xproto.decode_get_property_reply 0) conn
    >>= Cookie.wait
  in
  let resp = Result.get_ok resp in

  let* () =
    Lwt_io.printf "\n%s\n"
      (Sexplib.Sexp.to_string_hum (Xproto.sexp_of_get_property_reply resp))
  in
  Lwt.return_unit

let rec read_loop conn =
  let* closed = Connection.read conn in
  if closed then Lwt.return_unit else read_loop conn

let main () =
  let* conn = connect () in
  let* _ =
    Lwt.both
      (Lwt.catch
         (fun () -> get_atom ~conn Sys.argv.(1) ~only_if_exists:false)
         (fun exn ->
           print_endline (Printexc.to_string exn);
           Lwt.return_unit))
      (read_loop conn)
  in
  Lwt.return_unit

let () = Lwt_main.run (main ())
