open Xobl

let ( let* ) = Lwt.bind

let rec read_loop (conn : Connection.connection) =
  let* buf = Connection.read conn in
  match buf with
  | Some (`Error buf) ->
      let* _ =
        Lwt_io.printf "Response: %s\n" (Xobl.Util.hex_string_of_bytes buf)
      in
      read_loop conn
  | Some (`Event buf) ->
      let* _ =
        Lwt_io.printf "Response: %s\n" (Xobl.Util.hex_string_of_bytes buf)
      in
      read_loop conn
  | Some (`Reply buf) ->
      let* _ =
        Lwt_io.printf "Response: %s\n" (Xobl.Util.hex_string_of_bytes buf)
      in
      read_loop conn
  | None -> Lwt.return_unit

let connect () =
  let display =
    Option.bind (Sys.getenv_opt "DISPLAY") Display_name.parse
    |> Option.value ~default:Display_name.default
  in
  let Display_name.{ hostname; display; _ } = display in
  Connection.open_display ~hostname ~display ()

let main (conn : Connection.connection) =
  (*
  let wid = Connection.Xid_seed.generate conn.xid_seed in
*)
  let root = List.hd conn.display_info.roots in

  let buf = Codec.Encode_buffer.of_buffer (Buffer.create 120) in
  Xproto.encode_change_window_attributes ~window:root.root
    ~event_mask:(Some [ `Substructure_redirect ]) buf;
  let buf = Buffer.to_bytes buf.buffer in
  let* () =
    Lwt_io.printf "ChangeWindowAttributes(len=%d): %s\n" (Bytes.length buf)
      (Xobl.Util.hex_string_of_bytes buf)
  in
  let* _ = Connection.write conn buf in

  let buf = Codec.Encode_buffer.of_buffer (Buffer.create 4) in
  Xproto.encode_get_input_focus buf;
  let buf = Buffer.to_bytes buf.buffer in
  let* () =
    Lwt_io.printf "MapWindow(len=%d): %s\n" (Bytes.length buf)
      (Xobl.Util.hex_string_of_bytes buf)
  in
  let* _ = Connection.write conn buf in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let* conn = connect () in
     let* _ = Lwt.both (main conn) (read_loop conn) in
     Lwt.return_unit)
