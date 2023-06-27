open Xobl

let ( let* ) = Lwt.bind

let rec read_loop (conn : Connection.connection) =
  let* buf = Connection.read conn in
  match buf with
  | Some (`Error buf) ->
      let* _ = Lwt_io.printf "Response: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | Some (`Event buf) ->
      let* _ = Lwt_io.printf "Response: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | Some (`Reply buf) ->
      let* _ = Lwt_io.printf "Response: %s\n" (Xobl.Codec.hex buf) in
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

  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_change_window_attributes ~window:root.root
      ~event_mask:(Some [ `Substructure_redirect ]) buf ~at:0
    |> Option.get
  in
  let buf = Bytes.sub buf 0 len in
  let* () =
    Lwt_io.printf "ChangeWindowAttributes(len=%d): %s\n" len
      (Xobl.Codec.hex buf)
  in
  let* _ = Connection.write conn buf in

  let buf = Bytes.make 4 '\x00' in
  let len = Xproto.encode_get_input_focus buf ~at:0 |> Option.get in
  let* () = Lwt_io.printf "MapWindow(len=%d): %s\n" len (Xobl.Codec.hex buf) in
  let* _ = Connection.write conn buf in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let* conn = connect () in
     let* _ = Lwt.both (main conn) (read_loop conn) in
     Lwt.return_unit)
