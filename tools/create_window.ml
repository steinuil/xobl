open Xobl

let ( let* ) = Lwt.bind

let rec read_loop (conn : Connection.connection) =
  let* buf = Connection.read conn in
  match buf with
  | Some buf ->
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
  let wid = Connection.Xid_seed.generate conn.xid_seed in
  let root = List.hd conn.display_info.roots in

  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_create_window ~depth:root.root_depth ~wid:(Int32.to_int wid)
      ~parent:root.root ~x:10 ~y:10 ~width:300 ~height:300 ~border_width:10
      ~class_:`Input_output ~visual:root.root_visual ~at:0 buf
    |> Option.get
  in
  let buf = Bytes.sub buf 0 len in
  let* () =
    Lwt_io.printf "CreateWindow(len=%d): %s\n" len (Xobl.Codec.hex buf)
  in
  let* _ = Connection.write conn buf in

  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_map_window ~window:(Int32.to_int wid) buf ~at:0 |> Option.get
  in
  let buf = Bytes.sub buf 0 len in
  let* () = Lwt_io.printf "MapWindow(len=%d): %s\n" len (Xobl.Codec.hex buf) in
  let* _ = Connection.write conn buf in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let* conn = connect () in
     let* _ = Lwt.both (main conn) (read_loop conn) in
     Lwt.return_unit)
