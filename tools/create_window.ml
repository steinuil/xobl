open Xobl

let ( let& ) = Lwt.bind

let rec read_loop (conn : Connection.connection) =
  let& () = Lwt_unix.wait_read conn.socket in
  let& buf = Connection.read_response conn.socket in
  match buf with
  | Some buf ->
      let& _ = Lwt_io.printf "Response: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | None -> Lwt.return_unit

let connect () =
  let hostname = Display_name.default in

  Connection.open_display ~hostname:hostname.hostname ~display:hostname.display
    ()

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
  let& () =
    Lwt_io.printf "CreateWindow(len=%d): %s\n" len (Xobl.Codec.hex buf)
  in
  let& _ = Lwt_unix.write conn.socket buf 0 len in

  (*
  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_intern_atom ~only_if_exists:true ~name:"_WM_NAME" buf ~at:0
  in
  let& _ = Lwt_unix.write conn.socket buf 0 len in

  let buf = Bytes.make 120 '\x00' in
  let title = "xobl" in
  let len =
    Xproto.encode_change_property ~mode:`Replace ~window:(Int32.to_int wid)
      ~property:wm_name
      ~type_:(Xproto.atom_int_of_enum `String)
      ~format:8 ~data_len:(String.length title) ~data:title buf ~at:0
  in
  *)

  (*
  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_change_property ~mode:`Replace ~window:(Int32.to_int wid)
      ~property:wm_name
      ~type_:string
  *)
  let buf = Bytes.make 120 '\x00' in
  let len =
    Xproto.encode_map_window ~window:(Int32.to_int wid) buf ~at:0 |> Option.get
  in
  let buf = Bytes.sub buf 0 len in
  let& () = Lwt_io.printf "MapWindow(len=%d): %s\n" len (Xobl.Codec.hex buf) in
  let& _ = Lwt_unix.write conn.socket buf 0 len in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let& conn = connect () in
     let& _ = Lwt.both (main conn) (read_loop conn) in
     Lwt.return_unit)
