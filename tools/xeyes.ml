open Xobl

let ( let* ) = Lwt.bind

let circle ~x ~y ~width ~height =
  { Xproto.x; y; width; height; angle1 = 0; angle2 = 360 * 64 }

let _draw_eyes ~win ~black ~white ~width ~height ~x ~y ~at buf =
  let at =
    Xproto.encode_poly_fill_arc ~drawable:win ~gc:black ~at buf
      ~arcs:
        [
          circle ~x ~y ~width:(width / 2) ~height;
          circle ~x:(x + (width / 2)) ~y ~width:(width / 2) ~height;
        ]
    |> Option.get
  in
  let at =
    Xproto.encode_poly_fill_arc ~drawable:win ~gc:white ~at buf
      ~arcs:
        [
          circle ~x:(x + 50) ~y:(y + 50)
            ~width:((width / 2) - 100)
            ~height:(height - 100);
          circle
            ~x:(x + (width / 2) + 50)
            ~y:(y + 50)
            ~width:((width / 2) - 100)
            ~height:(height - 100);
        ]
    |> Option.get
  in
  at

let connect () =
  let display =
    Option.bind (Sys.getenv_opt "DISPLAY") Display_name.parse
    |> Option.value ~default:Display_name.default
  in
  let Display_name.{ hostname; display; _ } = display in
  Connection.open_display ~hostname ~display ()

(*
let rec read_loop (conn : Connection.connection) =
  let* buf = Connection.read conn in
  match buf with
  | Some (`Error buf) ->
      let* _ = Lwt_io.printf "Error: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | Some (`Event buf) ->
      let* _ = Lwt_io.printf "Event: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | Some (`Reply buf) ->
      let* _ = Lwt_io.printf "Reply: %s\n" (Xobl.Codec.hex buf) in
      read_loop conn
  | None -> Lwt.return_unit
*)

let main (conn : Connection.connection) =
  let root = List.hd conn.display_info.roots in

  let width = 300 in
  let height = 200 in

  let wid = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in

  let* () =
    let buf = Bytes.make 120 '\x00' in
    let len =
      Xproto.encode_create_window ~depth:root.root_depth ~wid ~parent:root.root
        ~x:0 ~y:0 ~width ~height ~border_width:0 ~class_:`Input_output
        ~visual:root.root_visual
        ~event_mask:(Some [ `Exposure; `Pointer_motion ])
        ~background_pixel:root.white_pixel ~at:0 buf
      |> Option.get
    in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let* resp = Connection.read conn in
    match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
  in

  let white = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in
  let black = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in

  let* () =
    let buf = Bytes.make 120 '\x00' in
    let len =
      Xproto.encode_create_gc ~cid:white ~drawable:root.root
        ~foreground:root.white_pixel ~graphics_exposures:0 ~at:0 buf
      |> Option.get
    in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let* resp = Connection.read conn in
    match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
  in

  let* () =
    let buf = Bytes.make 120 '\x00' in
    let len =
      Xproto.encode_create_gc ~cid:black ~drawable:root.root
        ~foreground:root.black_pixel ~graphics_exposures:0 ~at:0 buf
      |> Option.get
    in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let* resp = Connection.read conn in
    match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
  in

  (*
  let pixmap = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in

  let* () =
    let buf = Bytes.make 120 '\x00' in
    let len =
      Xproto.encode_create_pixmap ~depth:root.root_depth ~pid:pixmap
        ~drawable:wid ~width ~height ~at:0 buf
      |> Option.get
    in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let* resp = Connection.read conn in
    match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
  in
  *)
  let* () =
    let buf = Bytes.make 120 '\x00' in
    let len = Xproto.encode_map_window ~window:wid buf ~at:0 |> Option.get in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let* resp = Connection.read conn in
    match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
  in

  let draw_eyes () =
    let* () =
      let buf = Bytes.make 120 '\x00' in
      let len =
        Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black ~at:0 buf
          ~arcs:
            [
              circle ~x:0 ~y:0 ~width:(width / 2) ~height;
              circle ~x:(width / 2) ~y:0 ~width:(width / 2) ~height;
            ]
        |> Option.get
      in
      let buf = Bytes.sub buf 0 len in
      let* _ = Connection.write conn buf in
      let* () = Connection.check_for_error conn in
      let* resp = Connection.read conn in
      match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
    in

    let* () =
      let buf = Bytes.make 120 '\x00' in
      let len =
        Xproto.encode_poly_fill_arc ~drawable:wid ~gc:white ~at:0 buf
          ~arcs:
            [
              circle ~x:10 ~y:10 ~width:((width / 2) - 20) ~height:(height - 20);
              circle
                ~x:((width / 2) + 10)
                ~y:10
                ~width:((width / 2) - 20)
                ~height:(height - 20);
            ]
        |> Option.get
      in
      let buf = Bytes.sub buf 0 len in
      let* _ = Connection.write conn buf in
      let* () = Connection.check_for_error conn in
      let* resp = Connection.read conn in
      match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
    in

    let pupil_size = 30 in

    let* () =
      let buf = Bytes.make 120 '\x00' in
      let len =
        Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black ~at:0 buf
          ~arcs:
            [
              circle
                ~x:((width / 4) - (pupil_size / 2))
                ~y:((height / 2) - (pupil_size / 2))
                ~width:pupil_size ~height:pupil_size;
              circle
                ~x:((width / 4 * 3) - (pupil_size / 2))
                ~y:((height / 2) - (pupil_size / 2))
                ~width:pupil_size ~height:pupil_size;
            ]
        |> Option.get
      in
      let buf = Bytes.sub buf 0 len in
      let* _ = Connection.write conn buf in
      let* () = Connection.check_for_error conn in
      let* resp = Connection.read conn in
      match resp with Some (`Reply _) -> Lwt.return_unit | _ -> failwith "a"
    in
    Lwt.return_unit
  in

  let rec loop () =
    let* resp = Connection.read conn in
    match resp with
    | Some (`Error buf) ->
        let* _ = Lwt_io.printf "Error: %s\n" (Xobl.Codec.hex buf) in
        loop ()
    | Some (`Event buf) when Bytes.get buf 0 = '\x0C' ->
        let* () = draw_eyes () in
        loop ()
    | Some (`Event buf) when Bytes.get buf 0 = '\x06' ->
        let x = Bytes.get_int16_le buf 24 |> Float.of_int in
        let y = Bytes.get_int16_le buf 26 |> Float.of_int in
        let* _ = Lwt_io.printf "Coords: %f,%f\n" x y in

        loop ()
    | Some (`Event buf) ->
        let* _ = Lwt_io.printf "Event: %s\n" (Xobl.Codec.hex buf) in
        loop ()
    | Some (`Reply buf) ->
        let* _ = Lwt_io.printf "Reply: %s\n" (Xobl.Codec.hex buf) in
        loop ()
    | None -> Lwt.return_unit
  in

  let* () = loop () in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let* conn = connect () in
     let* _ = main conn in
     Lwt.return_unit)
