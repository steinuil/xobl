open Xobl
open Xobl_protocol
open Xobl_lwt
open X11_types

let ( let* ) = Lwt.bind

type coords = { x : int; y : int }
type v2 = { x : float; y : float }
type size = { w : float; h : float }

let circle ~x ~y ~width ~height =
  { Xproto.x; y; width; height; angle1 = 0; angle2 = 360 * 64 }

let square_distance v1 v2 =
  let dx = v1.x -. v2.x in
  let dy = v1.y -. v2.y in
  (dx *. dx) +. (dy *. dy)

let eye_position ~sclera ~pupil ~center ~mouse =
  (* Coordinates of the mouse relative to the eye of the tiger *)
  let rel_x = mouse.x -. center.x in
  let rel_y = mouse.y -. center.y in

  (* Angle of the mouse position from the center of the eye in radians *)
  let angle = atan2 (rel_y *. pupil.w) (rel_x *. pupil.h) in

  let cx = pupil.w *. cos angle in
  let cy = pupil.h *. sin angle in

  let x = center.x +. cx -. (sclera.w /. 2.) in
  let y = center.y +. cy -. (sclera.h /. 2.) in
  let pos = { x; y } in

  let mouse_x = mouse.x -. (sclera.w /. 2.) in
  let mouse_y = mouse.y -. (sclera.h /. 2.) in
  let mouse = { x = mouse_x; y = mouse_y } in

  let { x; y } =
    if square_distance center mouse < square_distance center pos then mouse
    else pos
  in

  circle ~x:(Float.to_int x) ~y:(Float.to_int y) ~width:(Float.to_int sclera.w)
    ~height:(Float.to_int sclera.h)

let connect () =
  Display_name.from_env ()
  |> Option.value ~default:Display_name.default
  |> Connection.open_display

let main (conn : Connection.t) =
  let queued_events = ref [] in

  let reqs ?(size = 120) es =
    let buf = Buffer.create size in
    List.iter (fun encode -> encode (Codec.Encode_buffer.of_buffer buf)) es;
    let buf = Buffer.to_bytes buf in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let rec loop () =
      let* resp = Connection.read conn in
      match resp with
      | Some (`Reply _) -> Lwt.return_unit
      | Some (`Event _buf) ->
          (* TODO maybe we should debounce events based on their type *)
          queued_events := !queued_events @ [ buf ];
          loop ()
      | _ -> failwith "a"
    in
    loop ()
  in

  let root = Connection.screen conn in

  let wid =
    Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int |> Xid.of_int
  in

  let white =
    Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int |> Xid.of_int
  in
  let black =
    Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int |> Xid.of_int
  in

  let width = ref 300 in
  let height = ref 200 in

  let* () =
    reqs
      [
        Xproto.encode_create_window ~depth:root.root_depth ~wid
          ~parent:root.root ~x:0 ~y:0 ~width:!width ~height:!height
          ~border_width:0 ~class_:`Input_output ~visual:root.root_visual
          ~event_mask:[ `Exposure; `Structure_notify; `Key_press ]
          ~background_pixel:root.white_pixel;
        Xproto.encode_change_window_attributes ~window:root.root
          ~event_mask:[ `Pointer_motion ];
        Xproto.encode_create_gc ~cid:white ~drawable:root.root
          ~foreground:root.white_pixel ~graphics_exposures:0;
        Xproto.encode_create_gc ~cid:black ~drawable:root.root
          ~foreground:root.black_pixel ~graphics_exposures:0;
        Xproto.encode_map_window ~window:wid;
      ]
  in

  let draw_eyes (mouse : coords) ~unit_x ~unit_y =
    let pupil = { w = unit_x *. 2.5; h = unit_y *. 2.5 } in
    let sclera = { w = unit_x *. 2.; h = unit_y *. 2. } in
    let mouse = { x = Float.of_int mouse.x; y = Float.of_int mouse.y } in
    let center_l = { x = unit_x *. 5.; y = unit_y *. 5. } in

    let left = eye_position ~sclera ~pupil ~center:center_l ~mouse in
    let right =
      eye_position ~sclera ~pupil
        ~center:{ center_l with x = center_l.x *. 3. }
        ~mouse
    in

    let w = Float.to_int (unit_x *. 10.) in
    let h = Float.to_int (unit_y *. 10.) in

    let* () =
      reqs
        [
          Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black
            ~arcs:
              [
                circle ~x:0 ~y:0 ~width:w ~height:h;
                circle ~x:w ~y:0 ~width:w ~height:h;
              ];
          Xproto.encode_poly_fill_arc ~drawable:wid ~gc:white
            ~arcs:
              (let x = Float.to_int unit_x in
               let y = Float.to_int unit_y in
               [
                 circle ~x ~y ~width:(w - (x * 2)) ~height:(h - (y * 2));
                 circle ~x:(w + x) ~y ~width:(w - (x * 2)) ~height:(h - (y * 2));
               ]);
          Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black
            ~arcs:[ left; right ];
        ]
    in

    Lwt.return_unit
  in

  let mouse = ref ({ x = 0; y = 0 } : coords) in
  let win_pos = ref ({ x = 0; y = 0 } : coords) in

  let handle_event buf =
    match Char.code (Bytes.get buf 0) land lnot 0x80 with
    | 0x02 ->
        let ev, _ = Xproto.decode_key_press_event buf ~at:0 |> Option.get in
        let* () =
          Lwt_io.printf "KeyPress: %s\n"
            (Sexplib.Sexp.to_string_hum (Xproto.sexp_of_key_press_event ev))
        in
        Lwt.return_unit
    | 0x0C ->
        let ev, _ = Xproto.decode_expose_event buf ~at:0 |> Option.get in
        let* () =
          Lwt_io.printf "Expose: %s\n"
            (Sexplib.Sexp.to_string_hum (Xproto.sexp_of_expose_event ev))
        in
        width := ev.width;
        height := ev.height;
        let unit_x = Float.of_int !width /. 20. in
        let unit_y = Float.of_int !height /. 10. in

        let* () = draw_eyes !mouse ~unit_x ~unit_y in
        Lwt.return_unit
    | 0x06 ->
        let ev, _ = Xproto.decode_motion_notify_event buf ~at:0 |> Option.get in
        let* () =
          Lwt_io.printf "MotionNotify: %s\n"
            (Sexplib.Sexp.to_string_hum (Xproto.sexp_of_motion_notify_event ev))
        in
        let x = ev.event_x in
        let y = ev.event_y in

        mouse := { x = x - !win_pos.x; y = y - !win_pos.y };
        let unit_x = Float.of_int !width /. 20. in
        let unit_y = Float.of_int !height /. 10. in

        let* () = draw_eyes !mouse ~unit_x ~unit_y in
        Lwt.return_unit
    | 0x16 ->
        let ev, _ =
          Xproto.decode_configure_notify_event buf ~at:0 |> Option.get
        in
        let* () =
          Lwt_io.printf "ConfigureNotify: %s\n"
            (Sexplib.Sexp.to_string_hum
               (Xproto.sexp_of_configure_notify_event ev))
        in
        let x = ev.x in
        let y = ev.y in
        let w = ev.width in
        let h = ev.height in
        width := w;
        height := h;
        win_pos := { x; y };
        Lwt.return_unit
    | 0x13 ->
        let ev, _ = Xproto.decode_map_notify_event buf ~at:0 |> Option.get in
        let* () =
          Lwt_io.printf "MapNotify: %s\n"
            (Sexplib.Sexp.to_string_hum (Xproto.sexp_of_map_notify_event ev))
        in
        Lwt.return_unit
    | _ ->
        let* () = Lwt_io.printf "Event: %s\n" (Util.hex_string_of_bytes buf) in
        Lwt.return_unit
  in

  let rec loop () =
    match !queued_events with
    | buf :: rest ->
        queued_events := rest;
        let* () = handle_event buf in
        let* () = Lwt.pause () in
        loop ()
    | [] -> (
        let* resp = Connection.read conn in
        match resp with
        | Some (`Event buf) ->
            let* () = handle_event buf in
            loop ()
        | Some (`Error buf) ->
            let* _ =
              Lwt_io.printf "Error: %s\n" (Util.hex_string_of_bytes buf)
            in
            loop ()
        | Some (`Reply buf) ->
            let* _ =
              Lwt_io.printf "Reply: %s\n" (Util.hex_string_of_bytes buf)
            in
            loop ()
        | None -> Lwt.return_unit)
  in

  let* () = loop () in

  Lwt.return_unit

let () =
  Lwt_main.run
    (let* conn = connect () in
     let* _ = main conn in
     Lwt.return_unit)
