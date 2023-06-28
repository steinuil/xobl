open Xobl

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
  let Display_name.{ hostname; display; screen } =
    Option.bind (Sys.getenv_opt "DISPLAY") Display_name.parse
    |> Option.value ~default:Display_name.default
  in
  Connection.open_display ~hostname ~display ~screen ()

let main (conn : Connection.connection) =
  let queued_events = ref [] in

  let reqs ?(size = 120) es =
    let buf = Bytes.make size '\x00' in
    let len =
      ListLabels.fold_left es ~init:0 ~f:(fun at encode ->
          encode buf ~at |> Option.get)
    in
    let buf = Bytes.sub buf 0 len in
    let* _ = Connection.write conn buf in
    let* () = Connection.check_for_error conn in
    let rec loop () =
      let* resp = Connection.read conn in
      match resp with
      | Some (`Reply _) -> Lwt.return_unit
      | Some (`Event _buf) ->
          (* TODO maybe we should debounce events based on their type *)
          (* queued_events := !queued_events @ [ buf ]; *)
          loop ()
      | _ -> failwith "a"
    in
    loop ()
  in

  let root = Connection.screen conn in

  let wid = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in

  let white = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in
  let black = Connection.Xid_seed.generate conn.xid_seed |> Int32.to_int in

  let width = ref 300 in
  let height = ref 200 in

  let* () =
    reqs
      [
        (fun buf ~at ->
          Xproto.encode_create_window ~depth:root.root_depth ~wid
            ~parent:root.root ~x:0 ~y:0 ~width:!width ~height:!height
            ~border_width:0 ~class_:`Input_output ~visual:root.root_visual
            ~event_mask:(Some [ `Exposure; `Structure_notify ])
            ~background_pixel:root.white_pixel buf ~at);
        (fun buf ~at ->
          Xproto.encode_change_window_attributes ~window:root.root
            ~event_mask:(Some [ `Pointer_motion ]) buf ~at);
        (fun buf ~at ->
          Xproto.encode_create_gc ~cid:white ~drawable:root.root
            ~foreground:root.white_pixel ~graphics_exposures:0 ~at buf);
        (fun buf ~at ->
          Xproto.encode_create_gc ~cid:black ~drawable:root.root
            ~foreground:root.black_pixel ~graphics_exposures:0 ~at buf);
        (fun buf ~at -> Xproto.encode_map_window ~window:wid buf ~at);
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
          (fun buf ~at ->
            Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black ~at buf
              ~arcs:
                [
                  circle ~x:0 ~y:0 ~width:w ~height:h;
                  circle ~x:w ~y:0 ~width:w ~height:h;
                ]);
          (fun buf ~at ->
            Xproto.encode_poly_fill_arc ~drawable:wid ~gc:white ~at buf
              ~arcs:
                (let x = Float.to_int unit_x in
                 let y = Float.to_int unit_y in
                 [
                   circle ~x ~y ~width:(w - (x * 2)) ~height:(h - (y * 2));
                   circle ~x:(w + x) ~y
                     ~width:(w - (x * 2))
                     ~height:(h - (y * 2));
                 ]));
          (fun buf ~at ->
            Xproto.encode_poly_fill_arc ~drawable:wid ~gc:black ~at buf
              ~arcs:[ left; right ]);
        ]
    in

    Lwt.return_unit
  in

  let mouse = ref ({ x = 0; y = 0 } : coords) in
  let win_pos = ref ({ x = 0; y = 0 } : coords) in

  let handle_event buf =
    match Char.code (Bytes.get buf 0) land lnot 0x80 with
    | 0x0C ->
        let unit_x = Float.of_int !width /. 20. in
        let unit_y = Float.of_int !height /. 10. in

        let* () = draw_eyes !mouse ~unit_x ~unit_y in
        Lwt.return_unit
    | 0x06 ->
        let x = Bytes.get_int16_le buf 24 in
        let y = Bytes.get_int16_le buf 26 in
        mouse := { x = x - !win_pos.x; y = y - !win_pos.y };
        let unit_x = Float.of_int !width /. 20. in
        let unit_y = Float.of_int !height /. 10. in

        let* () = draw_eyes !mouse ~unit_x ~unit_y in
        Lwt.return_unit
    | 0x16 ->
        let x = Bytes.get_int16_le buf 16 in
        let y = Bytes.get_int16_le buf 18 in
        let w = Bytes.get_int16_le buf 20 in
        let h = Bytes.get_int16_le buf 22 in
        width := w;
        height := h;
        win_pos := { x; y };
        Lwt.return_unit
    | _ ->
        let* () = Lwt_io.printf "Event: %s\n" (Xobl.Codec.hex buf) in
        Lwt.return_unit
  in

  let rec loop () =
    match !queued_events with
    | buf :: rest ->
        queued_events := rest;
        let* () = handle_event buf in
        loop ()
    | [] -> (
        let* resp = Connection.read conn in
        match resp with
        | Some (`Event buf) ->
            let* () = handle_event buf in
            loop ()
        | Some (`Error buf) ->
            let* _ = Lwt_io.printf "Error: %s\n" (Xobl.Codec.hex buf) in
            loop ()
        | Some (`Reply buf) ->
            let* _ = Lwt_io.printf "Reply: %s\n" (Xobl.Codec.hex buf) in
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
