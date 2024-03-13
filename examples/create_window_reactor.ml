open Xobl
open Xobl_protocol
open Xobl_lwt.Reactor

let ( let* ) = Lwt.bind
let ( >>= ) = Lwt.( >>= )
let ( >|= ) = Lwt.( >|= )

let encode f =
  let buf = Codec.Encode_buffer.of_buffer (Buffer.create 120) in
  f buf;
  Buffer.to_bytes buf.buffer

let req ?decode conn req =
  let req = encode req in
  Connection.write ?decode conn req

let create_window (conn : Connection.t) =
  let screen = Connection.screen conn in

  let* window_id = Xid_seed.generate conn.xid_seed in

  let* create_window_cookie =
    Xproto.encode_create_window ~depth:screen.root_depth ~wid:window_id
      ~parent:screen.root ~x:0 ~y:0 ~width:300 ~height:300 ~border_width:10
      ~class_:`Input_output ~visual:screen.root_visual
      ~event_mask:[ `Enter_window ] ~background_pixel:screen.white_pixel
    |> req conn
  in

  let* map_window_cookie =
    Xproto.encode_map_window ~window:window_id |> req conn
  in

  (* TODO Needed for now, remove when this is done automatically in reactor *)
  let* _ =
    Xproto.encode_get_input_focus
    |> req ~decode:(Xproto.decode_get_input_focus_reply 0) conn
    >>= Cookie.wait
  in

  let* () = Cookie.wait create_window_cookie >|= Result.get_ok in
  let* () = Cookie.wait map_window_cookie >|= Result.get_ok in

  print_endline "No errors occurred while creating window";

  Lwt.return_unit

let rec read_loop conn =
  let* closed = Connection.read conn in
  if closed then Lwt.return_unit
  else
    let* () = Lwt.pause () in
    read_loop conn

let main () =
  let* conn =
    Display_name.from_env ()
    |> Option.value ~default:Display_name.default
    |> Connection.open_display
  in
  let* () =
    Lwt.catch
      (fun () ->
        let* _ = Lwt.both (create_window conn) (read_loop conn) in
        Lwt.return_unit)
      (fun exn ->
        print_endline (Printexc.to_string exn);
        Lwt.return_unit)
  in
  Lwt.return_unit

let () = Lwt_main.run (main ())
