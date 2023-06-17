let modules =
  [
    "bigreq";
    "composite";
    "damage";
    "dpms";
    "dri2";
    "dri3";
    "ge";
    "glx";
    "present";
    "randr";
    "record";
    "render";
    "res";
    "screensaver";
    "shape";
    "shm";
    "sync";
    "xc_misc";
    "xevie";
    "xf86dri";
    "xf86vidmode";
    "xfixes";
    "xinerama";
    "xinput";
    (* "xkb"; *)
    "xprint";
    "xproto";
    "xselinux";
    "xtest";
    "xv";
    "xvmc";
  ]

let parse_module m =
  let f = open_in (Printf.sprintf "../xml-xcb/%s.xml" m) in
  try
    let m = Xobl_compiler.Parser.parse (`Channel f) |> Result.get_ok in
    close_in f;
    m
  with exn ->
    close_in f;
    raise exn

let () =
  let xcbs =
    modules |> List.map parse_module
    |> List.map Xobl_compiler.Elaborate.unions_to_switches
    |> Xobl_compiler.Elaborate.resolve_idents
  in
  List.map (Xobl_compiler.Elaborate.do_stuff xcbs) xcbs
  |> List.iter (fun xcb ->
         print_endline
           (Sexplib.Sexp.to_string_hum
              (Xobl_compiler__.Elaboratetree.sexp_of_xcb xcb)))
