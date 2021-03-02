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
    "xkb";
    "xprint";
    "xproto";
    "xselinux";
    "xtest";
    "xv";
    "xvmc";
  ]

exception Test_failed of Xobl_compiler.Parser.error

let parse_module m =
  let f = open_in (Printf.sprintf "../xml-xcb/%s.xml" m) in
  try
    ( match Xobl_compiler.Parser.parse (`Channel f) with
    | Error err -> raise (Test_failed err)
    | Ok xcb -> print_endline (Xobl_compiler__.Parsetree.show_xcb xcb) );
    close_in f
  with exn ->
    close_in f;
    raise exn

let () = modules |> List.iter parse_module
