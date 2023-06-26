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

let parse_module m =
  let fname = Printf.sprintf "../xml-xcb/%s.xml" m in
  In_channel.with_open_text fname (fun inp ->
      Xobl_compiler.Parser.parse (`Channel inp))
  |> Result.get_ok |> Xobl_compiler.Parsetree.sexp_of_xcb
  |> Sexplib.Sexp.to_string_hum |> print_endline

let () = List.iter parse_module modules
