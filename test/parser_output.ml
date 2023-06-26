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
  Xobl_compiler.Parser.parse_file fname |> Result.get_ok

let () =
  ListLabels.iter modules ~f:(fun m ->
      parse_module m |> Xobl_compiler.Parsetree.sexp_of_xcb
      |> Sexplib.Sexp.to_string_hum |> print_endline)
