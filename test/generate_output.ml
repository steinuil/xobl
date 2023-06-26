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
    (* "xevie"; *)
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
  let fname = Printf.sprintf "../xml-xcb/%s.xml" m in
  Xobl_compiler.Parser.parse_file fname |> Result.get_ok

let () =
  modules |> List.map parse_module |> Xobl_compiler.Hir.of_parsetree
  |> Xobl_compiler.Hir.sort
  |> Xobl_compiler__.Generate_ocaml.gen stdout
