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

let () =
  let loc = !Ast_helper.default_loc in
  modules
  |> List.map (Printf.sprintf "../xml-xcb/%s.xml")
  |> Xobl_compiler.compile_files_to_hir
  |> Xobl_ocaml_backend.Generate_ocaml_v2.Protocol.stri_protocols ~loc
  |> Format.printf "%a" Ppxlib.Pprintast.structure
