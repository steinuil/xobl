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
  modules
  |> List.map (Printf.sprintf "../xml-xcb/%s.xml")
  |> Xobl_compiler.compile_files_to_hir
  |> List.iter (fun xcb ->
         Xobl_compiler.Hir.sexp_of_xcb xcb
         |> Sexplib.Sexp.to_string_hum |> print_endline)
