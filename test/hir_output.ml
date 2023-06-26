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
  let fname = Printf.sprintf "../xml-xcb/%s.xml" m in
  In_channel.with_open_text fname (fun f ->
      Xobl_compiler.Parser.parse (`Channel f))
  |> Result.get_ok

let () =
  modules |> List.map parse_module |> Xobl_compiler.Hir.of_parsetree
  |> List.iter (fun xcb ->
         Xobl_compiler.Hir.sexp_of_xcb xcb
         |> Sexplib.Sexp.to_string_hum |> print_endline)
