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

let () =
  modules
  |> List.map (Printf.sprintf "../xml-xcb/%s.xml")
  |> Xobl_compiler.compile_files_to_hir
  |> List.iter (fun m ->
         let name =
           match m with
           | Xobl_compiler.Hir.Core _ -> "Xproto"
           | Extension { file_name; _ } -> String.capitalize_ascii file_name
         in
         Printf.fprintf stdout "module %s = struct\n" name;
         Xobl_compiler.output_ocaml stdout m;
         Printf.fprintf stdout "\nend\n")
