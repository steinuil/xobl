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
           | Xobl_compiler.Hir.Core _ -> "xproto"
           | Extension { file_name; _ } -> file_name
         in
         Format.printf "module %s = struct\n" (String.capitalize_ascii name);
         let stri =
           let loc = !Ast_helper.default_loc in
           Xobl_ocaml_backend.Generate_ocaml_v2.Type.stri_module ~loc m
         in
         Format.printf "%a\n" Ppxlib.Pprintast.structure stri;
         Format.printf "\nend\n")
