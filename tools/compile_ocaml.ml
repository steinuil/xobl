let compile files out_dir =
  Xobl_compiler.compile_files_to_hir files
  |> List.iter (fun m ->
         let filename =
           match m with
           | Xobl_compiler.Hir.Core _ -> "xproto"
           | Extension { file_name; _ } -> file_name
         in
         let str =
           let loc = !Ast_helper.default_loc in
           Xobl_ocaml_backend.Generate_ocaml_v2.Type.stri_module ~loc m
         in
         let out_filename = Filename.concat out_dir filename ^ ".ml" in
         Out_channel.with_open_text out_filename (fun out ->
             Printf.fprintf out "[@@@ocaml.warning \"-12\"]\n";
             Printf.fprintf out "open[@ocaml.warning \"-33\"] Types\n";
             Printf.fprintf out "open[@ocaml.warning \"-33\"] Sexplib.Conv\n";
             let out = Format.formatter_of_out_channel out in
             Format.fprintf out "%a\n" Ppxlib.Pprintast.structure str))

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "--out-dir" :: out_dir :: files -> compile files out_dir
  | _ -> exit 1
