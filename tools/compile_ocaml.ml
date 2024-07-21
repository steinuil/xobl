let compile files out_dir =
  Xobl_compiler.compile_files_to_hir files
  |> List.iter (fun m ->
         let filename =
           match m with
           | Xobl_compiler.Hir.Core _ -> "xproto"
           | Extension { file_name; _ } -> file_name
         in
         let name =
           match m with
           | Xobl_compiler.Hir.Core _ -> "xproto"
           | Extension { file_name; _ } -> file_name
         in
         let declarations =
           match m with
           | Xobl_compiler.Hir.Core decls -> decls
           | Extension { declarations; _ } -> declarations
         in
         let stri =
           List.concat_map
             (fun decl ->
               let loc = !Ast_helper.default_loc in
               let ctx = Xobl_ocaml_backend.Generate_ocaml.Cm name in
               let x =
                 Xobl_ocaml_backend.Generate_ocaml.Type.stri_declaration ~loc
                   ~ctx decl
                 |> Option.to_list
               in
               let decode =
                 try
                   Xobl_ocaml_backend.Generate_ocaml.Decode.stri_declaration
                     ~loc ~ctx decl
                 with Xobl_ocaml_backend.Generate_ocaml.Not_implemented _ ->
                   []
               in
               x @ decode)
             declarations
         in
         let out_filename = Filename.concat out_dir filename ^ ".ml" in
         Out_channel.with_open_text out_filename (fun out ->
             let out = Format.formatter_of_out_channel out in
             Format.fprintf out "open[@ocaml.warning \"-33\"] Types\n";
             Format.fprintf out "open Sexplib.Conv\n";
             Format.fprintf out "%a\n" Ppxlib.Pprintast.structure stri))

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "--out-dir" :: out_dir :: files -> compile files out_dir
  | _ -> exit 1
