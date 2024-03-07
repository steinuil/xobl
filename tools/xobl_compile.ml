open Xobl_compiler

let compile files out_dir =
  Xobl_compiler.compile_files_to_hir files
  |> List.iter (fun xcb ->
         let filename =
           match xcb with
           | Hir.Core _ -> "xproto"
           | Extension { file_name; _ } -> file_name
         in
         let out_filename = Filename.concat out_dir filename ^ ".ml" in
         Out_channel.with_open_text out_filename (fun out ->
             Xobl_compiler.output_ocaml out xcb))

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "--out-dir" :: out_dir :: files -> compile files out_dir
  | _ -> exit 1
