open Xobl_compiler

let parse_module m = Parser.parse_file m |> Result.get_ok
let out_filename ~out_dir ~file_name = Filename.concat out_dir file_name ^ ".ml"

let compile files out_dir =
  let xcbs = List.map parse_module files |> Hir.of_parsetree |> Hir.sort in
  xcbs
  |> List.iter (fun xcb ->
         match xcb with
         | Hir.Core _ ->
             let fname = out_filename ~out_dir ~file_name:"xproto" in
             Out_channel.with_open_text fname (fun out ->
                 Xobl_compiler__.Generate_ocaml.gen_xcb xcbs out xcb)
         | Extension { file_name; _ } ->
             let fname = out_filename ~out_dir ~file_name in
             Out_channel.with_open_text fname (fun out ->
                 Xobl_compiler__.Generate_ocaml.gen_xcb xcbs out xcb))

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "--out-dir" :: out_dir :: files -> compile files out_dir
  | _ -> exit 1
