let compile files out_dir =
  let loc = !Ast_helper.default_loc in
  let hir = Xobl_compiler.compile_files_to_hir files in
  let () =
    let generated_code =
      Xobl_ocaml_backend.Generate_ocaml_v2.Protocol.stri_protocols ~loc hir
    in
    let out_filename = Filename.concat out_dir "protocol.ml" in
    Out_channel.with_open_text out_filename (fun out ->
        let out = Format.formatter_of_out_channel out in
        Format.fprintf out "%a@." Ppxlib.Pprintast.structure generated_code)
  in
  let () =
    let generated_code =
      Xobl_ocaml_backend.Generate_ocaml_v2.Codecs.str_protocols ~loc hir
    in
    let out_filename = Filename.concat out_dir "codec.ml" in
    Out_channel.with_open_text out_filename (fun out ->
        let out = Format.formatter_of_out_channel out in
        Format.fprintf out "%a@." Ppxlib.Pprintast.structure generated_code)
  in

  ()

let () =
  match Sys.argv |> Array.to_list |> List.tl with
  | "--out-dir" :: out_dir :: files -> compile files out_dir
  | _ -> exit 1
