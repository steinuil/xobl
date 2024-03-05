module Parsetree = Parsetree
module Hir = Hir

type parse_error = Parser_utils.error

let parse source =
  Xmlm.make_input ~strip:true source
  |> Patche.Xml.make_input |> Patche.Xml.run Parser.xcb

let parse_file fname =
  In_channel.with_open_text fname (fun f -> parse (`Channel f))

let parsetree_to_hir xcbs =
  let xcbs =
    Pass1_resolve_idents.resolve xcbs
    |> List.map Pass2_remove_unions.unions_to_switch
    |> List.map Pass3_fixes.apply_fixes
    |> Pass4_serializable_events.mark_eventstruct_events_as_serializable
  in
  List.map (Pass5_parsetree_to_hir.elaborated_parsetree_to_hir xcbs) xcbs
  |> Sort_modules.sort_hir

let compile_files_to_hir files =
  files
  |> List.map (fun fname -> parse_file fname |> Result.get_ok)
  |> parsetree_to_hir

let output_ocaml = Generate_ocaml.gen_xcb
