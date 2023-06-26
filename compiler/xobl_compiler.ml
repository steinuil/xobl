module Parsetree = Parsetree

module Parser = struct
  type error = Parser_utils.error

  let parse source =
    Xmlm.make_input ~strip:true source
    |> Patche.Xml.make_input |> Patche.Xml.run Parser.xcb
end

module Hir = struct
  include Hir

  let of_parsetree xcbs =
    let xcbs = Pass_resolve_idents.resolve xcbs in
    let xcbs = List.map Pass_remove_unions.unions_to_switch xcbs in
    let xcbs = List.map Pass_fixes.apply_fixes xcbs in
    List.map (Compile_hir.compile_hir xcbs) xcbs
end
