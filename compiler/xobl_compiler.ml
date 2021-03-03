module Parser = struct
  type error = Parser_utils.error

  let parse source =
    Xmlm.make_input ~strip:true source
    |> Patche.Xml.make_input |> Patche.Xml.run Parser.xcb
end

let unions_to_switch = Elaborate_unions_to_switch.unions_to_switch

let resolve = Elaborate_resolve.resolve

module Parsetree = Parsetree
