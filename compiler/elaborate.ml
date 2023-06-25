(*

- Some list fields don't explicitly specify a length field even though they
  need one (e.g. QueryTextExtents in xproto), so we need to infer it somehow.
  There's some code in xcbgen that does this apparently.

Need a change in the AST:

- Figure out what's the field that holds the length of a list and simply
  calculate it from the length of the list rather than exposing it to the API.

- There's a single exprfield in the spec and it's to check whether the length
  of a list is odd or even, we could just special-case it.

- Turn switches whose expression is equality to an enum value into variants,
  and make another separate kind of field for the enum value like for the list
  length one.

- Turn switches whose expression is bit and into just a struct with all fields
  optional
  Note that some flag switches (in xkb, obviously) don't use a simple field but
  an arbitrary expression, but again, we're not supporting xkb.

*)

let unions_to_switches = Pass_remove_unions.unions_to_switch
let resolve_idents = Pass_resolve_idents.resolve

let do_stuff xcbs = function
  | Parsetree.Core declarations ->
      let declarations =
        Pass_declaration_order.fix_xproto_declaration_order declarations
      in
      Elaborate_masks.in_xcb xcbs (Parsetree.Core declarations)
  | extension -> Elaborate_masks.in_xcb xcbs extension
