(*

- Resolve all identifiers to structs and enums and all types.
  i.e. some of them specify the module they're in, but some don't.

- Some list fields don't explicitly specify a length field even though they
  need one (e.g. QueryTextExtents in xproto), so we need to infer it somehow.
  There's some code in xcbgen that does this apparently.

Outside of scope of this module?:

- Figure out what's the field that holds the length of a list and simply
  calculate it from the length of the list rather than exposing it to the API.

- There's a single exprfield in the spec and it's to check whether the length
  of a list is odd or even, we could just special-case it.

- Turn switches whose expression is equality to an enum value into variants,
  and make another separate kind of field for the enum value like for the list
  length one.

*)

let () = ()
