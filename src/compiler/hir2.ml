open Sexplib.Conv

module Ident = struct
  type t = { extension : string; name : string } [@@deriving show, sexp]
end

module Type = struct
  type primitive =
    | Void
    | Char
    | Byte
    | Bool
    | Int8
    | Int16
    | Int32
    | Fd  (** https://github.com/keith-packard/fdpassing *)
    | Card8
    | Card16
    | Card32
    | Card64
    | Float
    | Double
    | Xid  (** maps to a Card32 *)
  [@@deriving show, sexp]

  type t =
    | Primitive of primitive
    | Reference of Ident.t * primitive option
    | Xid_union of Ident.t list
  [@@deriving show, sexp]

  let primitive = function
    | Primitive prim -> Some prim
    | Reference (_, prim) -> prim
    | Xid_union _ -> Some Xid
end

module Expr = struct
  type binop = Parsetree.binop =
    | Add
    | Sub
    | Mul
    | Div
    | Bit_and
    | Bit_left_shift
  [@@deriving show, sexp]

  type unop = Parsetree.unop = Bit_not [@@deriving show, sexp]

  type t =
    | Binop of binop * t * t
    | Unop of unop * t
    | Field_ref of string
    | Param_ref of { param : string; type_ : Type.t }
    | Enum_ref of { enum : Ident.t; item : string }
    | Pop_count of t
    | Sum_of of { field : string; by_expr : t option }
    | List_element_ref
    | Int of int64
    | Bit of int
  [@@deriving show, sexp]
end

module Field = struct
  type t = Pad of int | Align of int [@@deriving show, sexp]
end
