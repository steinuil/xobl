open Sexplib.Conv

type binop = Parsetree.binop =
  | Add
  | Sub
  | Mul
  | Div
  | Bit_and
  | Bit_left_shift
[@@deriving show, sexp]

type unop = Parsetree.unop = Bit_not [@@deriving show, sexp]
type ident = { id_module : string; id_name : string } [@@deriving show, sexp]

type prim = Parsetree.prim =
  | Void
  | Char
  | Byte
  | Bool
  | Int8
  | Int16
  | Int32
  | Fd
  | Card8
  | Card16
  | Card32
  | Card64
  | Float
  | Double
  | Xid  (** maps to a Card32 *)
[@@deriving show, sexp]

type type_ =
  | Type_primitive of prim
  | Type_ref of ident
  | Type_union of ident list
[@@deriving show, sexp]

type expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Field_ref of string
  (* The field's type should probably be resolved so that when outputting
     expressions we know which conversion function to use when the type of the
     field is not compatible with that of the length, i.e. card32. *)
  | Param_ref of { param : string; type_ : type_ option }
  | Enum_ref of { enum : ident; item : string }
  | Pop_count of expression
  | Sum_of of { field : string; by_expr : expression option }
  | List_element_ref
  | Expr_value of int64
  | Expr_bit of int
[@@deriving show, sexp]

type pad = Parsetree.pad = Pad_bytes of int | Pad_align of int
[@@deriving show, sexp]

type field_allowed =
  | Allowed_enum of ident
  | Allowed_mask of ident
  | Allowed_alt_enum of ident
  | Allowed_alt_mask of ident
[@@deriving show, sexp]

type field_type = { ft_type : type_; ft_allowed : field_allowed option }
[@@deriving show, sexp]

type field =
  | Field of { name : string; type_ : field_type }
  | Field_expr of { name : string; type_ : field_type; expr : expression }
  | Field_file_descriptor of string
  | Field_pad of { pad : pad; serialize : bool }
  | Field_list of {
      name : string;
      type_ : field_type;
      length : expression option;
    }
  | Field_list_simple of { name : string; type_ : field_type; length : string }
      (** List with an associated {!constructor:Field_list_length} length. *)
  | Field_list_length of {
      name : string;
      type_ : type_;
      expr : expression;
      list : string;
      list_type : type_;
    }
      (** Contains the length of the associated simple list.
          Should be hidden in the public API. *)
  | Field_variant of { name : string; variant : ident }
  | Field_variant_tag of { field_name : string; variant : ident; type_ : type_ }
      (** The tag that discriminates between the branches of the associated
          variant. Should be hidden in the public API. *)
  | Field_optional of {
      name : string;
      mask : string;
      bit : int;
      type_ : field_type;
    }
      (** Field that can be [None] (or [null] depending on the language).
          The presence of the field is recorded in the associated mask
          by setting the bit at position {!fieldref:bit}. *)
  | Field_optional_mask of {
      name : string;
      type_ : type_;
      fields : (string * int) list;
    }
      (** Contains the mask that indicates whether the optional fields
          associated are present or not in the struct.
          Should be hidden in the public API. *)
[@@deriving show, sexp]

type variant_item = { vi_name : string; vi_tag : int64; vi_fields : field list }
[@@deriving show, sexp]

type mask_additional_value =
  | None_value
  | Additional_values of (string * int64) list
[@@deriving show, sexp]

type declaration =
  | Type_alias of { name : string; type_ : type_ }
  | Struct of { name : string; fields : field list }
  | Event_struct of { name : string; events : ident list }
  | Variant of { name : string; items : variant_item list }
  | Enum of { name : string; items : (string * int64) list }
  | Mask of {
      name : string;
      items : (string * int) list;
      additional_values : mask_additional_value;
    }
  | Event_copy of {
      name : string;
      event : ident;
      number : int;
      is_serializable : bool;
    }
  | Error_copy of { name : string; error : ident; number : int }
  | Event of {
      name : string;
      number : int;
      is_generic : bool;
      is_serializable : bool;
      no_sequence_number : bool;
      fields : field list;
    }
  | Error of { name : string; number : int; fields : field list }
  | Request of {
      name : string;
      opcode : int;
      combine_adjacent : bool;
      fields : field list;
      reply : field list option;
    }
[@@deriving show, sexp]

type xcb =
  | Core of declaration list
  | Extension of {
      name : string;
      file_name : string;
      query_name : string;
      multiword : bool;
      version : int * int;
      imports : string list;
      declarations : declaration list;
    }
[@@deriving show, sexp]
