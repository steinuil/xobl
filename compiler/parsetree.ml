(**
Here's an explaination of some of the most puzzling aspects of the X11 spec.

{2 Types}

Types in the spec refer to two differen kinds of types, which we'll call
{b basic} and {b composite}.
Types are used to give a wire representation to the values defined in the spec,
so they define size, alignment, and in the case of integers signedness.

{3 Basic types}

Basic types such as [INT8], [CARD16] and [float] are used to define the wire
representation of numbers. All types except for [void] should map to a certain
type in the output bindings.

The [void] type is used for defining fields in which the type doesn't matter,
like padding, or where it cannot be known in advance, like in [GetProperty].

{3 Composite types]

Composite types are aggregations of basic types in the form of simple fields,
lists, or expressions, and also padding and alignment information.

Their size is known at compile time except for generic events, requests, and
responses, which might include a variable length list at as their last element.
Requests may also include expression fields, whose value depends ont he value
of other fields in the request struct.

Structs and requests may include a switch, which uses another field to
determine which additional fields should be included in the struct.

Unions are an earlier version of switches, which require additional processing
to determine which field is included (the method used depends on the extension,
of course: xkb uses a field named [type] which refers to an enum while xproto
has an ad hoc field in the containing struct).

{2 Namespaces}

The declarations have a few separate namespaces:

- types
- enums
- events and generic events
- errors
- requests

{3 Name clashes}
Name clashes {i within} these namespaces are allowed between different
extensions. To disambiguate in the case that a declaration references a
name exported by two or more extensions currently in scope, the extension's
ID (here aliased as [file_name]) is prefixed to the name with a colon, such
as [xproto:PIXMAP].

{b NOTE}: some extensions {b do not} follow this rule, in which case I assumed
that names defined in the current extension take precedence over the rest.

{2 Data alignment}

C compilers will automatically add some padding between struct members as
needed to make sure the members are aligned in memory for optimal access
(refer to http://www.catb.org/esr/structure-packing/ for a much better
explaination). libxcb was designed only with C in mind, so alignment
padding used to implicit and not marked in the protocol files in any way.

In an admirable effort, somebody (I haven't followed the mailing list, but
the commit adding this was authored by Christian Linhart) decided to not only
explicitate these alignment pads in the protocol (through the <pad align />
fields, here [Pad_align]), but also to add an alignment checker that ensures
there's no implicit alignment left in any of the protocol descriptions.

Checking the protocol files for correctness is beyond the scope of this
project, so I'll just trust the Xproto team to run their own checkers.

See commit [c499401bdac3b87bd4f9cd4bc64cfd1781ab447f] in the xproto repo
for more information on the alignment checker.

*)

type doc = Doc [@@deriving show]

type required_start_align = { al_align : int; al_offset : int option }
[@@deriving show]
(** From what I could gather, the X11 protocol used to be defined by its C
    implementation so the padding between fields used to be "whatever the C
    compiler said". When it was formalized in the XML protocol files they
    decided to make padding explicit and probably ran into 

    This led to the introduction of an alignment checker in commit
    [c499401bdac3b87bd4f9cd4bc64cfd1781ab447f], and of the
    required_start_align field.
    The algorithm is described in the commit message on the xcb/proto repo. *)

type enum_item = Item_value of int64 | Item_bit of int [@@deriving show]

type binop = Add | Sub | Mul | Div | Bit_and | Bit_left_shift
[@@deriving show]

type unop = Bit_not [@@deriving show]

type ident = { id_module : string option; id_name : string } [@@deriving show]

type prim =
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
[@@deriving show]

type type_ = Type_primitive of prim | Type_ref of ident [@@deriving show]

type expression =
  | Binop of binop * expression * expression
  | Unop of unop * expression
  | Field_ref of string
  | Param_ref of { param : string; type_ : type_ }
  | Enum_ref of { enum : ident; item : string }
  | Pop_count of expression
  | Sum_of of { field : string; by_expr : expression option }
  | List_element_ref
  | Expr_value of int64
  | Expr_bit of int
[@@deriving show]

type 'a range = { min : 'a; max : 'a } [@@deriving show]

type allowed_events = {
  ae_module : string;
  ae_opcode_range : int range;
  ae_is_xge : bool;
}
[@@deriving show]

type pad = Pad_bytes of int | Pad_align of int [@@deriving show]

type field_allowed =
  | Allowed_enum of ident
  | Allowed_mask of ident
  | Allowed_alt_enum of ident
  | Allowed_alt_mask of ident
[@@deriving show]

type field_type = { ft_type : type_; ft_allowed : field_allowed option }
[@@deriving show]

type switch_cond = Cond_bit_and of expression | Cond_eq of expression
[@@deriving show]

type switch = { sw_name : string; sw_cond : switch_cond; sw_cases : case list }
[@@deriving show]

and case = {
  cs_name : string option;
  cs_cond : expression list;
  cs_fields : field list;
}
[@@deriving show]

and field =
  | Field_expr of { name : string; type_ : field_type; expr : expression }
  | Field_list of {
      name : string;
      type_ : field_type;
      length : expression option;
    }
  | Field_file_descriptor of string
  | Field_pad of { pad : pad; serialize : bool }
  | Field_switch of switch
  | Field of { name : string; type_ : field_type }
[@@deriving show]

type request_reply = { fields : field list; doc : doc option } [@@deriving show]

type declaration =
  | Import of string
  | Xid of string
  | Xid_union of { name : string; types : ident list }
  | Typedef of { name : string; type_ : type_ }
  | Event_copy of { name : string; event : ident; ev_number : int }
  | Error_copy of { name : string; error : ident; er_number : int }
  | Enum of {
      name : string;
      items : (string * enum_item) list;
      doc : doc option;
    }
  | Event_struct of { name : string; allowed_events : allowed_events list }
  | Union of { name : string; members : field list }
  | Event of {
      name : string;
      number : int;
      is_generic : bool;
      no_sequence_number : bool;
      fields : field list;
      doc : doc option;
    }
  | Error of { name : string; number : int; fields : field list }
  | Struct of { name : string; fields : field list }
  | Request of {
      name : string;
      opcode : int;
      combine_adjacent : bool;
      fields : field list;
      reply : request_reply option;
      doc : doc option;
    }
[@@deriving show]

type xcb =
  | Core of declaration list
  | Extension of {
      name : string;
      file_name : string;
      query_name : string;
      multiword : bool;
      version : int * int;
      declarations : declaration list;
    }
[@@deriving show]
