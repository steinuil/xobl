open Parsetree
open Parser_utils
open Patche.Combinators
open Patche.Combinators.Infix
open Patche.Xml

type 'a parser = ('a, Parser_utils.error) Patche.Xml.t

let doc = el_discard "doc" |> discard_with Doc

let required_start_align : required_start_align parser =
  el_a "required_start_align"
    (map2 mk_required_start_align (Attr.int "align") (Attr.int_o "offset"))

let import = el_b "import" data ->> mk_import
let xid = el_a "xidtype" (Attr.str "name") ->> mk_xid

let xid_union =
  el_ab "xidunion" Attr.(str "name") (many (el_b "type" data ->> mk_ident))
  ->> mk_xid_union

let typedef =
  el_a "typedef"
    Attr.(map2 mk_typedef (str "newname") (str "oldname" ->> mk_type))

let event_struct =
  el_ab "eventstruct" (Attr.str "name")
    (many
       (el_a "allowed"
          (map4 mk_allowed_events (Attr.str "extension") (Attr.bool "xge")
             (Attr.int "opcode-min") (Attr.int "opcode-max"))))
  ->> mk_event_struct

let enum_item : enum_item parser =
  el_b "value" data ->= try_parse_int64 ->> mk_item_value
  <|> el_b "bit" data ->= try_parse_int ->> mk_item_bit

let enum : declaration parser =
  el_ab "enum" (Attr.str "name")
    (many (el_ab "item" (Attr.str "name") enum_item) *<> opt doc)
  ->> mk_enum

let copy name mk =
  el_a name Attr.(map3 mk (str "name") (str "ref" ->> mk_ident) (int "number"))

let event_copy = copy "eventcopy" mk_event_copy
let error_copy = copy "errorcopy" mk_error_copy

let expression expression =
  let binop =
    el_ab "op" (Attr.str "op" ->= binop) (tuple2 expression expression)
  in
  let unop = el_ab "unop" (Attr.str "op" ->= unop) expression in
  let field_ref = el_b "fieldref" data in
  let param_ref = el_ab "paramref" (Attr.str "type" ->> mk_type) data in
  let enum_ref = el_ab "enumref" (Attr.str "ref" ->> mk_ident) data in
  let pop_count = el_b "popcount" expression in
  let sum_of = el_ab "sumof" (Attr.str "ref") (opt expression) in
  let list_element_ref = el "listelement-ref" in
  let expr_value = el_b "value" data ->= try_parse_int64 in
  let expr_bit = el_b "bit" data ->= try_parse_int in
  choice
    [
      binop ->> mk_binop;
      unop ->> mk_unop;
      field_ref ->> mk_field_ref;
      param_ref ->> mk_param_ref;
      enum_ref ->> mk_enum_ref;
      pop_count ->> mk_pop_count;
      sum_of ->> mk_sum_of;
      list_element_ref |> discard_with List_element_ref;
      expr_value ->> mk_expr_value;
      expr_bit ->> mk_expr_bit;
    ]

let expression : expression parser = fix expression

let field_attr =
  let open Attr in
  tuple2 (str "name")
    (map2 mk_field_type
       (str "type" ->> mk_type)
       (opt
          (choice
             [
               str "enum" ->> mk_allowed_enum;
               str "mask" ->> mk_allowed_mask;
               str "altenum" ->> mk_allowed_alt_enum;
               str "altmask" ->> mk_allowed_alt_mask;
             ])))

let pad_attr =
  let open Attr in
  bool ~default:false "serialize"
  *<> (int "bytes" ->> mk_pad_bytes <|> int "align" ->> mk_pad_align)

let field field =
  let exprfield = el_ab "exprfield" field_attr expression in
  let list = el_ab "list" field_attr (opt expression) in
  let fd = el_a "fd" (Attr.str "name") in
  let pad = el_a "pad" pad_attr in
  let normal_field = el_a "field" field_attr in
  let switch_case type_ =
    el_ab type_ (Attr.str_o "name")
      (tuple2 (many expression *< opt required_start_align) (many field))
    ->> mk_case
  in
  let switch_body =
    let& cond_expr = expression in
    let& _ = opt required_start_align in
    let& case1 = peek in
    let cond, case_type =
      match case1 with
      | `El_start ((_, "case"), _) -> (Cond_eq cond_expr, "case")
      | `El_start ((_, "bitcase"), _) -> (Cond_bit_and cond_expr, "bitcase")
      | _ -> failwith "expected `El_start case or bitcase"
    in
    tuple2 (return cond) (many (switch_case case_type))
  in
  let switch = el_ab "switch" (Attr.str "name") switch_body in
  choice
    [
      exprfield ->> mk_field_expr;
      list ->> mk_field_list;
      fd ->> mk_field_file_descriptor;
      pad ->> mk_field_pad;
      normal_field ->> mk_field;
      switch ->> mk_switch;
    ]

let field = fix field

let event =
  let attrs =
    let open Attr in
    tuple4 (str "name") (int "number")
      (bool ~default:false "xge")
      (bool ~default:false "no-sequence-number")
  in
  el_ab "event" attrs (opt required_start_align *> many field *<> opt doc)
  ->> mk_event

let error =
  el_ab "error"
    (tuple2 (Attr.str "name") (Attr.int "number"))
    (opt required_start_align *> many field)
  ->> mk_error

let union = el_ab "union" (Attr.str "name") (many field) ->> mk_union
let struct_ = el_ab "struct" (Attr.str "name") (many field) ->> mk_struct

let request_reply =
  el_b "reply" (opt required_start_align *> many field *<> opt doc)
  ->> mk_request_reply

let request =
  el_ab "request"
    (tuple3 (Attr.str "name") (Attr.int "opcode")
       (Attr.bool ~default:true "combine-adjacent"))
    (tuple3
       (opt required_start_align *> many field)
       (opt request_reply) (opt doc))
  ->> mk_request

let declaration =
  choice
    [
      import;
      enum;
      event_struct;
      xid;
      xid_union;
      typedef;
      event_copy;
      error_copy;
      union;
      event;
      error;
      struct_;
      request;
    ]

let core =
  let attrs = Attr.str "header" |> satisfies (( = ) "xproto") in
  el_ab "xcb" attrs (many declaration) ->> snd ->> mk_core

let extension =
  let attrs =
    let open Attr in
    tuple6 (str "extension-name") (str "header") (str "extension-xname")
      (bool ~default:false "extension-multiword")
      (int "major-version") (int "minor-version")
  in
  el_ab "xcb" attrs (many declaration) ->> mk_extension

let xcb = dtd *> (core <|> extension)
