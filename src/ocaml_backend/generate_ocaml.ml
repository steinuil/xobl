open Ppxlib
open Xobl_compiler.Hir
module Ident = Casing.OCaml

exception Unexpected of string

let unexpected str = raise (Unexpected str)

exception Not_implemented of string

let not_implemented str = raise (Not_implemented str)
let ( let& ) = Option.bind

(** Current module *)
type ctx = Cm of string

let is_field_visible = function
  | Field _ | Field_list _ | Field_list_simple _ | Field_variant _
  | Field_optional _ ->
      true
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      false

let visible_fields = List.filter is_field_visible

let name_of_field = function
  | Field { name; _ }
  | Field_optional { name; _ }
  | Field_list { name; _ }
  | Field_list_simple { name; _ }
  | Field_variant { name; _ }
  | Field_expr { name; _ }
  | Field_list_length { name; _ }
  | Field_variant_tag { field_name = name; _ }
  | Field_optional_mask { name; _ } ->
      Some name
  | Field_pad _ -> None

let names_of_visible_fields fields =
  visible_fields fields |> List.filter_map name_of_field

let primitive_of_type = function
  | Type_primitive prim -> Some prim
  | Type_ref (_, prim) -> prim
  | Type_union _ -> Some Xid

let prim_to_string = function
  | Void -> "void"
  | Char -> "char"
  | Byte -> "byte"
  | Bool -> "bool"
  | Int8 -> "i8"
  | Int16 -> "i16"
  | Int32 -> "i32"
  | Fd -> "file_descr"
  | Card8 -> "u8"
  | Card16 -> "u16"
  | Card32 -> "u32"
  | Card64 -> "u64"
  | Float -> "float"
  | Double -> "double"
  | Xid -> "xid"

(** Helpers *)

let e_int ?loc ?suffix int =
  Ast_helper.Exp.constant ?loc (Ast_helper.Const.int ?suffix int)

let with_loc ~loc txt = { txt; loc }

let lid ?prefix ?suffix ?parent ~loc name =
  let name = Ident.snake ?prefix ?suffix name in
  let txt =
    match parent with
    | Some parent -> Ldot (Lident parent, name)
    | None -> Lident name
  in
  with_loc ~loc txt

let e_id ?prefix ?suffix ?parent ~loc name =
  let ident = lid ?prefix ?suffix ?parent ~loc name in
  Ast_helper.Exp.ident ~loc ident

let e_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc { id_module; id_name }
    =
  if current_module = id_module then e_id ?prefix ?suffix ~loc id_name
  else e_id ?prefix ?suffix ~parent:(Ident.caml id_module) ~loc id_name

let t_id ?prefix ?suffix ?parent ~loc name =
  let typ = lid ?prefix ?suffix ?parent ~loc name in
  Ast_helper.Typ.constr ~loc typ []

let t_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc { id_module; id_name }
    =
  if current_module = id_module then t_id ?prefix ?suffix ~loc id_name
  else t_id ?prefix ?suffix ~parent:(Ident.caml id_module) ~loc id_name

let p_id ?prefix ?suffix ~loc name =
  let ident = Ident.snake ?prefix ?suffix name |> with_loc ~loc in
  Ast_helper.Pat.var ~loc ident

let vb ?prefix ?suffix ~loc name expr =
  let name = p_id ?prefix ?suffix ~loc name in
  Ast_helper.Vb.mk ~loc name expr

module Type = struct
  let t_prim ~loc prim =
    let str = prim_to_string prim in
    t_id ~loc str

  let t_type ?prefix ?suffix ~ctx ~loc = function
    | Type_primitive prim -> [%type: [%t t_prim ~loc prim]]
    | Type_union _ -> [%type: xid]
    | Type_ref (ident, _) ->
        [%type: [%t t_ident ?prefix ?suffix ~ctx ~loc ident]]

  let t_field_type ~ctx ~loc = function
    | { ft_type; ft_allowed = None } -> t_type ~ctx ~loc ft_type
    | { ft_type = _; ft_allowed = Some (Allowed_enum enum) } ->
        t_ident ~suffix:"enum" ~ctx ~loc enum
    | { ft_type = _; ft_allowed = Some (Allowed_mask mask) } ->
        t_ident ~suffix:"mask" ~ctx ~loc mask
    (* TODO use this?
       | { ft_type = _; ft_allowed = Some (Allowed_alt_enum enum) } ->
           [%type: [ [%t t_ident ~suffix:"enum" ~ctx ~loc enum] | `Custom of int ]]
    *)
    | { ft_type = _; ft_allowed = Some (Allowed_alt_enum enum) } ->
        [%type: [%t t_ident ~suffix:"enum" ~ctx ~loc enum] alt_enum]
    | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
        [%type:
          ( [%t t_ident ~suffix:"mask" ~ctx ~loc mask],
            [%t t_type ~ctx ~loc ft_type] )
          alt_mask]

  let t_list_type ~ctx ~loc = function
    | {
        ft_type = Type_ref ({ id_module = "xproto"; id_name = "CHAR2B" }, None);
        ft_allowed = None;
      } ->
        t_id ~loc "utf16_string"
    | t -> (
        match primitive_of_type t.ft_type with
        | Some Char | Some Void -> t_id ~loc "string"
        | Some _ | None -> [%type: [%t t_field_type ~ctx ~loc t] list])

  let e_list_length ~loc t =
    match primitive_of_type t with
    | Some Char | Some Void -> [%expr String.length]
    | Some _ | None -> [%expr List.length]

  let t_visible_field ~ctx ~loc = function
    | Field { type_; _ } -> t_field_type ~ctx ~loc type_
    | Field_list { type_; _ } | Field_list_simple { type_; _ } ->
        t_list_type ~ctx ~loc type_
    | Field_variant { variant; _ } ->
        t_ident ~suffix:"variant" ~ctx ~loc variant
    | Field_optional { type_; _ } ->
        [%type: [%t t_field_type ~ctx ~loc type_] option]
    | ( Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
      | Field_optional_mask _ ) as f ->
        Format.ksprintf unexpected "field is not visible:\n%s" (show_field f)

  let td_type ?prefix ?suffix ~loc name typ =
    let name = Ident.snake ?prefix ?suffix name |> with_loc ~loc in
    Ast_helper.Type.mk ~loc ~kind:Ptype_abstract ~manifest:typ name

  let t_fields ~ctx ~loc fields =
    match visible_fields fields with
    | [] -> `Type [%type: unit]
    | [ field ] ->
        let typ = t_visible_field ~ctx ~loc field in
        `Type typ
    | fields ->
        let fields =
          ListLabels.map fields ~f:(fun field ->
              let name =
                name_of_field field |> Option.get |> Ident.snake
                |> with_loc ~loc
              in
              let typ = t_visible_field ~ctx ~loc field in
              Ast_helper.Type.field ~loc name typ)
        in
        `Label_declarations fields

  let td_fields ?prefix ?suffix ~ctx ~loc name fields =
    match t_fields ~ctx ~loc fields with
    | `Type typ -> td_type ?prefix ?suffix ~loc name typ
    | `Label_declarations fields ->
        let name = Ident.snake ?prefix ?suffix name |> with_loc ~loc in
        Ast_helper.Type.mk ~loc ~kind:(Ptype_record fields) name

  let rf_enum_item ~loc (name, _) =
    let name = Ident.caml name |> with_loc ~loc in
    Ast_helper.Rf.mk ~loc (Rtag (name, true, []))

  let t_enum_items ~loc items =
    Ast_helper.Typ.variant ~loc (List.map (rf_enum_item ~loc) items) Closed None

  let cd_variant_item ~ctx ~loc { vi_name; vi_tag = _; vi_fields; _ } =
    let name = Ident.caml vi_name |> with_loc ~loc in
    let args =
      match t_fields ~ctx ~loc vi_fields with
      | `Type typ -> Pcstr_tuple [ typ ]
      | `Label_declarations fields -> Pcstr_record fields
    in
    Ast_helper.Type.constructor ~loc name ~args

  let td_variant ?suffix ~ctx ~loc name items =
    let name = Ident.snake ?suffix name |> with_loc ~loc in
    let items = List.map (cd_variant_item ~ctx ~loc) items in
    Ast_helper.Type.mk ~loc ~kind:(Ptype_variant items) name

  let td_event_struct ?suffix ~ctx ~loc name events =
    let name = Ident.snake ?suffix name |> with_loc ~loc in
    let items =
      ListLabels.map events ~f:(fun event ->
          let name = Ident.caml event.id_name |> with_loc ~loc in
          let event = t_ident ~suffix:"event" ~ctx ~loc event in
          Ast_helper.Type.constructor ~loc name ~args:(Pcstr_tuple [ event ]))
    in
    Ast_helper.Type.mk ~loc ~kind:(Ptype_variant items) name

  let td_declaration ~ctx ~loc = function
    | Type_alias { name; type_ } ->
        td_type ~loc name (t_type ~ctx ~loc type_) |> Option.some
    | Struct { name = "CHAR2B"; _ } -> None
    | Struct { name; fields; _ } ->
        td_fields ~ctx ~loc name fields |> Option.some
    | Event { name; fields; _ } ->
        td_fields ~suffix:"event" ~ctx ~loc name fields |> Option.some
    | Error { name; fields; _ } ->
        td_fields ~suffix:"error" ~ctx ~loc name fields |> Option.some
    | Request { name; reply = Some fields; _ } ->
        td_fields ~suffix:"reply" ~ctx ~loc name fields |> Option.some
    | Enum { name; items } ->
        td_type ~suffix:"enum" ~loc name (t_enum_items ~loc items)
        |> Option.some
    | Mask
        { name; items; additional_values = Additional_values [] | None_value }
      ->
        td_type ~suffix:"mask" ~loc name
          [%type: [%t t_enum_items ~loc items] list]
        |> Option.some
    (* TODO make this into [ values | `Flags of [ flags ]]*)
    | Mask { name; items; additional_values = Additional_values values } ->
        let items = t_enum_items ~loc items in
        let values = t_enum_items ~loc values in
        td_type ~suffix:"mask" ~loc name
          [%type: ([%t items] list, [%t values]) mask]
        |> Option.some
    | Variant { name; items; _ } ->
        td_variant ~suffix:"variant" ~ctx ~loc name items |> Option.some
    | Event_copy { name; event; _ } ->
        let event = t_ident ~suffix:"event" ~ctx ~loc event in
        td_type ~suffix:"event" ~loc name event |> Option.some
    | Error_copy { name; error; _ } ->
        let error = t_ident ~suffix:"error" ~ctx ~loc error in
        td_type ~suffix:"error" ~loc name error |> Option.some
    | Event_struct { name; events } ->
        td_event_struct ~ctx ~loc name events |> Option.some
    | Request { reply = None; _ } -> None

  let a_deriving_sexp ~loc =
    Ast_helper.Attr.mk (with_loc ~loc "deriving") (PStr [%str sexp])

  let stri_declaration ~ctx ~loc decl =
    let& decl = td_declaration ~ctx ~loc decl in
    let decl = { decl with ptype_attributes = [ a_deriving_sexp ~loc ] } in
    Ast_helper.Str.type_ ~loc Recursive [ decl ] |> Option.some
end

let e_prim_to_int ~loc = function
  | Char | Byte -> Some [%expr Char.code]
  | Bool -> Some [%expr Bool.to_int]
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some [%expr file_descr_to_int]
  | Int32 | Card32 -> None
  | Card64 -> Some [%expr Int64.to_int]
  | (Void | Float | Double) as p -> unexpected (show_prim p)

let e_prim_of_int ~loc = function
  | Char | Byte -> Some [%expr Char.chr]
  | Bool -> Some [%expr bool_of_int]
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some [%expr Obj.magic]
  | Int32 | Card32 -> None
  | Card64 -> Some [%expr Int64.of_int]
  | (Void | Float | Double) as t -> unexpected (show_prim t)

let e_prim_to_int64 ~loc = function
  | Char | Byte -> Some [%expr char_to_int64]
  | Bool -> Some [%expr bool_to_int64]
  | Int8 | Int16 | Card8 | Card16 | Xid -> Some [%expr Int64.of_int]
  | Fd -> Some [%expr file_descr_of_int]
  | Int32 | Card32 -> Some [%expr Int64.of_int]
  | Card64 -> None
  | Void | Float | Double -> failwith "gen_to_int64"

let e_binop ~loc = function
  | Add -> [%expr ( + )]
  | Sub -> [%expr ( - )]
  | Div -> [%expr ( / )]
  | Mul -> [%expr ( * )]
  | Bit_and -> [%expr ( land )]
  | Bit_left_shift -> [%expr ( lsl )]

let rec e_expression ?it ~loc = function
  | Binop (op, e1, e2) ->
      [%expr
        [%e e_binop ~loc op] [%e e_expression ?it ~loc e1]
          [%e e_expression ?it ~loc e2]]
  | Unop (Bit_not, e) -> [%expr lnot [%e e_expression ?it ~loc e]]
  | Field_ref f -> (
      match it with None -> e_id ~loc f | Some it -> e_id ~loc ~parent:it f)
  | List_element_ref -> (
      match it with
      | None -> unexpected "List_element_ref outside of a Sum_of expression"
      | Some it -> e_id ~loc it)
  | Enum_ref _ -> unexpected "Enum_ref"
  | Param_ref { param = name; type_ = _ } -> e_id ~prefix:"ext" ~loc name
  | Pop_count e -> [%expr pop_count [%e e_expression ?it ~loc e]]
  | Expr_value v -> e_int (Int64.to_int v)
  | Expr_bit b -> [%expr 1 lsl [%e e_int b]]
  | Sum_of { field; by_expr = None } ->
      [%expr List.fold_left ( + ) 0 [%e e_id ~loc field]]
  | Sum_of { field; by_expr = Some by_expr } ->
      [%expr
        sum_of_expr
          (fun list_element_ref ->
            [%e e_expression ~it:"list_element_ref" ~loc by_expr])
          [%e e_id ~loc field]]

module Decode = struct
  let e_prim ~loc = function
    | Bool -> [%expr Decode.bool]
    | Int8 -> [%expr Decode.i8]
    | Card8 -> [%expr Decode.u8]
    | Int16 -> [%expr Decode.i16]
    | Card16 -> [%expr Decode.u16]
    | Int32 -> [%expr Decode.i32]
    | Card32 -> [%expr Decode.u32]
    | Card64 -> [%expr Decode.u64]
    | Void -> [%expr Decode.void]
    | Char -> [%expr Decode.char]
    | Byte -> [%expr Decode.byte]
    | Float -> [%expr Decode.float]
    | Double -> [%expr Decode.double]
    | Fd -> [%expr Decode.file_descr]
    | Xid -> [%expr Decode.xid]

  let e_type ~ctx ~loc = function
    | Type_primitive prim -> e_prim ~loc prim
    | Type_union _ -> e_prim ~loc Xid
    | Type_ref (ident, _) -> e_ident ~prefix:"decode" ~ctx ~loc ident

  let e_field_type ~ctx ~loc = function
    | { ft_type; ft_allowed = None } -> e_type ~ctx ~loc ft_type
    | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
        let decode_t = e_type ~ctx ~loc ft_type in
        let to_int =
          primitive_of_type ft_type |> Option.get |> e_prim_to_int ~loc
          |> Option.value ~default:[%expr identity]
        in
        let enum_of_int = e_ident ~suffix:"enum_of_int" ~ctx ~loc enum in
        [%expr
          Decode.enum ~decode:[%e decode_t] ~int_of_t:[%e to_int]
            ~enum_of_int:[%e enum_of_int]]
    | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
        let decode_t = e_type ~ctx ~loc ft_type in
        let to_int =
          primitive_of_type ft_type |> Option.get |> e_prim_to_int ~loc
          |> Option.value ~default:[%expr identity]
        in
        let enum_of_int = e_ident ~suffix:"enum_of_int" ~ctx ~loc enum in
        [%expr Decode.alt_enum [%e decode_t] [%e to_int] [%e enum_of_int]]
    | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
        let decode_t = e_type ~ctx ~loc ft_type in
        let to_int =
          primitive_of_type ft_type |> Option.get |> e_prim_to_int64 ~loc
          |> Option.value ~default:[%expr identity]
        in
        let mask_of_int = e_ident ~suffix:"mask_of_int64" ~ctx ~loc mask in
        [%expr Decode.mask [%e decode_t] [%e to_int] [%e mask_of_int]]
    | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
        let decode_t = e_type ~ctx ~loc ft_type in
        let to_int =
          primitive_of_type ft_type |> Option.get |> e_prim_to_int64 ~loc
          |> Option.value ~default:[%expr identity]
        in
        let mask_of_int = e_ident ~suffix:"mask_of_int64" ~ctx ~loc mask in
        [%expr Decode.alt_mask [%e decode_t] [%e to_int] [%e mask_of_int]]

  let e_list_type ~ctx ~loc = function
    | {
        ft_type = Type_ref ({ id_module = "xproto"; id_name = "CHAR2B" }, None);
        ft_allowed = None;
      } ->
        [%expr Decode.utf16_string]
    | t -> (
        match primitive_of_type t.ft_type with
        | Some Char | Some Void -> [%expr Decode.string]
        | Some _ | None ->
            let decode_t = e_field_type ~ctx ~loc t in
            [%expr Decode.list ~item:[%e decode_t]])

  let e_field ~ctx ~loc = function
    | Field { name; type_ } ->
        let body = e_field_type ~ctx ~loc type_ in
        [ `Let (name, [%expr [%e body] buf]) ]
    | Field_pad { pad = Pad_bytes n; _ } ->
        [ `Sequence [%expr Decode.pad buf [%e e_int n]] ]
    | Field_pad { pad = Pad_align n; _ } ->
        [ `Sequence [%expr Decode.align buf [%e e_int n]] ]
    | Field_list_simple { name; type_; length } ->
        let body = e_list_type ~ctx ~loc type_ in
        [ `Let (name, [%expr [%e body] ~len:[%e e_id ~loc length] buf]) ]
    | Field_list_length { name; type_; expr; _ } -> (
        let body = e_type ~ctx ~loc type_ in
        match expr with
        | None -> [ `Let (name, [%expr [%e body] buf]) ]
        | Some expr ->
            [
              `Let (name, [%expr [%e body] buf]);
              `Let (name, e_expression ~loc expr);
            ])
    | Field_list { name; type_; length = Some length } ->
        [
          `Let
            ( name,
              [%expr
                [%e e_list_type ~ctx ~loc type_]
                  ~len:[%e e_expression ~loc length] buf] );
        ]
    | Field_variant_tag { field_name; variant = _; type_ } ->
        let body = e_type ~ctx ~loc type_ in
        [ `Let (field_name ^ "_tag", [%expr [%e body] buf]) ]
    | _ -> []
  (* | f -> Printf.ksprintf unexpected "field: %s" (show_field f) *)

  let e_struct_fields ~ctx ~loc fields =
    let result =
      let fields =
        names_of_visible_fields fields
        |> List.map (fun name -> (lid ~loc name, e_id ~loc name))
      in
      Ast_helper.Exp.record ~loc fields None
    in
    List.concat_map (e_field ~ctx ~loc) fields
    |> ListLabels.fold_right ~init:result ~f:(fun field expr ->
           match field with
           | `Let (name, body) ->
               let binding = vb ~loc name body in
               Ast_helper.Exp.let_ ~loc Nonrecursive [ binding ] expr
           | `Sequence body -> Ast_helper.Exp.sequence ~loc body expr)

  let e_struct ~ctx ~loc name fields external_params =
    let fields = e_struct_fields ~ctx ~loc fields in
    let type_ = t_id ~loc name in
    match external_params with
    | [] -> [%expr fun buf : [%t type_] -> [%e fields]]
    | [ param ] ->
        let param = p_id ~loc ~prefix:"ext" param.ep_name in
        [%expr fun buf [%p param] : [%t type_] -> [%e fields]]
    | _ -> not_implemented "multiple param_refs"

  let vb_declaration ~ctx ~loc = function
    | Type_alias { name; type_ } ->
        vb ~prefix:"decode" ~loc name (e_type ~ctx ~loc type_) :: []
    | Struct { name; fields; external_params } ->
        let expr = e_struct ~ctx ~loc name fields external_params in
        vb ~prefix:"decode" ~loc name expr :: []
        (* | Enum { name; items } -> *)
    | Event_copy { name; event; _ } ->
        let event = e_ident ~prefix:"decode" ~suffix:"event" ~ctx ~loc event in
        vb ~prefix:"decode" ~suffix:"event" ~loc name event :: []
    | Error_copy { name; error; _ } ->
        let error = e_ident ~prefix:"decode" ~suffix:"error" ~ctx ~loc error in
        vb ~prefix:"decode" ~suffix:"error" ~loc name error :: []
    | _ -> not_implemented "declaration"

  let stri_declaration ~ctx ~loc decl =
    let decls = vb_declaration ~ctx ~loc decl in
    List.map (fun decl -> Ast_helper.Str.value ~loc Nonrecursive [ decl ]) decls
end
