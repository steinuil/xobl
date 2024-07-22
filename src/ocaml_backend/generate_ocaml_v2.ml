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

let e_int64 ?loc ?suffix int =
  Ast_helper.Exp.constant ?loc (Ast_helper.Const.int64 ?suffix int)

let e_str ~loc str =
  Ast_helper.Exp.constant ~loc (Ast_helper.Const.string ~loc str)

let with_loc ~loc txt = { txt; loc }

let lid ?prefix ?suffix ?parent ~loc name =
  let name = Ident.snake ?prefix ?suffix name in
  let txt =
    match parent with
    | Some parent -> Ldot (Lident parent, name)
    | None -> Lident name
  in
  with_loc ~loc txt

let lid_caml ?prefix ?suffix ?parent ~loc name =
  let name = Ident.caml ?prefix ?suffix name in
  let txt =
    match parent with
    | Some parent -> Ldot (Lident parent, name)
    | None -> Lident name
  in
  with_loc ~loc txt

let lid_module_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc
    { id_module; id_name } name =
  let module_name = Ident.caml ?prefix ?suffix id_name in
  let txt =
    if current_module = id_module then Ldot (Lident module_name, name)
    else
      let parent = Ident.caml id_module in
      Ldot (Ldot (Lident parent, module_name), name)
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

let t_module_ident ?prefix ?suffix ~ctx ~loc ident name =
  let lid = lid_module_ident ?prefix ?suffix ~ctx ~loc ident name in
  Ast_helper.Typ.constr ~loc lid []

let p_id ?prefix ?suffix ~loc name =
  let ident = Ident.snake ~sanitize:"_" ?prefix ?suffix name |> with_loc ~loc in
  Ast_helper.Pat.var ~loc ident

let p_int ~loc n =
  let n = Ast_helper.Const.int n in
  Ast_helper.Pat.constant ~loc n

let e_variant ~loc name = Ast_helper.Exp.variant ~loc (Ident.caml name) None

let vb ?prefix ?suffix ~loc name expr =
  let name = p_id ?prefix ?suffix ~loc name in
  Ast_helper.Vb.mk ~loc name expr

let rf_poly_item ~loc name =
  let name = Ident.caml name |> with_loc ~loc in
  Ast_helper.Rf.mk ~loc (Rtag (name, true, []))

let t_poly ~loc items =
  let items = List.map (rf_poly_item ~loc) items in
  Ast_helper.Typ.variant ~loc items Closed None

module Type = struct
  let a_deriving_sexp ~loc =
    Ast_helper.Attr.mk (with_loc ~loc "deriving") (PStr [%str sexp])

  let t_prim ~loc prim =
    let str = prim_to_string prim in
    t_id ~loc str

  let t_type ?prefix ?suffix ~ctx ~loc = function
    | Type_primitive prim -> t_prim ~loc prim
    | Type_union _ -> [%type: xid]
    | Type_ref (ident, _) -> t_ident ?prefix ?suffix ~ctx ~loc ident

  let t_field_type ~ctx ~loc { ft_type; ft_allowed } =
    match ft_allowed with
    | None -> t_type ~ctx ~loc ft_type
    | Some (Allowed_enum enum) ->
        t_module_ident ~suffix:"enum" ~ctx ~loc enum "t"
    | Some (Allowed_mask mask) ->
        t_module_ident ~suffix:"mask" ~ctx ~loc mask "t"
    | Some (Allowed_alt_enum enum) ->
        let type_ = t_type ~ctx ~loc ft_type in
        let enum = t_module_ident ~suffix:"enum" ~ctx ~loc enum "t" in
        [%type: [ [%t enum] | [%t type_] alt ]]
    | Some (Allowed_alt_mask mask) ->
        t_module_ident ~suffix:"mask" ~ctx ~loc mask "t"

  let td_type ?prefix ?suffix ~loc name typ =
    let name = Ident.snake ?prefix ?suffix name |> with_loc ~loc in
    Ast_helper.Type.mk ~loc ~kind:Ptype_abstract ~manifest:typ name

  let td_type_declaration ~ctx ~loc = function
    | Type_alias { name; type_ } ->
        let t = t_type ~ctx ~loc type_ in
        td_type ~loc name t |> Option.some
    | _ -> None

  let stri_td ~loc td =
    let decl = { td with ptype_attributes = [ a_deriving_sexp ~loc ] } in
    Ast_helper.Str.type_ ~loc Recursive [ decl ]

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

  let t_visible_field ~ctx ~loc = function
    | Field { type_; _ } -> t_field_type ~ctx ~loc type_
    | Field_list { type_; _ } | Field_list_simple { type_; _ } ->
        t_list_type ~ctx ~loc type_
    | Field_variant { variant; _ } ->
        t_module_ident ~suffix:"union" ~ctx ~loc variant "t"
    | Field_optional { type_; _ } ->
        [%type: [%t t_field_type ~ctx ~loc type_] option]
    | ( Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
      | Field_optional_mask _ ) as f ->
        Format.ksprintf unexpected "field is not visible:\n%s" (show_field f)

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

  let td_record ?prefix ?suffix ~ctx ~loc name fields =
    match t_fields ~ctx ~loc fields with
    | `Type typ -> td_type ?prefix ?suffix ~loc name typ
    | `Label_declarations fields ->
        let name = Ident.snake ?prefix ?suffix name |> with_loc ~loc in
        Ast_helper.Type.mk ~loc ~kind:(Ptype_record fields) name

  let stri_record ?prefix ?suffix ~ctx ~loc name fields =
    td_record ?prefix ?suffix ~ctx ~loc name fields |> stri_td ~loc

  let stri_struct ~ctx ~loc = function
    | Struct { name; fields; _ } ->
        stri_record ~ctx ~loc name fields |> Option.some
    | Request { name; reply = Some fields; _ } ->
        stri_record ~suffix:"reply" ~ctx ~loc name fields |> Option.some
    | _ -> None

  let stri_module ?suffix ~loc name body =
    let name = Ident.caml ?suffix name in
    Ast_helper.Str.module_ ~loc
      (Ast_helper.Mb.mk ~loc
         (Some name |> with_loc ~loc)
         (Ast_helper.Mod.structure ~loc body))

  let rf_enum_item ~loc (name, _) =
    let name = Ident.caml name |> with_loc ~loc in
    Ast_helper.Rf.mk ~loc (Rtag (name, true, []))

  let t_enum_items ~loc items =
    Ast_helper.Typ.variant ~loc (List.map (rf_enum_item ~loc) items) Closed None

  let td_enum ~loc = function
    | Enum { name; items } ->
        td_type ~suffix:"enum" ~loc name (t_enum_items ~loc items)
        |> Option.some
    | _ -> None

  let stri_enum ~loc = function
    | Enum { name; items } ->
        let body = td_type ~loc "t" (t_enum_items ~loc items) in
        stri_module ~loc ~suffix:"enum" name [ stri_td ~loc body ]
        |> Option.some
    | _ -> None

  let stri_mask ~loc = function
    | Mask { name; items; additional_values = Additional_values values } ->
        let items =
          ListLabels.map items ~f:(fun (name, value) ->
              [%stri
                let [%p p_id ~loc name] : t =
                  of_int64 (Int64.shift_right 1L [%e e_int ~loc value])])
        in
        let values =
          ListLabels.map values ~f:(fun (name, value) ->
              [%stri
                let [%p p_id ~loc name] : t =
                  of_int64 [%e e_int64 ~suffix:'L' ~loc value]])
        in
        stri_module ~loc ~suffix:"mask" name
          (([%stri include Mask] :: items) @ values)
        |> Option.some
    | _ -> None

  let stri_decl ~ctx ~loc decl =
    let type_decl =
      td_type_declaration ~ctx ~loc decl |> Option.map (stri_td ~loc)
    in
    let record_decl = stri_struct ~ctx ~loc decl in
    let enum_decl = stri_enum ~loc decl in
    let mask_decl = stri_mask ~loc decl in
    List.filter_map Fun.id [ type_decl; record_decl; enum_decl; mask_decl ]

  let stri_event ~ctx ~loc = function
    | Event { name; fields; number; _ } ->
        let t = stri_record ~ctx ~loc "t" fields in
        let name' = [%stri let name = [%e e_str ~loc name]] in
        let number = [%stri let number = [%e e_int ~loc number]] in
        stri_module ~loc name [ t; name'; number ] |> Option.some
    | _ -> None

  let rf_event_type ~ctx:(Cm current_module as ctx) ~loc = function
    | Event { name; fields = _; number = _; _ } ->
        let typ =
          t_module_ident ~ctx ~loc
            { id_module = current_module; id_name = name }
            "t"
        in
        let name = Ident.caml name |> with_loc ~loc in
        Ast_helper.Rf.mk ~loc (Rtag (name, true, [ typ ])) |> Option.some
    | _ -> None

  let stri_events ~ctx ~loc decls =
    let t =
      let events = List.filter_map (rf_event_type ~ctx ~loc) decls in
      let t = Ast_helper.Typ.variant ~loc events Closed None in
      [%stri type t = [%t t] [@@deriving sexp]]
    in
    let events = List.filter_map (stri_event ~ctx ~loc) decls in
    stri_module ~loc "event" (events @ [ t ])

  let stri_module ~loc module_ =
    let declarations, ctx =
      match module_ with
      | Core decls -> (decls, "xproto")
      | Extension { declarations; file_name; _ } -> (declarations, file_name)
    in
    let ctx = Cm ctx in
    let decls = List.concat_map (stri_decl ~ctx ~loc) declarations in
    let events = stri_events ~ctx ~loc declarations in
    let body = decls @ [ events ] in
    match module_ with
    | Core _declarations -> body
    | Extension
        {
          name;
          query_name;
          version = major, minor;
          _;
          (* file_name; *)
          (* multiword; *)
          (* imports; *)
          (* declarations; *)
        } ->
        let name_t =
          let name = with_loc ~loc name in
          let name = Ast_helper.Rf.mk ~loc (Rtag (name, true, [])) in
          Ast_helper.Typ.variant ~loc [ name ] Closed None
        in
        [%str
          module Meta = struct
            type t = [%t name_t]

            let version = ([%e e_int major], [%e e_int minor])
            let query_name = [%e e_str ~loc query_name]
          end]
        @ body
end
