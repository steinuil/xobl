open Ppxlib
open Xobl_compiler.Hir
module Ident = Casing.OCaml
open Ast_helper

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

let prim_of_type_exn = function
  | Type_primitive prim | Type_ref (_, Some prim) -> prim
  | Type_union _ -> Xid
  | Type_ref (ident, None) ->
      Format.kasprintf unexpected "not a primitive: %a" Sexplib.Sexp.pp_hum
        (sexp_of_ident ident)

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
let xproto_to_core = function "xproto" -> "core" | name -> name

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
      let parent = Ident.caml (xproto_to_core id_module) in
      Ldot (Ldot (Lident parent, module_name), name)
  in
  with_loc ~loc txt

let e_id ?prefix ?suffix ?parent ~loc name =
  let ident = lid ?prefix ?suffix ?parent ~loc name in
  Ast_helper.Exp.ident ~loc ident

let e_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc { id_module; id_name }
    =
  if current_module = id_module then e_id ?prefix ?suffix ~loc id_name
  else
    e_id ?prefix ?suffix
      ~parent:(Ident.caml (xproto_to_core id_module))
      ~loc id_name

let t_id ?prefix ?suffix ?parent ~loc name =
  let typ = lid ?prefix ?suffix ?parent ~loc name in
  Ast_helper.Typ.constr ~loc typ []

let t_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc { id_module; id_name }
    =
  if current_module = id_module then t_id ?prefix ?suffix ~loc id_name
  else
    t_id ?prefix ?suffix
      ~parent:(Ident.caml (xproto_to_core id_module))
      ~loc id_name

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

let stri_module ?suffix ~loc name body =
  let name = Ident.caml ?suffix name in
  Ast_helper.Str.module_ ~loc
    (Ast_helper.Mb.mk ~loc
       (Some name |> with_loc ~loc)
       (Ast_helper.Mod.structure ~loc body))

module Protocol = struct
  let a_deriving_sexp ~loc =
    Ast_helper.Attr.mk (with_loc ~loc "deriving") (PStr [%str sexp_of])

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
    | Field_variant { variant; _ } -> t_module_ident ~ctx ~loc variant "t"
    | Field_optional { type_; _ } ->
        let t = t_field_type ~ctx ~loc type_ in
        [%type: [%t t] option]
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
    | _ -> None

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
        let t = td_type ~loc "t" (t_enum_items ~loc items) in
        let of_int =
          let cases =
            ListLabels.map items ~f:(fun (name, value) ->
                Ast_helper.Exp.case
                  (p_int ~loc (Int64.to_int value))
                  (e_variant ~loc name))
          in
          let error_msg =
            Printf.ksprintf (e_str ~loc) "Invalid enum value for %s: " name
          in
          let default =
            Exp.case
              [%pat? n]
              [%expr invalid_arg ([%e error_msg] ^ string_of_int n)]
          in
          let body = Exp.function_ ~loc (cases @ [ default ]) in
          [%stri let of_int : int -> [> t ] = [%e body]]
        in
        let to_int =
          let cases =
            ListLabels.map items ~f:(fun (name, value) ->
                Exp.case
                  (Pat.variant (Ident.caml name) None)
                  (e_int ~loc (Int64.to_int value)))
          in
          let body = Exp.function_ ~loc cases in
          [%stri let to_int : [< t ] -> int = [%e body]]
        in
        stri_module ~loc ~suffix:"enum" name [ stri_td ~loc t; of_int; to_int ]
        |> Option.some
    | _ -> None

  let stri_mask ~loc = function
    | Mask { name; items; additional_values } ->
        let items =
          ListLabels.map items ~f:(fun (name, value) ->
              [%stri
                let [%p p_id ~loc name] : t =
                  of_int32 (bit [%e e_int ~loc value])])
        in
        let values =
          match additional_values with
          | Additional_values values ->
              ListLabels.map values ~f:(fun (name, value) ->
                  [%stri
                    let [%p p_id ~loc name] : t =
                      of_int32
                        (Optint.of_int [%e e_int ~loc (Int64.to_int value)])])
          | None_value -> [%str let none : t = of_int32 Optint.zero]
        in
        stri_module ~loc ~suffix:"mask" name
          (([%stri include Mask_impl ()] :: items) @ values)
        |> Option.some
    | _ -> None

  let rf_variant_item ~ctx ~loc { vi_name; vi_fields; _ } =
    (* The Property_ is a little special case. *)
    let name = Ident.caml ~sanitize:"Property_" vi_name |> with_loc ~loc in
    let args, type_ =
      match t_fields ~ctx ~loc vi_fields with
      | `Type typ -> (typ, None)
      | `Label_declarations _fields ->
          let name = Ident.snake ~sanitize:"property_" vi_name in
          let type_ = td_record ~ctx ~loc name vi_fields in
          let name =
            let lid = Lident name |> with_loc ~loc in
            Ast_helper.Typ.constr ~loc lid []
          in
          (name, Some (stri_td ~loc type_))
    in
    (Ast_helper.Rf.mk ~loc (Rtag (name, true, [ args ])), type_)

  let str_variant_t ~ctx ~loc items =
    let items, types =
      List.map (rf_variant_item ~ctx ~loc) items |> List.split
    in
    let types = List.filter_map Fun.id types in
    let variant =
      Ast_helper.Typ.variant ~loc items Closed None
      |> td_type ~loc "t" |> stri_td ~loc
    in
    types @ [ variant ]

  let stri_variant ~ctx ~loc = function
    | Variant { name; items; _ } ->
        stri_module ~loc name (str_variant_t ~ctx ~loc items) |> Option.some
    | _ -> None

  let stri_decl ~ctx ~loc decl =
    let type_decl =
      td_type_declaration ~ctx ~loc decl |> Option.map (stri_td ~loc)
    in
    let record_decl = stri_struct ~ctx ~loc decl in
    let enum_decl = stri_enum ~loc decl in
    let mask_decl = stri_mask ~loc decl in
    let variant_decl = stri_variant ~ctx ~loc decl in
    List.filter_map Fun.id
      [ type_decl; record_decl; enum_decl; mask_decl; variant_decl ]

  let stri_event ~ctx:(Cm current_module as ctx) ~loc = function
    | Event { name; fields; number; _ } ->
        let t = stri_record ~ctx ~loc "t" fields in
        let name' = [%stri let name = [%e e_str ~loc name]] in
        let number = [%stri let number = [%e e_int ~loc number]] in
        stri_module ~loc name [ t; name'; number ] |> Option.some
    | Event_copy { name; event; number; _ } ->
        let event_t =
          if event.id_module = current_module then
            t_ident ~ctx ~loc { id_module = event.id_name; id_name = "t" }
          else
            let id_module = Ident.caml event.id_module in
            let ev = Ident.caml event.id_name in
            let lid =
              Ldot (Ldot (Ldot (Lident id_module, "Event"), ev), "t")
              |> with_loc ~loc
            in
            Ast_helper.Typ.constr ~loc lid []
        in
        let t = [%stri type t = [%t event_t] [@@deriving sexp_of]] in
        let name' = [%stri let name = [%e e_str ~loc name]] in
        let number = [%stri let number = [%e e_int ~loc number]] in
        stri_module ~loc name [ t; name'; number ] |> Option.some
    | _ -> None

  let rf_event_type ~ctx:(Cm current_module as ctx) ~loc = function
    | Event { name; _ } | Event_copy { name; _ } ->
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
      [%stri type t = [%t t] [@@deriving sexp_of]]
    in
    let events = List.filter_map (stri_event ~ctx ~loc) decls in
    stri_module ~loc "event" (events @ [ t ])

  let stri_error ~ctx:(Cm current_module as ctx) ~loc = function
    | Error { name; fields; number } ->
        let t = stri_record ~ctx ~loc "t" fields in
        let name' = [%stri let name = [%e e_str ~loc name]] in
        let number = [%stri let number = [%e e_int ~loc number]] in
        stri_module ~loc name [ t; name'; number ] |> Option.some
    | Error_copy { name; error; number; _ } ->
        let error_t =
          if error.id_module = current_module then
            t_ident ~ctx ~loc { id_module = error.id_name; id_name = "t" }
          else
            let id_module = Ident.caml (xproto_to_core error.id_module) in
            let err = Ident.caml error.id_name in
            let lid =
              Ldot (Ldot (Ldot (Lident id_module, "Error"), err), "t")
              |> with_loc ~loc
            in
            Ast_helper.Typ.constr ~loc lid []
        in
        let t = [%stri type t = [%t error_t] [@@deriving sexp_of]] in
        let name' = [%stri let name = [%e e_str ~loc name]] in
        let number = [%stri let number = [%e e_int ~loc number]] in
        stri_module ~loc name [ t; name'; number ] |> Option.some
    | _ -> None

  let rf_error_type ~ctx:(Cm current_module as ctx) ~loc = function
    | Error { name; _ } | Error_copy { name; _ } ->
        let typ =
          t_module_ident ~ctx ~loc
            { id_module = current_module; id_name = name }
            "t"
        in
        let name = Ident.caml name |> with_loc ~loc in
        Ast_helper.Rf.mk ~loc (Rtag (name, true, [ typ ])) |> Option.some
    | _ -> None

  let stri_errors ~ctx ~loc decls =
    let t =
      let errors = List.filter_map (rf_error_type ~ctx ~loc) decls in
      let t = Ast_helper.Typ.variant ~loc errors Closed None in
      [%stri type t = [%t t] [@@deriving sexp_of]]
    in
    let errors = List.filter_map (stri_error ~ctx ~loc) decls in
    stri_module ~loc "error" (errors @ [ t ])

  let rf_event_struct_event ~ctx:(Cm current_module) ~loc ev_id =
    let name = Ident.caml ev_id.id_name |> with_loc ~loc in
    let ev = Ident.caml ev_id.id_name in
    let typ =
      let lid =
        if ev_id.id_module = current_module then
          Ldot (Ldot (Lident "Event", ev), "t")
        else
          let id_module = Ident.caml (xproto_to_core ev_id.id_module) in
          Ldot (Ldot (Ldot (Lident id_module, "Event"), ev), "t")
      in
      Ast_helper.Typ.constr ~loc (with_loc ~loc lid) []
    in
    Ast_helper.Rf.mk ~loc (Rtag (name, true, [ typ ]))

  let stri_event_struct ~ctx ~loc = function
    | Event_struct { name; events } ->
        let events = List.map (rf_event_struct_event ~ctx ~loc) events in
        let t =
          Ast_helper.Typ.variant ~loc events Closed None
          |> td_type ~loc "t" |> stri_td ~loc
        in
        stri_module ~loc ~suffix:"enum" name [ t ] |> Option.some
    | _ -> None

  let str_event_structs ~ctx ~loc decls =
    List.filter_map (stri_event_struct ~ctx ~loc) decls

  let al_field = function
    | Field { name; _ }
    | Field_list { name; _ }
    | Field_list_simple { name; _ }
    | Field_variant { name; _ } ->
        let name = Ident.snake name in
        Labelled name
    | Field_optional { name; _ } ->
        let name = Ident.snake name in
        Optional name
    | ( Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
      | Field_optional_mask _ ) as f ->
        Format.ksprintf unexpected "field is not visible:\n%s" (show_field f)

  let e_make_request ~loc fields =
    match visible_fields fields with
    | [] -> [%expr fun f () -> f (() : t)]
    | [ field ] ->
        let name = name_of_field field |> Option.get |> Ident.snake in
        [%expr fun f [%p p_id ~loc name] -> f ([%e e_id ~loc name] : t)]
    | fields ->
        let body =
          let fields =
            List.map
              (fun field ->
                let name = name_of_field field |> Option.get in
                (lid ~loc name, e_id ~loc name))
              fields
          in
          let record = Ast_helper.Exp.record ~loc fields None in
          [%expr f ([%e record] : t)]
        in
        let init =
          let no_optional_fields =
            List.for_all
              (function Field_optional _ -> false | _ -> true)
              fields
          in
          if no_optional_fields then body else [%expr fun () -> [%e body]]
        in
        let make =
          ListLabels.fold_right fields ~init ~f:(fun field expr ->
              let name = name_of_field field |> Option.get |> Ident.snake in
              let arg = al_field field in
              Ast_helper.Exp.fun_ ~loc arg None (p_id ~loc name) expr)
        in
        [%expr fun f -> [%e make]]

  let stri_request ~ctx ~loc = function
    | Request { name; fields; reply; opcode; _ } ->
        let request = stri_record ~ctx ~loc "t" fields in
        let make = e_make_request ~loc fields in
        let reply =
          match reply with
          | None -> []
          | Some reply ->
              [%str
                module Reply = struct
                  [%%i stri_record ~ctx ~loc "t" reply]
                end]
        in
        let body =
          [%str
            let name = [%e e_str ~loc name]
            let opcode = [%e e_int ~loc opcode]

            [%%i request]

            let with_request = [%e make]]
          @ reply
        in
        stri_module ~loc name body |> Option.some
    | _ -> None

  let str_requests ~ctx ~loc decls =
    List.filter_map (stri_request ~ctx ~loc) decls

  let stri_protocol ~loc module_ =
    let declarations, ctx =
      match module_ with
      | Core decls -> (decls, "xproto")
      | Extension { declarations; file_name; _ } -> (declarations, file_name)
    in
    let ctx = Cm ctx in
    let decls = List.concat_map (stri_decl ~ctx ~loc) declarations in
    let events = stri_events ~ctx ~loc declarations in
    let errors = stri_errors ~ctx ~loc declarations in
    let event_structs = str_event_structs ~ctx ~loc declarations in
    let requests = List.filter_map (stri_request ~ctx ~loc) declarations in
    let header = [%str] in
    let body = decls @ [ events ] @ event_structs @ [ errors ] @ requests in
    match module_ with
    | Core _declarations -> header @ body
    | Extension { name; query_name; version = major, minor; _ } ->
        let name_t =
          let name = with_loc ~loc name in
          let name = Ast_helper.Rf.mk ~loc (Rtag (name, true, [])) in
          Ast_helper.Typ.variant ~loc [ name ] Closed None
        in
        let meta =
          [%str
            module Meta = struct
              type t = [%t name_t]

              let version = ([%e e_int major], [%e e_int minor])
              let query_name = [%e e_str ~loc query_name]
            end]
        in
        header @ meta @ body

  let stri_protocols ~loc protocols =
    let protocols =
      protocols
      |> List.map (fun protocol ->
             let name =
               match protocol with
               | Xobl_compiler.Hir.Core _ -> "Core"
               | Extension { file_name; _ } ->
                   file_name |> String.capitalize_ascii
             in
             let body = stri_protocol ~loc protocol in
             stri_module ~loc name body)
    in
    [%str
      [@@@ocamlformat "disable"]
      [@@@ocaml.warning "-12"]
      [@@@ocaml.warning "-73"]
      [@@@ocaml.warning "-11"]

      open Types
      open Util
      open Sexplib.Conv]
    @ protocols
end

module Codecs = struct
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

  let e_prim_to_int ~loc = function
    | Bool -> [%expr Conv.To_int.bool]
    | Int8 -> [%expr Conv.To_int.i8]
    | Card8 -> [%expr Conv.To_int.u8]
    | Int16 -> [%expr Conv.To_int.i16]
    | Card16 -> [%expr Conv.To_int.u16]
    | Int32 -> [%expr Conv.To_int.i32]
    | Card32 -> [%expr Conv.To_int.u32]
    | Card64 -> [%expr Conv.To_int.u64]
    | Void -> [%expr Conv.To_int.void]
    | Char -> [%expr Conv.To_int.char]
    | Byte -> [%expr Conv.To_int.byte]
    | Float -> [%expr Conv.To_int.float]
    | Double -> [%expr Conv.To_int.double]
    | Fd -> [%expr Conv.To_int.file_descr]
    | Xid -> [%expr Conv.To_int.xid]

  let e_prim_to_i32 ~loc = function
    | Bool -> [%expr Conv.To_i32.bool]
    | Int8 -> [%expr Conv.To_i32.i8]
    | Card8 -> [%expr Conv.To_i32.u8]
    | Int16 -> [%expr Conv.To_i32.i16]
    | Card16 -> [%expr Conv.To_i32.u16]
    | Int32 -> [%expr Conv.To_i32.i32]
    | Card32 -> [%expr Conv.To_i32.u32]
    | Card64 -> [%expr Conv.To_i32.u64]
    | Void -> [%expr Conv.To_i32.void]
    | Char -> [%expr Conv.To_i32.char]
    | Byte -> [%expr Conv.To_i32.byte]
    | Float -> [%expr Conv.To_i32.float]
    | Double -> [%expr Conv.To_i32.double]
    | Fd -> [%expr Conv.To_i32.file_descr]
    | Xid -> [%expr Conv.To_i32.xid]

  let e_ident ?prefix ?suffix ~ctx:(Cm current_module) ~loc
      { id_module; id_name } =
    if current_module = id_module then e_id ?prefix ?suffix ~loc id_name
    else
      e_id ?prefix ?suffix
        ~parent:(Ident.caml (xproto_to_core id_module) ^ "_codec")
        ~loc id_name

  let e_type ~ctx ~loc = function
    | Type_primitive prim | Type_ref (_, Some prim) -> e_prim ~loc prim
    | Type_union _ -> e_prim ~loc Xid
    | Type_ref (ident, _) -> e_ident ~prefix:"decode" ~ctx ~loc ident

  let e_field_type ~ctx:(Cm current_module as ctx) ~loc { ft_type; ft_allowed }
      =
    let decode_t = e_type ~ctx ~loc ft_type in
    match ft_allowed with
    | None -> decode_t
    | Some (Allowed_enum enum) ->
        let int_of_t = prim_of_type_exn ft_type |> e_prim_to_int ~loc in
        let enum_of_int =
          let id =
            if current_module = enum.id_module then
              Ldot (Lident (Ident.caml ~suffix:"enum" enum.id_name), "of_int")
            else
              Ldot
                ( Ldot
                    ( Lident (Ident.caml (xproto_to_core enum.id_module)),
                      Ident.caml ~suffix:"enum" enum.id_name ),
                  "of_int" )
          in
          id |> with_loc ~loc |> Exp.ident ~loc
        in
        [%expr [%e decode_t] %> [%e int_of_t] %> [%e enum_of_int]]
    | Some (Allowed_alt_enum enum) ->
        let int_of_t = prim_of_type_exn ft_type |> e_prim_to_int ~loc in
        let enum_of_int =
          let id =
            if current_module = enum.id_module then
              Ldot (Lident (Ident.caml ~suffix:"enum" enum.id_name), "of_int")
            else
              Ldot
                ( Ldot
                    ( Lident (Ident.caml (xproto_to_core enum.id_module)),
                      Ident.caml ~suffix:"enum" enum.id_name ),
                  "of_int" )
          in
          id |> with_loc ~loc |> Exp.ident ~loc
        in
        [%expr
          [%e decode_t]
          %> Conv.alt_enum ~enum_of_int:[%e enum_of_int] ~int_of_t:[%e int_of_t]]
    | Some (Allowed_mask mask | Allowed_alt_mask mask) ->
        let i32_of_t = prim_of_type_exn ft_type |> e_prim_to_i32 ~loc in
        let mask_of_int32 =
          let id =
            if current_module = mask.id_module then
              Ldot (Lident (Ident.caml ~suffix:"mask" mask.id_name), "of_int32")
            else
              Ldot
                ( Ldot
                    ( Lident (Ident.caml (xproto_to_core mask.id_module)),
                      Ident.caml ~suffix:"mask" mask.id_name ),
                  "of_int32" )
          in
          id |> with_loc ~loc |> Exp.ident ~loc
        in
        [%expr [%e decode_t] %> [%e i32_of_t] %> [%e mask_of_int32]]

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

  let e_field ~ctx ~loc = function
    | Field { name; type_ } ->
        let body = e_field_type ~ctx ~loc type_ in
        [ `Let (name, [%expr [%e body] buf]) ]
    | Field_pad { pad = Pad_bytes n; _ } ->
        [ `Seq [%expr Decode.pad buf [%e e_int n]] ]
    | Field_pad { pad = Pad_align n; _ } ->
        [ `Seq [%expr Decode.pad buf [%e e_int n]] ]
    | Field_list_simple { name; type_; length } ->
        let decode_ls = e_list_type ~ctx ~loc type_ in
        let len = e_id ~loc length in
        [ `Let (name, [%expr [%e decode_ls] ~len:[%e len] buf]) ]
    | Field_list_length { name; type_; expr = None; _ } ->
        let body = e_type ~ctx ~loc type_ in
        let int_of_t = prim_of_type_exn type_ |> e_prim_to_int ~loc in
        [ `Let (name, [%expr [%e int_of_t] ([%e body] buf)]) ]
    | Field_list_length { name; type_; expr = Some expr; _ } ->
        let body = e_type ~ctx ~loc type_ in
        let int_of_t = prim_of_type_exn type_ |> e_prim_to_int ~loc in
        [
          `Let (name, [%expr [%e int_of_t] ([%e body] buf)]);
          `Let (name, e_expression ~loc expr);
        ]
    | Field_list { name; type_; length = Some length } ->
        let len = e_expression ~loc length in
        let body = e_list_type ~ctx ~loc type_ in
        [ `Let (name, [%expr [%e body] ~len:[%e len] buf]) ]
    | Field_variant_tag { field_name; variant = _; type_ } ->
        let body = e_type ~ctx ~loc type_ in
        [ `Let (field_name ^ "_tag", [%expr [%e body] buf]) ]
    | Field_variant { name; variant; external_params } -> (
        let body =
          e_ident ~prefix:"decode" ~suffix:"variant" ~ctx ~loc variant
        in
        let tag = e_id ~suffix:"tag" ~loc name in
        match external_params with
        | [] -> [ `Let (name, [%expr [%e body] ~tag:[%e tag] buf]) ]
        | [ param ] ->
            let param = e_id ~loc param.ep_name in
            [ `Let (name, [%expr [%e body] ~tag:[%e tag] buf [%e param]]) ]
        | _ -> not_implemented "multiple param_refs")
    | f -> Printf.ksprintf unexpected "field: %s" (show_field f)

  let e_result_fields_record ~loc fields =
    let fields = names_of_visible_fields fields in
    match fields with
    | [] -> [%expr ()]
    | [ name ] -> e_id ~loc name
    | _ ->
        let fields =
          List.map (fun name -> (lid ~loc name, e_id ~loc name)) fields
        in
        Ast_helper.Exp.record ~loc fields None

  let e_fields ~ctx ~loc fields result =
    let fields = List.concat_map (e_field ~ctx ~loc) fields in
    ListLabels.fold_right fields ~init:result ~f:(fun field expr ->
        match field with
        | `Let (name, body) ->
            let binding = vb ~loc name body in
            Ast_helper.Exp.let_ ~loc Nonrecursive [ binding ] expr
        | `Seq body -> Ast_helper.Exp.sequence ~loc body expr)

  let e_variant ~ctx ~loc name items external_params =
    let type_ = t_id ~parent:(Ident.caml name) ~loc "t" in
    let items =
      ListLabels.map items ~f:(fun { vi_name; vi_tag; vi_fields; _ } ->
          let result =
            Ast_helper.Exp.variant ~loc
              (Ident.caml ~sanitize:"Property_" vi_name)
              (Some (e_result_fields_record ~loc vi_fields))
          in
          Ast_helper.Exp.case
            (p_int ~loc (Int64.to_int vi_tag))
            (e_fields ~ctx ~loc vi_fields result))
    in
    let items =
      items
      @ [
          Ast_helper.Exp.case
            [%pat? n]
            [%expr invalid_arg ("Invalid enum value: " ^ string_of_int n)];
        ]
    in
    let body = Ast_helper.Exp.match_ ~loc (e_id ~loc "tag") items in
    match external_params with
    | [] -> [%expr fun buf ~tag : [%t type_] -> [%e body]]
    | [ param ] ->
        let param = p_id ~loc ~prefix:"ext" param.ep_name in
        [%expr fun buf ~tag [%p param] : [%t type_] -> [%e body]]
    | _ -> not_implemented "multiple param_refs"

  let stri_declaration ~ctx ~loc = function
    | Struct { name; fields; external_params = [] } ->
        let result = e_result_fields_record ~loc fields in
        let body = e_fields ~ctx ~loc fields result in
        let result_t = t_id ~loc name in
        [%stri
          let [%p p_id ~loc ~prefix:"decode" name] =
           fun buf : [%t result_t] -> [%e body]]
        :: []
    | Type_alias { name; type_ } when primitive_of_type type_ = None ->
        let body = e_type ~ctx ~loc type_ in
        [%stri let [%p p_id ~loc ~prefix:"decode" name] = [%e body]] :: []
    | Variant { name; items; external_params } ->
        let variant = e_variant ~ctx ~loc name items external_params in
        [%stri
          let [%p p_id ~loc ~prefix:"decode" ~suffix:"variant" name] =
            [%e variant]]
        :: []
    | _ -> []

  let stri_protocol ~loc proto =
    let declarations, ctx =
      match proto with
      | Core decls -> (decls, "xproto")
      | Extension { declarations; file_name; _ } -> (declarations, file_name)
    in
    let ctx = Cm ctx in
    List.concat_map (stri_declaration ~ctx ~loc) declarations

  let str_protocols ~loc protos =
    let protos =
      protos
      |> List.map (fun proto ->
             let name, t_name =
               match proto with
               | Core _ -> ("Core_codec", "Core")
               | Extension { file_name; _ } ->
                   ( String.capitalize_ascii file_name ^ "_codec",
                     String.capitalize_ascii file_name )
             in
             let body = stri_protocol ~loc proto in
             let open_ =
               Ldot (Lident "Protocol", String.capitalize_ascii t_name)
               |> with_loc ~loc |> Mod.ident |> Opn.mk |> Str.open_
             in
             stri_module ~loc name (open_ :: body))
    in
    [%str
      [@@@ocamlformat "disable"]
      [@@@ocaml.warning "-33"]

      open Util
      open Protocol
      (* [@@@ocaml.warning "-12"] *)
      (* [@@@ocaml.warning "-73"] *)
      (* [@@@ocaml.warning "-11"] *)]
    @ protos
end
