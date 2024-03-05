(** Resolve identifiers to the module they belong. *)

(** The declarations have a few separate namespaces:
    - types (includes typedefs, XIDs, XID unions, structs, event structs, and
      unions)
    - enums
    - events and generic events
    - errors
    - requests

    Name clashes within these namespaces are allowed between different modules.
    To disambiguate in the case that a declaration references a
    name exported by two or more modules currently in scope, the modules's
    [file_name] (in {!constructor:Parsetree.xcb.Extension}) is prefixed to the
    name with a colon, such as [xproto:PIXMAP].

    Some modules don't follow this rule, in which case I assumed that the names
    defined in the current module take precedence over the rest.
    
    In practice, this module turns all idents whose id_module is None into Some.
    It would be better to rebuild the AST into one where ident is not an option,
    but I value my sanity more (and we can do that in a later pass where we
    {i really} need to change the AST anyway). There's an expect test for this
    anyway. *)

open Parsetree

let find_module_by_name xcbs name =
  List.find_map
    (function
      | Core decls when name = "xproto" -> Some (decls, [])
      | Extension { file_name; declarations; _ } when file_name = name ->
          let imports =
            List.filter_map
              (function Import i -> Some i | _ -> None)
              declarations
          in
          Some (declarations, imports)
      | _ -> None)
    xcbs
  |> Option.get

type id_kind = [ `Type | `Enum | `Event | `Error ] [@@deriving show, sexp]

let id_exists (kind : id_kind) id_name = function
  | Xid name
  | Xid_union { name; _ }
  | Typedef { name; _ }
  | Struct { name; _ }
  | Union { name; _ }
  | Event_struct { name; _ }
    when name = id_name && kind = `Type ->
      true
  | Enum { name; _ } when name = id_name && kind = `Enum -> true
  | (Event { name; _ } | Event_copy { name; _ })
    when name = id_name && kind = `Event ->
      true
  | (Error { name; _ } | Error_copy { name; _ })
    when name = id_name && kind = `Error ->
      true
  | _ -> false

let resolve_ident (current_module_name, xcbs) (kind : id_kind) = function
  | { id_module = Some _; _ } as id -> id
  | { id_module = None; id_name } ->
      let current_module_declarations, imports =
        find_module_by_name xcbs current_module_name
      in
      let module_ =
        if List.exists (id_exists kind id_name) current_module_declarations then
          current_module_name
        else
          List.find
            (fun module_ ->
              let decls, _ = find_module_by_name xcbs module_ in
              List.exists (id_exists kind id_name) decls)
            (List.rev imports)
      in
      { id_module = Some module_; id_name }

let resolve_in_type ctx = function
  | Type_primitive _ as p -> p
  | Type_ref id -> Type_ref (resolve_ident ctx `Type id)

let rec resolve_in_expression ctx = function
  | Binop (op, e1, e2) ->
      Binop (op, resolve_in_expression ctx e1, resolve_in_expression ctx e2)
  | Unop (op, e) -> Unop (op, resolve_in_expression ctx e)
  | Param_ref { param; type_ } ->
      Param_ref { param; type_ = resolve_in_type ctx type_ }
  | Enum_ref { item; enum } ->
      Enum_ref { item; enum = resolve_ident ctx `Enum enum }
  | Pop_count e -> Pop_count (resolve_in_expression ctx e)
  | Sum_of { field; by_expr = Some e } ->
      Sum_of { field; by_expr = Some (resolve_in_expression ctx e) }
  | Sum_of { by_expr = None; _ } as f -> f
  | List_element_ref as f -> f
  | Field_ref _ as f -> f
  | Expr_value _ as f -> f
  | Expr_bit _ as f -> f

let resolve_in_field_allowed ctx = function
  | Allowed_enum id -> Allowed_enum (resolve_ident ctx `Enum id)
  | Allowed_mask id -> Allowed_mask (resolve_ident ctx `Enum id)
  | Allowed_alt_enum id -> Allowed_alt_enum (resolve_ident ctx `Enum id)
  | Allowed_alt_mask id -> Allowed_alt_mask (resolve_ident ctx `Enum id)

let resolve_in_field_type ctx { ft_type; ft_allowed } =
  {
    ft_type = resolve_in_type ctx ft_type;
    ft_allowed = Option.map (resolve_in_field_allowed ctx) ft_allowed;
  }

let resolve_in_switch_cond ctx = function
  | Cond_bit_and e -> Cond_bit_and (resolve_in_expression ctx e)
  | Cond_eq e -> Cond_eq (resolve_in_expression ctx e)

let rec resolve_in_switch ctx { sw_name; sw_cond; sw_cases } =
  {
    sw_name;
    sw_cond = resolve_in_switch_cond ctx sw_cond;
    sw_cases = List.map (resolve_in_case ctx) sw_cases;
  }

and resolve_in_case ctx { cs_name; cs_cond; cs_fields } =
  {
    cs_name;
    cs_cond = List.map (resolve_in_expression ctx) cs_cond;
    cs_fields = List.map (resolve_in_field ctx) cs_fields;
  }

and resolve_in_field ctx = function
  | Field_expr { name; type_; expr } ->
      Field_expr
        {
          name;
          type_ = resolve_in_field_type ctx type_;
          expr = resolve_in_expression ctx expr;
        }
  | Field_list { name; type_; length } ->
      Field_list
        {
          name;
          type_ = resolve_in_field_type ctx type_;
          length = Option.map (resolve_in_expression ctx) length;
        }
  | Field_file_descriptor _ as f -> f
  | Field_pad _ as f -> f
  | Field_switch sw -> Field_switch (resolve_in_switch ctx sw)
  | Field { name; type_ } ->
      Field { name; type_ = resolve_in_field_type ctx type_ }

let resolve_in_request_reply ctx { fields; doc } =
  { fields = List.map (resolve_in_field ctx) fields; doc }

let resolve_in_declaration ctx = function
  | Import _ as d -> d
  | Xid _ as d -> d
  | Xid_union { name; types } ->
      Xid_union { name; types = List.map (resolve_ident ctx `Type) types }
  | Typedef { name; type_ } ->
      Typedef { name; type_ = resolve_in_type ctx type_ }
  | Event_copy { name; event; ev_number } ->
      Event_copy { name; ev_number; event = resolve_ident ctx `Event event }
  | Error_copy { name; error; er_number } ->
      Error_copy { name; er_number; error = resolve_ident ctx `Error error }
  | Enum _ as d -> d
  | Event_struct _ as d -> d
  | Union { name; members } ->
      Union { name; members = List.map (resolve_in_field ctx) members }
  | Event ({ fields; _ } as ev) ->
      Event { ev with fields = List.map (resolve_in_field ctx) fields }
  | Error { name; number; fields } ->
      Error { name; number; fields = List.map (resolve_in_field ctx) fields }
  | Struct { name; fields } ->
      Struct { name; fields = List.map (resolve_in_field ctx) fields }
  | Request ({ fields; reply; _ } as req) ->
      Request
        {
          req with
          fields = List.map (resolve_in_field ctx) fields;
          reply = Option.map (resolve_in_request_reply ctx) reply;
        }

let resolve_in_xcb xcbs = function
  | Core decls ->
      Core (List.map (resolve_in_declaration ("xproto", xcbs)) decls)
  | Extension ({ file_name; declarations; _ } as ext) ->
      Extension
        {
          ext with
          declarations =
            List.map (resolve_in_declaration (file_name, xcbs)) declarations;
        }

let resolve xcbs = List.map (resolve_in_xcb xcbs) xcbs
