(**
- Find switches with bit cases and just turn them into optional fields.
  Requires two additional types of fields:
  - hidden field for the mask
  - "Optional field"

- Find switches with eq cases and turn the corresponding enum into
  a variant.
  Requires two additional types of fields:
  - hidden field for the discriminant value
  - variant field
  Also needs a new type of declaration
*)

let conv_ident Parsetree.{ id_module; id_name } =
  Elaboratetree.{ id_module = Option.get id_module; id_name }

let conv_type = function
  | Parsetree.Type_primitive prim -> Elaboratetree.Type_primitive prim
  | Type_ref id -> Elaboratetree.Type_ref (conv_ident id)

let rec conv_expression = function
  | Parsetree.Binop (op, e1, e2) ->
      Elaboratetree.Binop (op, conv_expression e1, conv_expression e2)
  | Unop (op, e) -> Elaboratetree.Unop (op, conv_expression e)
  | Field_ref f -> Elaboratetree.Field_ref f
  | Param_ref { param; type_ } ->
      Elaboratetree.Param_ref { param; type_ = conv_type type_ }
  | Enum_ref { enum; item } ->
      Elaboratetree.Enum_ref { enum = conv_ident enum; item }
  | Pop_count e -> Elaboratetree.Pop_count (conv_expression e)
  | Sum_of { field; by_expr } ->
      Elaboratetree.Sum_of
        { field; by_expr = Option.map conv_expression by_expr }
  | List_element_ref -> Elaboratetree.List_element_ref
  | Expr_value n -> Elaboratetree.Expr_value n
  | Expr_bit b -> Elaboratetree.Expr_bit b

let conv_field_allowed = function
  | Parsetree.Allowed_enum id -> Elaboratetree.Allowed_enum (conv_ident id)
  | Allowed_mask id -> Elaboratetree.Allowed_mask (conv_ident id)
  | Allowed_alt_enum id -> Elaboratetree.Allowed_alt_enum (conv_ident id)
  | Allowed_alt_mask id -> Elaboratetree.Allowed_alt_mask (conv_ident id)

let conv_field_type Parsetree.{ ft_type; ft_allowed } =
  Elaboratetree.
    {
      ft_type = conv_type ft_type;
      ft_allowed = Option.map conv_field_allowed ft_allowed;
    }

let mask_switches_to_optional_fields' _ctx = function
  | Parsetree.Field_switch
      { sw_cond = Cond_bit_and (Field_ref cond_field); sw_name; sw_cases } ->
      assert (
        List.for_all
          (fun Parsetree.{ cs_cond; _ } -> List.length cs_cond = 1)
          sw_cases );
      ignore cond_field;
      ignore sw_name;
      ignore sw_cases;
      ()
  | Field_switch { sw_cond = Cond_eq (Field_ref cond_field); sw_name; sw_cases }
    ->
      assert (
        List.for_all
          (fun Parsetree.{ cs_cond; _ } -> List.length cs_cond = 1)
          sw_cases );
      ignore cond_field;
      ignore sw_name;
      ignore sw_cases;
      ()
  | Field_switch { sw_cond = Cond_bit_and e; _ } ->
      Printf.kprintf failwith "invalid bit switch cond: %a"
        (fun () -> Parsetree.show_expression)
        e
  | Field_switch { sw_cond = Cond_eq e; _ } ->
      Printf.kprintf failwith "invalid eq switch cond: %a"
        (fun () -> Parsetree.show_expression)
        e
  | _ -> ()

(* let ( let* ) = Option.bind *)

(* let mask_switches_to_optional_fields fields =
  let* switch =
    List.find_opt
      (function
      | Parsetree.Field_switch
          { sw_cond = Cond_bit_and (Field_ref cond_field); sw_name; sw_cases } ->
        
  | Parsetree.Field_switch { sw_cond = Cond_bit_and e; _ } ->
      Printf.kprintf failwith "invalid bit switch cond: %a"
        (fun () -> Parsetree.show_expression)
        e
        | f -> f
      )
      fields
  in *)

(* let masks xcbs = *)

let in_declaration ctx = function
  | Parsetree.Union { members = fields; _ }
  | Event { fields; _ }
  | Error { fields; _ }
  | Struct { fields; _ } ->
      List.iter (mask_switches_to_optional_fields' ctx) fields
  | Request { fields; reply; _ } ->
      List.iter (mask_switches_to_optional_fields' ctx) fields;
      Option.iter
        (fun Parsetree.{ fields; _ } ->
          List.iter (mask_switches_to_optional_fields' ctx) fields)
        reply
  | _ -> ()

let in_xcb = function
  | Parsetree.Core declarations ->
      List.iter (in_declaration "xproto") declarations
  | Extension { declarations; file_name; _ } ->
      List.iter (in_declaration file_name) declarations
