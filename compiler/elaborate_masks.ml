(**
- [x] Find switches with bit cases and just turn them into optional fields.
      Requires two additional types of fields:
      - hidden field for the mask
      - "Optional field"

- [x] Find switches with eq cases and turn the corresponding enum into
      a variant.
      Requires two additional types of fields:
      - hidden field for the discriminant value
      - variant field
      Also needs a new type of declaration


- [x] put imports into xcb
- figure out list length fields
- [x] add variants
- prune unused enums and masks
- reorder some declarations that have dependencies above them (there's like 2)
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

let find_module xcbs n =
  xcbs
  |> List.find_map (function
       | Parsetree.Core decls when n = "xproto" -> Some decls
       | Extension { file_name; declarations; _ } when file_name = n ->
           Some declarations
       | _ -> None)
  |> Option.get

let ( let* ) = Option.bind

let rec enum_switches_to_variants (curr_module, xcbs) fields =
  let variants =
    fields
    |> List.filter_map (function
         | Parsetree.Field_switch
             { sw_cond = Cond_eq (Field_ref cond_field); sw_name; sw_cases } ->
             let cond_enum =
               fields
               |> List.find_map (function
                    | Parsetree.Field
                        {
                          name;
                          type_ = { ft_allowed = Some (Allowed_enum enum); _ };
                        }
                      when name = cond_field ->
                        Some enum
                    | Field { name; _ } when name = cond_field ->
                        failwith "unexpected"
                    | _ -> None)
               |> Option.get
             in
             let cond_enum_items =
               find_module xcbs (Option.get cond_enum.id_module)
               |> List.find_map (function
                    | Parsetree.Enum { name; items; _ }
                      when name = cond_enum.id_name ->
                        Some items
                    | _ -> None)
               |> Option.get
             in
             let variant_items, additional_variants =
               sw_cases
               |> List.map (function
                    | Parsetree.
                        { cs_cond = [ Enum_ref { item; _ } ]; cs_fields; _ } ->
                        let vi_tag =
                          cond_enum_items
                          |> List.find_map (function
                               | name, Parsetree.Item_value v when name = item
                                 ->
                                   Some v
                               | _ -> None)
                          |> Option.get
                        in
                        let vi_fields, variants =
                          cs_fields
                          |> enum_switches_to_variants (curr_module, xcbs)
                        in
                        ( Elaboratetree.{ vi_name = item; vi_tag; vi_fields },
                          variants )
                    | _ -> failwith "lol, lmao")
               |> List.split
             in
             let variant_name = cond_enum.id_name in
             let additional_variants = List.flatten additional_variants in
             Some
               ( sw_name,
                 cond_field,
                 variant_name,
                 variant_items,
                 additional_variants )
         | Parsetree.Field_switch { sw_cond = Cond_eq _; _ } as a ->
             failwith (Parsetree.show_field a)
         | _ -> None)
  in
  let _ayy =
    List.filter_map
      (function
        | Parsetree.Field_list { length = Some e; _ } -> (
            let rec traverse = function
              | Parsetree.Binop (_, e1, e2) -> traverse e1 @ traverse e2
              | Unop (_, e) -> traverse e
              | Field_ref f -> [ f ]
              | Param_ref { param = _; _ } ->
                  (* There is a single case in which this happens
                     and of course it's inside xinput. *)
                  []
              | Enum_ref _ | Pop_count _ | List_element_ref ->
                  failwith "unreachable"
              | Sum_of { field = _; _ } ->
                  (* There are a few cases in xinput. *)
                  []
              | Expr_value _ | Expr_bit _ -> []
            in
            let x = traverse e in
            match x with [ f ] -> Some f | _ -> None )
        | _ -> None)
      fields
    |> List.map (function f ->
           ( match
               List.find_map
                 (function
                   | Parsetree.Field { name; _ } when name = f -> Some name
                   | _ -> None)
                 fields
             with
           | Some _ -> ()
           | None -> Printf.eprintf "%s %s\n" curr_module f ))
  in
  let optional_fields =
    fields
    |> List.filter_map (function
         | Parsetree.Field_switch
             {
               sw_cond = Cond_bit_and (Field_ref cond_field);
               sw_name;
               sw_cases;
             } ->
             let cond_mask =
               fields
               |> List.find_map (function
                    | Parsetree.Field
                        {
                          name;
                          type_ = { ft_allowed = Some (Allowed_mask mask); _ };
                        }
                      when name = cond_field ->
                        Some mask
                    | _ -> None)
               |> Option.get
             in
             let cond_mask_items =
               find_module xcbs (Option.get cond_mask.id_module)
               |> List.find_map (function
                    | Parsetree.Enum { name; items; _ }
                      when name = cond_mask.id_name ->
                        Some items
                    | _ -> None)
               |> Option.get
             in
             let mask_items =
               sw_cases
               |> List.map (function
                    | Parsetree.
                        {
                          cs_cond = [ Enum_ref { item; _ } ];
                          cs_fields = [ Field { type_; _ } ];
                          cs_name = _;
                        } ->
                        let bit =
                          cond_mask_items
                          |> List.find_map (function
                               | name, Parsetree.Item_bit b when name = item ->
                                   Some b
                               | _ -> None)
                          |> Option.get
                        in
                        let type_ = conv_field_type type_ in
                        Elaboratetree.Field_optional
                          { name = item; mask = cond_field; bit; type_ }
                    | case -> failwith (Parsetree.show_case case))
             in
             Some (sw_name, mask_items, cond_field)
         | _ -> None)
  in
  let fields, additional_variants =
    fields
    |> List.map (function
         | Parsetree.Field_switch { sw_cond = Cond_eq _; sw_name; _ } ->
             let name, _, variant_name, _, additional_variants =
               List.find (fun (name, _, _, _, _) -> name = sw_name) variants
             in

             ( Elaboratetree.
                 [
                   Field_variant
                     {
                       name;
                       variant =
                         { id_module = curr_module; id_name = variant_name };
                     };
                 ],
               additional_variants )
         | Field { name; type_ = { ft_type; _ } }
           when List.exists
                  (fun (_, cond_field, _, _, _) -> name = cond_field)
                  variants ->
             let field_name, _, _, _, _ =
               List.find
                 (fun (_, cond_field, _, _, _) -> name = cond_field)
                 variants
             in
             ( [
                 Elaboratetree.Field_variant_tag
                   { variant = field_name; type_ = conv_type ft_type };
               ],
               [] )
         | Field_switch { sw_cond = Cond_bit_and _; sw_name; _ } ->
             let _, optional_fields, _ =
               List.find (fun (name, _, _) -> name = sw_name) optional_fields
             in
             (optional_fields, [])
         | Field { name; type_ = { ft_type; _ } }
           when List.exists
                  (fun (_, _, cond_field) -> name = cond_field)
                  optional_fields ->
             ( [
                 Elaboratetree.Field_optional_mask
                   { name; type_ = conv_type ft_type };
               ],
               [] )
         | Field { name; type_ } ->
             ( [ Elaboratetree.Field { name; type_ = conv_field_type type_ } ],
               [] )
         | Field_expr { name; type_; expr } ->
             ( [
                 Elaboratetree.Field_expr
                   {
                     name;
                     type_ = conv_field_type type_;
                     expr = conv_expression expr;
                   };
               ],
               [] )
         | Field_file_descriptor f ->
             ([ Elaboratetree.Field_file_descriptor f ], [])
         | Field_pad { pad; serialize } ->
             ([ Elaboratetree.Field_pad { pad; serialize } ], [])
         | Field_list _ ->
             (* FIXME *)
             ([], []))
    |> List.split
  in
  let variants =
    List.map (fun (_, _, name, items, _) -> (name, items)) variants
    @ List.flatten additional_variants
  in
  (List.flatten fields, variants)

(** Because event structs use these despite everything else just using the
    file name. *)
let find_module_by_extension_name xcbs n =
  xcbs
  |> List.find_map (function
       | Parsetree.Extension { name; file_name; declarations; _ } when name = n
         ->
           Some (file_name, declarations)
       | _ -> None)
  |> Option.get

(** Resolve event numbers to their idents. *)
let resolve_allowed_event xcbs
    Parsetree.{ ae_module; ae_opcode_range = { min; max }; _ } =
  let find_event id_module ev_number =
    List.find_map (function
      | Parsetree.Event { name; number; _ }
      | Parsetree.Event_copy { name; ev_number = number; _ }
        when number = ev_number ->
          Some Elaboratetree.{ id_module; id_name = name }
      | _ -> None)
  in
  let file_name, declarations = find_module_by_extension_name xcbs ae_module in
  List.init (max - min + 1) (fun x -> x + min)
  |> List.map (fun n -> find_event file_name n declarations |> Option.get)

(** Split enums in pure enums and masks, based on whether they only have value
    items or also bit items. *)
let split_enums name enum_items =
  if
    List.for_all
      (function _, Parsetree.Item_value _ -> true | _ -> false)
      enum_items
  then
    let items =
      List.map
        (function
          | name, Parsetree.Item_value n -> (name, n)
          | _ -> failwith "unexpected")
        enum_items
    in
    Elaboratetree.Enum { name; items }
  else
    let items =
      List.filter_map
        (function name, Parsetree.Item_bit b -> Some (name, b) | _ -> None)
        enum_items
    in
    let additional_values =
      List.filter_map
        (function name, Parsetree.Item_value n -> Some (name, n) | _ -> None)
        enum_items
    in
    Elaboratetree.Mask { name; items; additional_values }

let variant_to_decl (name, items) = Elaboratetree.Variant { name; items }

let in_declarations (curr_module, xcbs) decls =
  decls
  |> List.filter (function Parsetree.Import _ -> false | _ -> true)
  |> List.map (function
       | Parsetree.Xid name ->
           [ Elaboratetree.(Type_alias { name; type_ = Type_primitive Xid }) ]
       | Typedef { name; type_ } ->
           [ Elaboratetree.Type_alias { name; type_ = conv_type type_ } ]
       | Xid_union { name; types } ->
           Elaboratetree.
             [
               Type_alias
                 { name; type_ = Type_union (List.map conv_ident types) };
             ]
       | Event_copy { name; event; ev_number = number } ->
           [
             Elaboratetree.Event_copy { name; event = conv_ident event; number };
           ]
       | Error_copy { name; error; er_number = number } ->
           [
             Elaboratetree.Error_copy { name; error = conv_ident error; number };
           ]
       | Event_struct { name; allowed_events } ->
           let events =
             allowed_events
             |> List.map (resolve_allowed_event xcbs)
             |> List.flatten
           in
           [ Elaboratetree.Event_struct { name; events } ]
       | Enum { name; items; doc = _ } -> [ split_enums name items ]
       | Import _ | Union _ ->
           failwith "imports and unions should already have been pruned"
       | Parsetree.Event
           { name; number; is_generic; no_sequence_number; fields; doc = _ } ->
           let fields, variants =
             enum_switches_to_variants (curr_module, xcbs) fields
           in
           List.map variant_to_decl variants
           @ [
               Elaboratetree.Event
                 {
                   name;
                   number;
                   is_generic;
                   is_serializable = false;
                   no_sequence_number;
                   fields;
                 };
             ]
       | Parsetree.Error { name; number; fields } ->
           let fields, variants =
             enum_switches_to_variants (curr_module, xcbs) fields
           in
           List.map variant_to_decl variants
           @ [ Elaboratetree.Error { name; number; fields } ]
       | Parsetree.Struct { name; fields } ->
           let fields, variants =
             enum_switches_to_variants (curr_module, xcbs) fields
           in
           List.map variant_to_decl variants
           @ [ Elaboratetree.Struct { name; fields } ]
       | Parsetree.Request
           {
             name;
             opcode;
             combine_adjacent;
             fields;
             reply = Some reply;
             doc = _;
           } ->
           let fields, variants =
             enum_switches_to_variants (curr_module, xcbs) fields
           in
           let reply_fields, reply_variants =
             enum_switches_to_variants (curr_module, xcbs) reply.fields
           in
           List.map variant_to_decl variants
           @ List.map variant_to_decl reply_variants
           @ [
               Elaboratetree.Request
                 {
                   name;
                   opcode;
                   combine_adjacent;
                   fields;
                   reply = Some reply_fields;
                 };
             ]
       | Parsetree.Request
           { name; opcode; combine_adjacent; fields; reply = None; doc = _ } ->
           let fields, variants =
             enum_switches_to_variants (curr_module, xcbs) fields
           in
           List.map variant_to_decl variants
           @ [
               Elaboratetree.Request
                 { name; opcode; combine_adjacent; fields; reply = None };
             ])
  |> List.flatten

let in_xcb xcbs = function
  | Parsetree.Core declarations ->
      Elaboratetree.Core (in_declarations ("xproto", xcbs) declarations)
  | Extension { name; file_name; query_name; multiword; version; declarations }
    ->
      let imports =
        declarations
        |> List.filter_map (function Parsetree.Import i -> Some i | _ -> None)
      in
      let declarations = declarations in
      Elaboratetree.Extension
        {
          name;
          file_name;
          query_name;
          multiword;
          version;
          imports;
          declarations = in_declarations (file_name, xcbs) declarations;
        }
