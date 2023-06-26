open Ext

(* Dumb conversions. *)

let conv_ident Parsetree.{ id_module; id_name } =
  Hir.{ id_module = Option.get id_module; id_name }

let conv_type = function
  | Parsetree.Type_primitive prim -> Hir.Type_primitive prim
  | Type_ref id -> Hir.Type_ref (conv_ident id)

let rec conv_expression = function
  | Parsetree.Binop (op, e1, e2) ->
      Hir.Binop (op, conv_expression e1, conv_expression e2)
  | Unop (op, e) -> Hir.Unop (op, conv_expression e)
  | Field_ref f -> Hir.Field_ref f
  | Param_ref { param; type_ } ->
      Hir.Param_ref { param; type_ = conv_type type_ }
  | Enum_ref { enum; item } -> Hir.Enum_ref { enum = conv_ident enum; item }
  | Pop_count e -> Hir.Pop_count (conv_expression e)
  | Sum_of { field; by_expr } ->
      Hir.Sum_of { field; by_expr = Option.map conv_expression by_expr }
  | List_element_ref -> Hir.List_element_ref
  | Expr_value n -> Hir.Expr_value n
  | Expr_bit b -> Hir.Expr_bit b

let conv_field_allowed = function
  | Parsetree.Allowed_enum id -> Hir.Allowed_enum (conv_ident id)
  | Allowed_mask id -> Hir.Allowed_mask (conv_ident id)
  | Allowed_alt_enum id -> Hir.Allowed_alt_enum (conv_ident id)
  | Allowed_alt_mask id -> Hir.Allowed_alt_mask (conv_ident id)

let conv_field_type Parsetree.{ ft_type; ft_allowed } =
  Hir.
    {
      ft_type = conv_type ft_type;
      ft_allowed = Option.map conv_field_allowed ft_allowed;
    }

(** Invert a simple expression containing a single field_ref. *)
let invert_expression expr =
  let rec invert = function
    | Parsetree.Binop (Add, e1, e2) ->
        let e1, is_var1 = invert e1 in
        let e2, is_var2 = invert e2 in
        if is_var1 then (Hir.(Binop (Sub, e1, e2)), true)
        else (Hir.(Binop (Sub, e2, e1)), is_var1 && is_var2)
    | Binop (Sub, e1, e2) ->
        let e1, is_var1 = invert e1 in
        let e2, is_var2 = invert e2 in
        (Hir.(Binop (Add, e1, e2)), is_var1 && is_var2)
    | Binop (Mul, e1, e2) ->
        let e1, is_var1 = invert e1 in
        let e2, is_var2 = invert e2 in
        if is_var1 then (Hir.(Binop (Div, e1, e2)), true)
        else (Hir.(Binop (Div, e2, e1)), is_var1 && is_var2)
    | Binop (Div, e1, e2) ->
        let e1, is_var1 = invert e1 in
        let e2, is_var2 = invert e2 in
        (Hir.(Binop (Mul, e1, e2)), is_var1 && is_var2)
    | Unop (Bit_not, e) ->
        let e, is_var = invert e in
        (Hir.(Unop (Bit_not, e)), is_var)
    | Field_ref f -> (Hir.Field_ref f, true)
    | Expr_value n -> (Hir.Expr_value n, false)
    | Expr_bit b -> (Hir.Expr_bit b, false)
    | e -> failwith (Parsetree.show_expression e)
  in
  invert expr |> fst

let%test _ =
  invert_expression Parsetree.(Binop (Add, Expr_value 4L, Field_ref "test"))
  = Hir.(Binop (Sub, Field_ref "test", Expr_value 4L))

(** Because event structs use these despite everything else just using the
    [file_name]. *)
let resolve_module_by_extension_name n =
  ListExt.find_map_exn ~f:(function
    | Parsetree.Extension { name; file_name; declarations; _ } when name = n ->
        Some (file_name, declarations)
    | _ -> None)

let resolve_event_by_num decls id_module ev_number =
  ListExt.find_map_exn decls ~f:(function
    | Parsetree.Event { name; number; _ }
    | Parsetree.Event_copy { name; ev_number = number; _ }
      when number = ev_number ->
        Some Hir.{ id_module; id_name = name }
    | _ -> None)

(** Resolve event numbers to their idents for event structs. *)
let resolve_allowed_event xcbs
    Parsetree.{ ae_module; ae_opcode_range = { min; max }; _ } =
  let file_name, declarations =
    resolve_module_by_extension_name ae_module xcbs
  in
  List.init (max - min + 1) (fun x -> x + min)
  |> List.map (resolve_event_by_num declarations file_name)

(** Split enums in pure enums and masks, based on whether they only have value
    items or also bit items. *)
let split_masks_from_enums name enum_items =
  let bits, values =
    ListLabels.partition_map enum_items ~f:(function
      | name, Parsetree.Item_bit b -> Either.Left (name, b)
      | name, Parsetree.Item_value n -> Either.Right (name, n))
  in
  if bits = [] then Hir.Enum { name; items = values }
  else
    let values =
      match values with
      | [ (_, 0L) ] -> Hir.None_value
      | _ -> Additional_values values
    in
    Hir.Mask { name; items = bits; additional_values = values }

let ( let& ) = Option.bind

(**  Return the names of the referenced fields and the number of constant
    values of an expression if it is possible to invert it. *)
let rec collect_fields_in_expression = function
  | Parsetree.Binop (Add, e1, e2)
  | Binop (Sub, e1, e2)
  | Binop (Mul, e1, e2)
  | Binop (Div, e1, e2) ->
      let& e1, v1 = collect_fields_in_expression e1 in
      let& e2, v2 = collect_fields_in_expression e2 in
      Some (e1 @ e2, v1 + v2)
  | Unop (Bit_not, e) -> collect_fields_in_expression e
  | Field_ref f -> Some ([ f ], 0)
  | Expr_value _ -> Some ([], 1)
  | Binop (_, _, _)
  (* The  *)
  | Param_ref _ | Pop_count _ | Sum_of _ | List_element_ref | Expr_bit _
  | Enum_ref _ ->
      None

type list_t = {
  list_name : string;
  inverted_expr : Hir.expression;
  type_ : Parsetree.type_;
}

type lists_cache = {
  length_by_list : (string * string) list;
  by_length : (string * list_t) list;
}

(* TODO some list length fields mark the length for multiple list fields,
   we should probably handle that. *)

(** We can currently infer the length of lists whose length is determined
    by exactly one referenced field and at most one constant value.

    Here we collect all the invertible lists indexed by the list's field name
    and its length field's name. *)
let collect_invertible_lists fields =
  ListLabels.filter_map fields ~f:(function
    | Parsetree.Field_list
        { length = Some e; name = list_name; type_ = list_type } -> (
        match collect_fields_in_expression e with
        | Some ([ length_field ], (0 | 1)) ->
            (* TODO return error when find_map returns none *)
            ListLabels.find_map fields ~f:(function
              | Parsetree.Field { name; _ } when name = length_field ->
                  let inverted_expr = invert_expression e in
                  let l =
                    { inverted_expr; type_ = list_type.ft_type; list_name }
                  in
                  Some (Ok (length_field, l))
              | _ -> None)
        | Some ([], _) -> None
        | Some ([ _ ], n) -> Some (Error (`Too_many_constants n))
        | Some (fields, (0 | 1)) -> Some (Error (`Too_many_fields fields))
        | Some (fields, n) ->
            Some (Error (`Too_many_fields_and_constants (fields, n)))
        | None -> Some (Error (`Expression_not_invertible e)))
    | _ -> None)
  |> List.fold_left
       (fun (cache, errors) -> function
         | Ok (length_field, l) ->
             let cache =
               {
                 length_by_list =
                   (l.list_name, length_field) :: cache.length_by_list;
                 by_length = cache.by_length @ [ (length_field, l) ];
               }
             in
             (cache, errors)
         | Error e -> (cache, e :: errors))
       ({ length_by_list = []; by_length = [] }, [])

let resolve_module n =
  ListExt.find_map_exn ~f:(function
    | Parsetree.Core decls when n = "xproto" -> Some decls
    | Extension { file_name; declarations; _ } when file_name = n ->
        Some declarations
    | _ -> None)

(** Switches that have a cond of & are turned into optional fields.
    Here we resolve the referenced enum's items into [Hir.Field_optional]s. *)
let conv_optional_fields ~cond ~cases fields xcbs =
  let mask =
    ListExt.find_map_exn fields ~f:(function
      | Parsetree.Field
          { name; type_ = { ft_allowed = Some (Allowed_mask mask); _ } }
        when name = cond ->
          Some mask
      | _ -> None)
  in
  let enum_module = resolve_module (Option.get mask.id_module) xcbs in
  let enum_items =
    ListExt.find_map_exn enum_module ~f:(function
      | Parsetree.Enum { name; items; _ } when name = mask.id_name -> Some items
      | _ -> None)
  in
  ListLabels.map cases ~f:(function
    | {
        Parsetree.cs_cond = [ Enum_ref { item; _ } ];
        cs_fields = [ Field { type_; name } ];
        (* Case names, when available, are exactly the same as the enum
           case name, so it's safe to ignore them. *)
        cs_name = _;
      } ->
        let bit =
          ListExt.find_map_exn enum_items ~f:(function
            | name, Parsetree.Item_bit b when name = item -> Some b
            | _ -> None)
        in
        let type_ = conv_field_type type_ in
        (name, cond, bit, type_)
        (*
           Hir.Field_optional { name; mask = cond; bit; type_ } *)
    | _ -> failwith "unexpected")

type ctx = {
  current_module : string;  (** [file_name] of the current module *)
  xcbs : Parsetree.xcb list;
}

type variant = { name : string; items : Hir.variant_item list }

let with_variants v t =
  List.map (fun { name; items } -> Hir.Variant { name; items }) v @ [ t ]

type variant_name_cache = {
  v_by_switch : (string * string) list;
  v_by_cond : (string * string) list;
}

type optional_fields_cache = {
  by_switch : (string * Hir.field list) list;
  by_condition : (string * (string * int) list) list;
}

type conv_field_output = {
  fields : Hir.field list;
  variant_types : variant list;
}

let mk_list f = [ f ]

let rec conv_variant_field ~cond ~cases fields (curr_module, xcbs) =
  let enum =
    ListExt.find_map_exn fields ~f:(function
      | Parsetree.Field
          { name; type_ = { ft_allowed = Some (Allowed_enum enum); _ } }
        when name = cond ->
          Some enum
      | _ -> None)
  in
  let enum_module = resolve_module (Option.get enum.id_module) xcbs in
  let enum_items =
    ListExt.find_map_exn enum_module ~f:(function
      | Parsetree.Enum { name; items; _ } when name = enum.id_name -> Some items
      | _ -> None)
  in
  let variant_items, variant_types =
    ListLabels.map cases ~f:(function
      | { Parsetree.cs_cond = [ Enum_ref { item; _ } ]; cs_fields; _ } ->
          let vi_tag =
            ListExt.find_map_exn enum_items ~f:(function
              | name, Parsetree.Item_value v when name = item -> Some v
              | _ -> None)
          in
          let { fields; variant_types } =
            conv_fields cs_fields (curr_module, xcbs)
          in
          (Hir.{ vi_name = item; vi_tag; vi_fields = fields }, variant_types)
      | _ -> failwith "unexpected")
    |> List.split
  in
  let variant_type = { name = enum.id_name; items = variant_items } in
  (enum.id_name, variant_type :: List.flatten variant_types)

and conv_fields fields (curr_module, xcbs) =
  let lists, _ = collect_invertible_lists fields in
  let switches =
    ListLabels.filter_map fields ~f:(function
      | Parsetree.Field_switch
          { sw_cond = Cond_eq (Field_ref c); sw_name; sw_cases } ->
          Some (`Eq, c, sw_name, sw_cases)
      | Parsetree.Field_switch
          { sw_cond = Cond_bit_and (Field_ref c); sw_name; sw_cases } ->
          Some (`Bit_and, c, sw_name, sw_cases)
      | _ -> None)
  in
  let variants, optionals =
    ListLabels.partition_map switches ~f:(function
      | `Eq, cond, name, cases ->
          let variant_name, variant_types =
            conv_variant_field ~cond ~cases fields (curr_module, xcbs)
          in
          Either.Left ((name, cond, variant_name), variant_types)
      | `Bit_and, cond, name, cases ->
          let optional_fields = conv_optional_fields ~cond ~cases fields xcbs in
          Either.Right (name, cond, optional_fields))
  in
  let variants, variant_types = List.split variants in
  let variants =
    ListLabels.fold_left variants ~init:{ v_by_switch = []; v_by_cond = [] }
      ~f:(fun acc (switch, cond, variant_name) ->
        {
          v_by_switch = (switch, variant_name) :: acc.v_by_switch;
          v_by_cond = (cond, switch) :: acc.v_by_cond;
        })
  in
  let optionals =
    ListLabels.fold_left optionals ~init:{ by_switch = []; by_condition = [] }
      ~f:(fun acc (switch, cond, fields) ->
        let fields, cases =
          ListLabels.map fields ~f:(fun (name, mask, bit, type_) ->
              (Hir.Field_optional { name; mask; bit; type_ }, (name, bit)))
          |> List.split
        in
        {
          by_switch = (switch, fields) :: acc.by_switch;
          by_condition = (cond, cases) :: acc.by_condition;
        })
  in
  let fields =
    ListLabels.map fields ~f:(function
      (* List *)
      | Parsetree.Field_list { name; type_; length = Some _ }
        when List.mem_assoc name lists.length_by_list ->
          let length_field = List.assoc name lists.length_by_list in
          Hir.Field_list_simple
            { name; type_ = conv_field_type type_; length = length_field }
          |> mk_list
      (* List length *)
      | Field { name; type_ = { ft_type; _ } }
        when List.mem_assoc name lists.by_length ->
          let l = List.assoc name lists.by_length in
          Hir.Field_list_length
            {
              name;
              type_ = conv_type ft_type;
              expr = l.inverted_expr;
              list = l.list_name;
              list_type = conv_type l.type_;
            }
          |> mk_list
      (* Variant field *)
      | Field_switch { sw_cond = Cond_eq _; sw_name; _ } ->
          let variant_name = List.assoc sw_name variants.v_by_switch in
          Hir.Field_variant
            {
              name = sw_name;
              variant = { id_module = curr_module; id_name = variant_name };
            }
          |> mk_list
      (* Variant tag *)
      | Field { name; type_ = { ft_type; _ } }
        when List.mem_assoc name variants.v_by_cond ->
          let field_name = List.assoc name variants.v_by_cond in
          Hir.Field_variant_tag
            { variant = field_name; type_ = conv_type ft_type }
          |> mk_list
      (* Optional fields *)
      | Field_switch { sw_cond = Cond_bit_and _; sw_name; _ } ->
          List.assoc sw_name optionals.by_switch
      (* Optional field mask *)
      | Field { name; type_ = { ft_type; _ } }
        when List.mem_assoc name optionals.by_condition ->
          let fields = List.assoc name optionals.by_condition in
          Hir.Field_optional_mask { name; type_ = conv_type ft_type; fields }
          |> mk_list
      (* Other fields *)
      | Field { name; type_ } ->
          Hir.Field { name; type_ = conv_field_type type_ } |> mk_list
      | Field_expr { name; type_; expr } ->
          Hir.Field_expr
            { name; type_ = conv_field_type type_; expr = conv_expression expr }
          |> mk_list
      | Field_file_descriptor f -> Hir.Field_file_descriptor f |> mk_list
      | Field_pad { pad; serialize } ->
          Hir.Field_pad { pad; serialize } |> mk_list
      | Field_list { name; type_; length } ->
          Hir.Field_list
            {
              name;
              type_ = conv_field_type type_;
              length = Option.map conv_expression length;
            }
          |> mk_list)
  in
  { fields = List.flatten fields; variant_types = List.flatten variant_types }

let conv_declaration (curr_module, xcbs) = function
  | Parsetree.Xid name ->
      Hir.(Type_alias { name; type_ = Type_primitive Xid }) |> mk_list
  | Typedef { name; type_ } ->
      Hir.Type_alias { name; type_ = conv_type type_ } |> mk_list
  | Xid_union { name; types } ->
      Hir.Type_alias { name; type_ = Type_union (List.map conv_ident types) }
      |> mk_list
  | Event_copy { name; event; ev_number = number } ->
      Hir.Event_copy { name; event = conv_ident event; number } |> mk_list
  | Error_copy { name; error; er_number = number } ->
      Hir.Error_copy { name; error = conv_ident error; number } |> mk_list
  | Event_struct { name; allowed_events } ->
      let events =
        ListLabels.map allowed_events ~f:(resolve_allowed_event xcbs)
        |> List.flatten
      in
      Hir.Event_struct { name; events } |> mk_list
  | Enum { name; items; doc = _ } ->
      split_masks_from_enums name items |> mk_list
  | Import _ -> []
  | Union _ -> failwith "unions should already have been pruned"
  | Event { name; number; is_generic; no_sequence_number; fields; doc = _ } ->
      let { fields; variant_types } = conv_fields fields (curr_module, xcbs) in
      Hir.Event
        {
          name;
          number;
          is_generic;
          is_serializable = false;
          no_sequence_number;
          fields;
        }
      |> with_variants variant_types
  | Error { name; number; fields } ->
      let { fields; variant_types } = conv_fields fields (curr_module, xcbs) in
      Hir.Error { name; number; fields } |> with_variants variant_types
  | Struct { name; fields } ->
      let { fields; variant_types } = conv_fields fields (curr_module, xcbs) in
      Hir.Struct { name; fields } |> with_variants variant_types
  | Request
      { name; opcode; combine_adjacent; fields; reply = Some reply; doc = _ } ->
      let { fields; variant_types = v1 } =
        conv_fields fields (curr_module, xcbs)
      in
      let { fields = reply_fields; variant_types = v2 } =
        conv_fields reply.fields (curr_module, xcbs)
      in
      Hir.Request
        { name; opcode; combine_adjacent; fields; reply = Some reply_fields }
      |> with_variants (v1 @ v2)
  | Request { name; opcode; combine_adjacent; fields; reply = None; doc = _ } ->
      let { fields; variant_types } = conv_fields fields (curr_module, xcbs) in
      Hir.Request { name; opcode; combine_adjacent; fields; reply = None }
      |> with_variants variant_types

let conv_declarations ctx declarations =
  let declarations =
    List.map (conv_declaration ctx) declarations |> List.flatten
  in
  ListLabels.fold_left declarations ~init:[] ~f:(fun acc -> function
    | Hir.Variant { name = n1; _ } as item ->
        if
          List.exists
            (function Hir.Variant { name = n2; _ } -> n1 = n2 | _ -> false)
            acc
        then acc
        else item :: acc
    | item -> item :: acc)
  |> List.rev

let compile_hir xcbs = function
  | Parsetree.Core declarations ->
      let declarations = conv_declarations ("xproto", xcbs) declarations in
      Hir.Core declarations
  | Extension { name; file_name; query_name; multiword; version; declarations }
    ->
      let imports =
        declarations
        |> List.filter_map (function Parsetree.Import i -> Some i | _ -> None)
      in
      let declarations = conv_declarations (file_name, xcbs) declarations in
      Hir.Extension
        {
          name;
          file_name;
          query_name;
          multiword;
          version;
          imports;
          declarations;
        }
