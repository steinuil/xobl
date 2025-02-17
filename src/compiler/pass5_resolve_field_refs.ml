open Parsetree

let find_field_type fields field_name =
  ListLabels.find_map fields ~f:(function
    | Field_expr { name; type_; _ } when field_name = name -> Some type_
    | Field { name; type_ } when field_name = name -> Some type_
    | _ -> None)

let rec in_expression fields = function
  | Binop (op, e1, e2) ->
      Binop (op, in_expression fields e1, in_expression fields e2)
  | Unop (op, e) -> Unop (op, in_expression fields e)
  | Field_ref { field; type_ = None } ->
      let type_ = find_field_type fields field in
      let type_ =
        match type_ with
        | Some t -> t
        | None when field = "length" ->
            { ft_type = Type_primitive Card16; ft_allowed = None }
        | None when field = "string_len" ->
            { ft_type = Type_primitive Card16; ft_allowed = None }
        | None when field = "num_class_info" ->
            { ft_type = Type_primitive Card8; ft_allowed = None }
        | None -> failwith field
      in
      Field_ref { field; type_ = Some type_.ft_type }
  | Sum_of { field; by_expr = Some expr } ->
      Sum_of { field; by_expr = Some (in_expression fields expr) }
  | e -> e

let rec in_field fields = function
  | Field_expr { name; type_; expr } ->
      Field_expr { name; type_; expr = in_expression fields expr }
  | Field_switch { sw_name; sw_cond; sw_cases } ->
      let sw_cond =
        match sw_cond with
        | Cond_bit_and expr -> Cond_bit_and (in_expression fields expr)
        | Cond_eq expr -> Cond_eq (in_expression fields expr)
      in
      let sw_cases =
        ListLabels.map sw_cases ~f:(fun { cs_name; cs_cond; cs_fields } ->
            let cs_cond = ListLabels.map cs_cond ~f:(in_expression fields) in
            let cs_fields = in_fields cs_fields in
            { cs_name; cs_cond; cs_fields })
      in
      Field_switch { sw_name; sw_cond; sw_cases }
  | Field_list { name; type_; length = Some length } ->
      let length = in_expression fields length in
      Field_list { name; type_; length = Some length }
  | f -> f

and in_fields fields = List.map (in_field fields) fields

let in_declaration decl =
  match decl with
  | Import _ | Xid _ | Xid_union _ | Typedef _ | Event_copy _ | Error_copy _
  | Enum _ | Event_struct _ ->
      decl
  | Union { name; members } ->
      let members = in_fields members in
      Union { name; members }
  | Event
      {
        name;
        number;
        is_generic;
        is_serializable;
        no_sequence_number;
        fields;
        doc;
      } ->
      let fields = in_fields fields in
      Event
        {
          name;
          number;
          is_generic;
          is_serializable;
          no_sequence_number;
          fields;
          doc;
        }
  | Error { name; number; fields } ->
      let fields = in_fields fields in
      Error { name; number; fields }
  | Struct { name; fields } ->
      let fields = in_fields fields in
      Struct { name; fields }
  | Request { name; opcode; combine_adjacent; fields; reply; doc } -> (
      try
        let fields = in_fields fields in
        let reply =
          Option.map
            (fun { fields; doc } -> { fields = in_fields fields; doc })
            reply
        in
        Request { name; opcode; combine_adjacent; fields; reply; doc }
      with Failure field ->
        Printf.ksprintf failwith "field `%s` in request `%s`" field name)

let resolve_field_refs xcbs =
  ListLabels.map xcbs ~f:(function
    | Core declarations ->
        let declarations = List.map in_declaration declarations in
        Core declarations
    | Extension
        { name; file_name; query_name; multiword; version; declarations } ->
        let declarations = List.map in_declaration declarations in
        Extension
          { name; file_name; query_name; multiword; version; declarations })
