open Hir
module Ident = Casing.OCaml

let not_implemented =
  Printf.ksprintf (fun str -> failwith ("not implemented: " ^ str))

let primitive_of_type = function
  | Type_primitive prim -> Some prim
  | Type_ref (_, prim) -> prim
  | Type_union _ -> Some Xid

let list_sep sep f out = function
  | [] -> ()
  | head :: tail ->
      f out head;
      List.iter
        (fun item ->
          output_string out sep;
          f out item)
        tail

let list f out = List.iter (f out)

let gen_prim = function
  | Void -> "char"
  | Char -> "char"
  | Byte -> "char"
  | Bool -> "bool"
  | Int8 -> "int"
  | Int16 -> "int"
  | Int32 -> "int"
  (* FIXME: Int32 and Card32 should be mapped to int32 to ensure compatibility with
     32-bit platforms. *)
  | Fd -> "file_descr"
  | Card8 -> "int"
  | Card16 -> "int"
  | Card32 -> "int"
  | Card64 -> "int64"
  | Float -> "float"
  | Double -> "float"
  | Xid -> "Xid.t"

let gen_to_int = function
  | Char | Byte -> Some "Char.code"
  | Bool -> Some "Bool.to_int"
  | Int8 | Int16 | Card8 | Card16 -> None
  | Xid -> Some "Xid.to_int"
  | Fd -> Some "Obj.magic"
  (* | Int32 | Card32 -> Some "Int32.to_int" *)
  | Int32 | Card32 -> None
  | Card64 -> Some "Int64.to_int"
  | Void | Float | Double -> failwith "gen_to_int"

let gen_of_int = function
  | Char | Byte -> Some "Char.chr"
  | Bool -> Some "bool_of_int"
  | Int8 | Int16 | Card8 | Card16 -> None
  | Xid -> Some "Xid.of_int"
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> None
  | Card64 -> Some "Int64.of_int"
  | Void | Float | Double -> failwith "gen_of_int"

let gen_to_int64 = function
  | Char | Byte -> Some "char_to_int64"
  | Bool -> Some "bool_to_int64"
  | Int8 | Int16 | Card8 | Card16 -> Some "Int64.of_int"
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> Some "Int64.of_int"
  | Card64 -> None
  | Void | Float | Double | Xid -> failwith "gen_to_int64"

let gen_decode_prim = function
  | Void -> "decode_char"
  | Char -> "decode_char"
  | Byte -> "decode_char"
  | Bool -> "decode_bool"
  | Int8 -> "decode_int8"
  | Int16 -> "decode_int16"
  | Int32 -> "decode_int32"
  | Fd -> "decode_file_descr"
  | Card8 -> "decode_uint8"
  | Card16 -> "decode_uint16"
  | Card32 -> "decode_int32"
  | Card64 -> "decode_int64"
  | Float -> "decode_float"
  | Double -> "decode_float"
  | Xid -> "decode_xid"

let gen_encode_prim = function
  | Void -> "encode_char"
  | Char -> "encode_char"
  | Byte -> "encode_char"
  | Bool -> "encode_bool"
  | Int8 -> "encode_int8"
  | Int16 -> "encode_int16"
  | Int32 -> "encode_int32"
  | Fd -> "encode_file_descr"
  | Card8 -> "encode_uint8"
  | Card16 -> "encode_uint16"
  | Card32 -> "encode_int32"
  | Card64 -> "encode_int64"
  | Float -> "encode_float"
  | Double -> "encode_float"
  | Xid -> "encode_xid"

let gen_size_of_prim = function
  | Void -> 1
  | Char -> 1
  | Byte -> 1
  | Bool -> 1
  | Int8 -> 1
  | Int16 -> 2
  | Int32 -> 4
  | Fd -> 2
  | Card8 -> 1
  | Card16 -> 2
  | Card32 -> 4
  | Card64 -> 8
  | Float -> 4
  | Double -> 8
  | Xid -> 4

let gen_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
  | Bit_and -> "land"
  | Bit_left_shift -> "lsl"

let rec gen_expr ctx out = function
  | Binop (op, e1, e2) ->
      Printf.fprintf out "(%a) %s (%a)" (gen_expr ctx) e1 (gen_binop op)
        (gen_expr ctx) e2
  | Unop (Bit_not, e) -> Printf.fprintf out "lnot (%a)" (gen_expr ctx) e
  | Field_ref f -> (
      match ctx with
      | None -> output_string out (Ident.snake f)
      | Some ctx -> Printf.fprintf out "%s.%s" ctx (Ident.snake f))
  | Expr_value v -> Printf.fprintf out "%Ld" v
  | Expr_bit b -> Printf.fprintf out "1 lsl %d" b
  | Sum_of { field; by_expr = None } ->
      Printf.fprintf out "List.fold_left ( + ) 0 %s" (Ident.snake field)
  | Sum_of { field; by_expr = Some by_expr } ->
      Printf.fprintf out "sum_of_expr (fun list_element_ref -> %a) %s"
        (gen_expr (Some "list_element_ref"))
        by_expr (Ident.snake field)
  (* FIXME hoist param_ref as a function argument *)
  | Param_ref { param; type_ = _ } ->
      Printf.fprintf out "failwith \"param_ref not implemented: %s\""
        (Ident.snake param)
  | Enum_ref _ -> not_implemented "gen_expr enum_ref"
  | Pop_count e -> Printf.fprintf out "pop_count (%a)" (gen_expr ctx) e
  | List_element_ref -> (
      match ctx with
      | None ->
          failwith "referenced list_element_ref outside of a sum_of expression"
      | Some ctx -> output_string out ctx)

let gen_expr_type type_ ctx out expr =
  match type_.ft_type with
  | Type_primitive Bool -> Printf.fprintf out "(%a) <> 0" (gen_expr ctx) expr
  | Type_primitive Char ->
      Printf.fprintf out "Char.chr (%a)" (gen_expr ctx) expr
  | _ -> gen_expr ctx out expr

let gen_ident ?prefix ?suffix current_module out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ?prefix ?suffix id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ?prefix ?suffix id_name)

let gen_decode_ident current_module out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"decode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"decode" id_name)

let gen_encode_ident current_module out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"encode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"encode" id_name)

let gen_size_of_ident current_module out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"size_of" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"size_of" id_name)

let gen_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref (ident, _) -> gen_ident ctx out ident
  | Type_union _ -> output_string out "Xid.t"

let gen_decode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_decode_prim prim
  | Type_ref (ident, _) -> gen_decode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_decode_prim Xid

let gen_encode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_encode_prim prim
  | Type_ref (ident, _) -> gen_encode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_encode_prim Xid

let gen_size_of_type ctx out = function
  | Type_primitive prim ->
      output_string out @@ Int.to_string @@ gen_size_of_prim prim
  | Type_ref (ident, _) -> gen_size_of_ident ctx out ident
  | Type_union _ -> output_string out @@ Int.to_string @@ gen_size_of_prim Xid

let gen_enum_item out (name, _) = Printf.fprintf out "`%s" (Ident.caml name)

let gen_decode_enum_item out (name, v) =
  Printf.fprintf out "%Ld -> Some `%s" v (Ident.caml name)

let gen_decode_enum_item_int64 out (name, v) =
  Printf.fprintf out "%LdL -> Some `%s" v (Ident.caml name)

let gen_encode_enum_item out (name, v) =
  Printf.fprintf out "`%s -> %Ld" (Ident.caml name) v

let gen_encode_enum_item_int64 out (name, v) =
  Printf.fprintf out "`%s -> %LdL" (Ident.caml name) v

let gen_decode_mask_item out (name, v) =
  Printf.fprintf out "%d -> Some `%s" v (Ident.caml name)

let gen_encode_mask_item out (name, v) =
  Printf.fprintf out "`%s -> %d" (Ident.caml name) v

let gen_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_type ctx out ft_type
  | { ft_type = _; ft_allowed = Some (Allowed_enum enum) } ->
      gen_ident ctx out
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
  | { ft_type = _; ft_allowed = Some (Allowed_mask mask) } ->
      gen_ident ctx out
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
  | { ft_type = _; ft_allowed = Some (Allowed_alt_enum enum) } ->
      Printf.fprintf out "%a alt_enum" (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      Printf.fprintf out "(%a, %a) mask" (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
        (gen_type ctx) ft_type

let gen_list_type ctx out t =
  match primitive_of_type t.ft_type with
  | Some Char | Some Void -> output_string out "string"
  | Some _ | None -> Printf.fprintf out "%a list" (gen_field_type ctx) t

let gen_list_length out t =
  match primitive_of_type t with
  | Some Char | Some Void -> output_string out "String.length"
  (* TODO why is this marked as invalid_argument? *)
  | None -> output_string out "(* invalid_argument *) List.length"
  | Some t ->
      Printf.fprintf out "(* primitive %s *) " (show_prim t);
      output_string out "List.length"

let gen_field_type_of_field ctx out = function
  | Field { type_; _ } -> Printf.fprintf out "%a" (gen_field_type ctx) type_
  | Field_optional { type_; _ } ->
      Printf.fprintf out "%a option" (gen_field_type ctx) type_
  | Field_list { type_; _ } | Field_list_simple { type_; _ } ->
      Printf.fprintf out "%a" (gen_list_type ctx) type_
  | Field_variant { variant; _ } ->
      Printf.fprintf out "%a" (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      failwith "invalid field"

let gen_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "%s : %a option; " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_list_type ctx)
        type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()

let gen_decode_field_type ctx out = function
  | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "decode_enum %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "decode_alt_enum %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "decode_mask %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "decode_alt_mask %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = None } -> gen_decode_type ctx out ft_type

let gen_encode_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_encode_type ctx out ft_type
  | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "encode_enum %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "encode_mask %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"int_of_mask" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "encode_alt_enum %a %s %s %a" (gen_encode_type ctx)
        ft_type
        (match gen_encode_prim p with "encode_xid" -> "encode_int32" | s -> s)
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      let p = primitive_of_type ft_type |> Option.get in
      Printf.fprintf out "encode_alt_mask %a %s %a" (gen_encode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"int_of_mask" }

let gen_encode_list ctx out t =
  match primitive_of_type t.ft_type with
  | Some (Char | Void) -> output_string out "encode_string"
  | Some _ | None ->
      Printf.fprintf out "encode_list (%a)" (gen_encode_field_type ctx) t

let gen_decode_list ctx out t =
  match primitive_of_type t.ft_type with
  | Some (Char | Void) -> output_string out "decode_string"
  | Some _ | None ->
      Printf.fprintf out "decode_list (%a)" (gen_decode_field_type ctx) t

let gen_size_of_field_type ctx out { ft_type; _ } =
  gen_size_of_type ctx out ft_type

let gen_decode_field ctx _fields out = function
  | Field { name; type_ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in" (Ident.snake name)
        (gen_decode_field_type ctx)
        type_
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "let at = at + %d in" n
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "let at = at + ((at - orig) mod %d) in" n
  | Field_list_length { name; type_; expr; _ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in " (Ident.snake name)
        (gen_decode_type ctx) type_;
      Option.iter
        (fun to_int ->
          Printf.fprintf out "let %s = %s %s in " (Ident.snake name) to_int
            (Ident.snake name))
        (primitive_of_type type_ |> Option.get |> gen_to_int);
      Printf.fprintf out "let %s = %a in" (Ident.snake name) (gen_expr None)
        expr
  | Field_list_simple { name; type_; length } ->
      Printf.fprintf out "let* %s, at = %a %s buf ~at in" (Ident.snake name)
        (gen_decode_list ctx) type_ (Ident.snake length)
  | Field_optional_mask { name; type_; _ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in" (Ident.snake name)
        (gen_decode_type ctx) type_
  | Field_optional { name; type_; mask; bit } ->
      Printf.fprintf out
        "let* %s, at = if %s land (1 lsl %d) <> 0 then let* v, at = %a buf ~at \
         in Some ((Some v, at)) else Some ((None, at))"
        (Ident.snake name) (Ident.snake mask) bit
        (gen_decode_field_type ctx)
        type_
  | Field_list { name; type_; length = Some length } ->
      Printf.fprintf out "let* %s, at = let length = %a in %a length buf ~at in"
        (Ident.snake name) (gen_expr None) length (gen_decode_list ctx) type_
  | Field_list { name; type_ = _; length = None } ->
      Printf.ksprintf failwith "decoding list field with length = None: %s" name
  | Field_expr _ -> output_string out "(* field_expr *)"
  | Field_variant_tag { field_name; variant = _; type_ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in"
        (Ident.snake field_name ~suffix:"tag")
        (gen_decode_type ctx) type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "let* %s, at = %a %s buf ~at ~orig in"
        (Ident.snake name) (gen_ident ctx)
        {
          variant with
          id_name =
            Ident.snake variant.id_name ~prefix:"decode" ~suffix:"variant";
        }
        (Ident.snake name ~suffix:"tag")

let gen_encode_optional_mask_fields out fields =
  output_string out "[";
  list_sep "; "
    (fun out (name, bit) ->
      Printf.fprintf out "(Option.is_some %s, %d)" (Ident.snake name) bit)
    out fields;
  output_string out "]"

let gen_encode_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%a buf v.%s;"
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "encode_pad buf %d;" n
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "encode_align buf %d;" n
  | Field_expr { type_; expr; _ } ->
      Printf.fprintf out "%a buf (%a);"
        (gen_encode_field_type ctx)
        type_ (gen_expr (Some "v")) expr
  | Field_list_length { type_; list; list_type; _ } ->
      (* Can be wrong *)
      Printf.fprintf out "%a buf (%s (%a v.%s));" (gen_encode_type ctx) type_
        (Option.value ~default:""
           (gen_of_int (primitive_of_type type_ |> Option.get)))
        gen_list_length list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "%a buf v.%s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "%a buf v.%s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_variant { name; variant } ->
      Printf.fprintf out "%a buf v.%s;"
        (gen_ident ~prefix:"encode" ~suffix:"variant" ctx)
        variant name
  | Field_variant_tag { field_name; variant; type_ } ->
      Printf.fprintf out "%a buf (%a v.%s);" (gen_encode_type ctx) type_
        (gen_ident ~suffix:"int_of_variant" ctx)
        variant field_name
  | Field_optional _ | Field_optional_mask _ ->
      invalid_arg "there are no optional fields in structs"

let gen_encode_arg_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%a buf %s;"
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "encode_pad buf %d;" n
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "encode_align buf %d;" n
  | Field_expr { type_; expr; _ } ->
      Printf.fprintf out "%a buf (%a);"
        (gen_encode_field_type ctx)
        type_
        (gen_expr_type type_ (Some "v"))
        expr
  | Field_list_length { type_; list; list_type; _ } ->
      (* Can be wrong TODO how?? *)
      Printf.fprintf out "%a buf (%s (%a %s));" (gen_encode_type ctx) type_
        (Option.value ~default:""
           (gen_of_int (primitive_of_type type_ |> Option.get)))
        gen_list_length list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "%a buf %s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "%a buf %s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_variant { name; variant } ->
      Printf.fprintf out "%a buf %s;"
        (gen_ident ~prefix:"encode" ~suffix:"variant" ctx)
        variant name
  | Field_variant_tag { field_name; variant; type_ } ->
      Printf.fprintf out "%a buf (%a %s);" (gen_encode_type ctx) type_
        (gen_ident ~suffix:"int_of_variant" ctx)
        variant field_name
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "(match %s with None -> () | Some v -> "
        (Ident.snake name);
      Printf.fprintf out "%a buf v);" (gen_encode_field_type ctx) type_
  | Field_optional_mask { type_; fields; _ } ->
      Printf.fprintf out "encode_optional_mask %a buf %a;" (gen_encode_type ctx)
        type_ gen_encode_optional_mask_fields fields

let gen_encode_single_field ctx out ~name = function
  | Field { type_; _ } ->
      Printf.fprintf out "%a buf %s;"
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_list { type_; _ } ->
      Printf.fprintf out "%a buf %s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_list_simple { type_; _ } ->
      Printf.fprintf out "%a buf %s;" (gen_encode_list ctx) type_
        (Ident.snake name)
  | Field_optional { type_; _ } ->
      Printf.fprintf out "(match %s with None -> () | Some v -> "
        (Ident.snake name);
      Printf.fprintf out "%a buf v);" (gen_encode_field_type ctx) type_
  | Field_expr _ | Field_pad _ | Field_optional_mask _ | Field_list_length _
  | Field_variant _ | Field_variant_tag _ ->
      failwith "never exercised"

let gen_size_of_field ctx out = function
  | Field { type_; _ } -> gen_size_of_field_type ctx out type_
  | Field_pad { pad = Pad_bytes n; _ } -> output_string out (Int.to_string n)
  | Field_list_length { type_; _ } -> gen_size_of_type ctx out type_
  | Field_optional_mask { type_; _ } -> gen_size_of_type ctx out type_
  | Field_expr { type_; _ } -> gen_size_of_field_type ctx out type_
  | Field_variant_tag { type_; _ } -> gen_size_of_type ctx out type_
  | Field_optional { type_; mask; bit; _ } ->
      Printf.fprintf out "if (v.%s land %d) <> 0 then (%a) else 0"
        (Ident.snake mask) bit
        (gen_size_of_field_type ctx)
        type_
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "((at - orig) mod %d)" n
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "(%a v.%s) * (%a)" gen_list_length type_.ft_type
        (Ident.snake name)
        (gen_size_of_field_type ctx)
        type_
  | Field_variant { variant; _ } ->
      gen_size_of_ident ctx out
        {
          variant with
          id_name =
            Ident.snake variant.id_name ~prefix:"size_of" ~suffix:"variant";
        }

let name_of_field = function
  | Field { name; _ }
  | Field_optional { name; _ }
  | Field_list { name; _ }
  | Field_list_simple { name; _ }
  | Field_variant { name; _ }
  | Field_expr { name; _ } ->
      Some name
  | _ -> None

let gen_decode_fields ctx out fields =
  output_string out "let orig = at in ";
  list_sep "\n" (gen_decode_field ctx fields) out fields;
  let names_of_fields = List.filter_map name_of_field fields in
  match names_of_fields with
  | [] -> Printf.fprintf out "\nignore orig;\nSome ((), at)"
  | [ field_name ] ->
      Printf.fprintf out "\nignore orig;\nSome (%s, at)"
        (Ident.snake field_name)
  | _ ->
      Printf.fprintf out "\nignore orig;\nSome ({ %s }, at)"
        (names_of_fields |> List.map Ident.snake |> String.concat "; ")

let gen_decode_event_fields ctx out fields =
  output_string out "let orig = at in ";
  output_string out "let at = at + 1 in ";
  (match fields with
  | first :: rest ->
      gen_decode_field ctx [] out first;
      output_string out " let* _sequence_number, at = decode_uint16 buf ~at in ";
      list_sep " " (gen_decode_field ctx fields) out rest
  | [] -> ());
  let names_of_fields = List.filter_map name_of_field fields in
  match names_of_fields with
  | [] -> Printf.fprintf out "\nignore orig;\nSome ((), at)"
  | [ field_name ] ->
      Printf.fprintf out "\nignore orig;\nSome (%s, at)"
        (Ident.snake field_name)
  | _ ->
      Printf.fprintf out "\nignore orig;\nSome ({ %s }, at)"
        (names_of_fields |> List.map Ident.snake |> String.concat "; ")

let is_field_visible = function
  | Field _ | Field_optional _ | Field_list _ | Field_list_simple _
  | Field_variant _ ->
      true
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      false

let visible_fields fields = fields |> List.filter is_field_visible

let gen_encode_fields ctx out fields =
  (match visible_fields fields with
  | [ field ] -> gen_encode_single_field ctx out field ~name:"v"
  | _ -> list_sep " " (gen_encode_field ctx) out fields);
  Printf.fprintf out "()"

(* TODO make sure the total length of the request includes the request length field
    when there are less than two fields *)

(** Generate fields without the v. prefix *)
let gen_encode_arg_fields ctx opcode out fields =
  (* output_string out "let orig = at in\n"; *)
  output_string out "(* opcode *)\n";
  Printf.fprintf out "encode_uint8 buf %d;\n" opcode;
  (match fields with
  | first :: rest ->
      gen_encode_arg_field ctx out first;
      output_string out "\n(* reserve request length *)\nencode_pad buf 2;\n";
      list_sep "\n" (gen_encode_arg_field ctx) out rest
  | [] -> output_string out "(* reserve request length *)\nencode_pad buf 3;");
  output_string out "\n(* write request length *)\nencode_request_length buf\n"

let gen_size_of_fields ctx out fields =
  output_string out "let orig = at in ";
  fields
  |> List.iter
       (Printf.fprintf out "let at = at + (%a) in " (gen_size_of_field ctx));
  output_string out "at"

let gen_decode_reply_fields ctx out = function
  | first :: rest ->
      let fields =
        (* First byte indicates that it is a reply (?) *)
        Field_pad { pad = Pad_bytes 1; serialize = false } (* First field *)
        :: first (* Sequence number (2 bytes) + length (4 bytes) *)
        :: Field_pad { pad = Pad_bytes 6; serialize = false }
        :: rest
      in
      gen_decode_fields ctx out fields
  | [] -> failwith "reply with no fields"

let gen_variant_item ctx out { vi_name; vi_tag = _; vi_fields } =
  Printf.fprintf out "%s of { %a}" (Ident.caml vi_name)
    (list (gen_field ctx))
    vi_fields

let gen_decode_variant_item ctx out { vi_name; vi_tag; vi_fields } =
  Printf.fprintf out "%Ld -> %a Some (%s { %a}, at)" vi_tag
    (list_sep " " (gen_decode_field ctx []))
    vi_fields (Ident.caml vi_name)
    (list_sep "; " output_string)
    (List.filter_map name_of_field vi_fields)

let gen_int_of_variant_item _ctx out { vi_name; vi_tag; _ } =
  Printf.fprintf out "%s _ -> %Ld" (Ident.caml vi_name) vi_tag

let gen_encode_variant_item ctx out { vi_name; vi_fields; _ } =
  Printf.fprintf out "%s {%a} -> %a ()" (Ident.caml vi_name)
    (list_sep "; " output_string)
    (List.filter_map name_of_field vi_fields)
    (list (gen_encode_arg_field ctx))
    vi_fields

let gen_named_arg ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "?(%s : %a option) " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_list_type ctx)
        type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()

let gen_fields ctx out fields =
  match visible_fields fields with
  | [] -> output_string out "unit"
  | [ field ] -> (gen_field_type_of_field ctx) out field
  | _ -> Printf.fprintf out "{ %a}" (list (gen_field ctx)) fields

let gen_event_struct_field ctx out ident =
  Printf.fprintf out "%s of %a" (Ident.caml ident.id_name) (gen_ident ctx)
    { ident with id_name = Ident.snake ident.id_name ~suffix:"event" }

let gen_encode_event_struct_field ctx out ident =
  Printf.fprintf out "%s v -> %a buf v" (Ident.caml ident.id_name)
    (gen_ident ~prefix:"encode" ~suffix:"event" ctx)
    ident

let mask_item out (name, _) = Printf.fprintf out "`%s" (Ident.caml name)

let gen_declaration ctx out = function
  | Type_alias { name; type_ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;\n" (Ident.snake name)
        (gen_type ctx) type_;
      Printf.fprintf out "let %s = %a;;\n"
        (Ident.snake ~prefix:"decode" name)
        (gen_decode_type ctx) type_;
      Printf.fprintf out "let %s = %a;;\n"
        (Ident.snake ~prefix:"encode" name)
        (gen_encode_type ctx) type_
      (* Printf.fprintf out "let %s = %a;;"
         (Ident.snake ~prefix:"size_of" name)
         (gen_size_of_type ctx) type_ *)
  | Struct { name; fields } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;\n" (Ident.snake name)
        (gen_fields ctx) fields;
      Printf.fprintf out "let %s buf ~at : (%s * int) option = %a;;\n"
        (Ident.snake ~prefix:"decode" name)
        (Ident.snake name) (gen_decode_fields ctx) fields;
      Printf.fprintf out "let %s buf (v : %s) = %a;;\n"
        (Ident.snake ~prefix:"encode" name)
        (Ident.snake name) (gen_encode_fields ctx) fields
      (* Printf.fprintf out "let %s buf (v : %s) ~at : int = %a;;"
         (Ident.snake ~prefix:"size_of" name)
         (Ident.snake name) (gen_size_of_fields ctx) fields *)
  | Enum { name; items } ->
      Printf.fprintf out "type %s = [ %a ] [@@deriving sexp];;\n"
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_enum_item)
        items;
      Printf.fprintf out
        "let %s : int -> [> %s ] option = function %a | n -> None;;\n"
        (Ident.snake name ~suffix:"enum_of_int")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_decode_enum_item)
        items;
      Printf.fprintf out "let %s : %s -> int = function %a;;"
        (Ident.snake name ~suffix:"int_of_enum")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_encode_enum_item)
        items
  | Mask { name; items; additional_values = Additional_values [] } ->
      Printf.fprintf out "type %s = [ %a ] list [@@deriving sexp];;\n"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items;
      Printf.fprintf out
        "let %s mask : %s option = let of_int = function %a | _ -> None in \
         mask_of_int of_int mask;;\n"
        (Ident.snake name ~suffix:"mask_of_int64")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_decode_mask_item)
        items;
      Printf.fprintf out
        "let %s : %s -> int = let to_bit = function %a in int_of_mask to_bit;;"
        (Ident.snake name ~suffix:"int_of_mask")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_encode_mask_item)
        items
  | Mask { name; items; additional_values = Additional_values values } ->
      Printf.fprintf out
        "type %s = ([ %a ] list, [ %a ]) mask [@@deriving sexp];;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items (list_sep " | " mask_item) values;
      Printf.fprintf out
        "let %s mask : %s option = let of_enum = function %a | _ -> None in \
         let of_mask = function %a | _ -> None in mask_value_of_int of_mask \
         of_enum mask;;\n"
        (Ident.snake name ~suffix:"mask_of_int64")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_decode_enum_item_int64)
        values
        (list_sep " | " gen_decode_mask_item)
        items;
      Printf.fprintf out
        "let %s (mask : %s) : int = let to_enum = function %a in let to_mask = \
         function %a in int_of_mask_value to_mask to_enum mask;;"
        (Ident.snake name ~suffix:"int_of_mask")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_encode_enum_item)
        values
        (list_sep " | " gen_encode_mask_item)
        items
  | Mask { name; items; additional_values = None_value } ->
      Printf.fprintf out "type %s = [ %a ] list [@@deriving sexp];;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items;
      Printf.fprintf out
        "let %s mask : %s option =\n\
         let of_mask = function %a | _ -> None in if mask = 0L then None else \
         mask_of_int of_mask mask;;\n"
        (Ident.snake name ~suffix:"mask_of_int64")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_decode_mask_item)
        items;
      Printf.fprintf out
        "let %s (mask : %s) : int = let to_mask = function %a in match mask \
         with [] -> 0 | mask -> int_of_mask to_mask mask;;"
        (Ident.snake name ~suffix:"int_of_mask")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_encode_mask_item)
        items
  | Variant { name; items } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_variant_item ctx))
        items;
      Printf.fprintf out
        "let %s tag buf ~at ~orig : (%s * int) option = match tag with %a | _ \
         -> None;;"
        (Ident.snake name ~prefix:"decode" ~suffix:"variant")
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_decode_variant_item ctx))
        items;
      Printf.fprintf out "let %s (v : %s) : int = match v with %a;;"
        (Ident.snake name ~suffix:"int_of_variant")
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_int_of_variant_item ctx))
        items;
      Printf.fprintf out "let %s buf (v : %s) = match v with %a;;"
        (Ident.snake name ~prefix:"encode" ~suffix:"variant")
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_encode_variant_item ctx))
        items
  | Event { name = "RedirectNotify" as name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields
  | Event { name; fields; is_serializable; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields;
      Printf.fprintf out "let %s buf ~at : (%s * int) option = %a;;\n"
        (Ident.snake name ~prefix:"decode" ~suffix:"event")
        (Ident.snake name ~suffix:"event")
        (gen_decode_event_fields ctx)
        fields;
      if is_serializable then
        Printf.fprintf out "let %s buf (v : %s) = %a;;\n"
          (Ident.snake name ~prefix:"encode" ~suffix:"event")
          (Ident.snake name ~suffix:"event")
          (gen_encode_fields ctx) fields
  | Error { name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"error")
        (gen_fields ctx) fields
  (* TODO fix QueryTextExtents *)
  | Request { name = "QueryTextExtents"; _ } -> ()
  | Request { name; fields; reply; opcode; _ } ->
      Option.iter
        (Printf.fprintf out "type %s = %a [@@deriving sexp];;\n"
           (Ident.snake name ~suffix:"reply")
           (gen_fields ctx))
        reply;
      Printf.fprintf out "let %s %a buf =\n"
        (Ident.snake name ~prefix:"encode")
        (list (gen_named_arg ctx))
        fields;
      Printf.fprintf out "%a;;" (gen_encode_arg_fields ctx opcode) fields;
      reply
      |> Option.iter (function
           | fields when visible_fields fields = [] ->
               Printf.fprintf out
                 "\n\
                  let %s length buf ~at : (%s * int) option = ignore length; \
                  ignore buf; Some ((), at);;"
                 (Ident.snake ~prefix:"decode" ~suffix:"reply" name)
                 (Ident.snake name ~suffix:"reply")
           | fields ->
               Printf.fprintf out
                 "\nlet %s length buf ~at : (%s * int) option = %a;;"
                 (Ident.snake ~prefix:"decode" ~suffix:"reply" name)
                 (Ident.snake name ~suffix:"reply")
                 (gen_decode_reply_fields ctx)
                 fields)
  | Event_copy { name; event; is_serializable; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_ident ctx)
        { event with id_name = Ident.snake event.id_name ~suffix:"event" };
      if is_serializable then
        Printf.fprintf out "let %s = %a;;"
          (Ident.snake name ~prefix:"encode" ~suffix:"event")
          (gen_ident ~prefix:"encode" ~suffix:"event" ctx)
          event
  | Error_copy { name; error; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"error")
        (gen_ident ctx)
        { error with id_name = Ident.snake error.id_name ~suffix:"error" }
  | Event_struct { name; events } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;" (Ident.snake name)
        (list_sep " | " (gen_event_struct_field ctx))
        events;
      Printf.fprintf out "let %s buf (v: %s) = match v with %a;;"
        (Ident.snake name ~prefix:"encode")
        (Ident.snake name)
        (list_sep " | " (gen_encode_event_struct_field ctx))
        events

let gen_xcb out xcb =
  output_string out "[@@@warning \"-27\"]\n";
  output_string out "[@@@warning \"-11\"]\n";
  output_string out "[@@@warning \"-33\"]\n";
  output_string out "open Codec\n";
  output_string out "open X11_types\n";
  output_string out "open Util\n";
  output_string out "open Sexplib.Conv\n";
  match xcb with
  | Core decls -> (list_sep "\n" (gen_declaration "xproto")) out decls
  | Extension { declarations; name = _; file_name; _ } ->
      (list_sep "\n" (gen_declaration file_name)) out declarations

let gen out xcbs = List.iter (gen_xcb out) xcbs
