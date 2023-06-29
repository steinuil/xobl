open Hir
module Ident = Casing.OCaml

module Ctx : sig
  type t = { current_module : string; xcbs : xcb list }

  val resolve_prim : t -> type_ -> prim option
  (** A result of [None] means the type is not a primitive. *)
end = struct
  type t = { current_module : string; xcbs : xcb list }

  let ( let& ) = Option.bind

  let find_module_by_name module_name xcbs =
    List.find_map
      (function
        | Core decls when module_name = "xproto" -> Some (decls, [])
        | Extension { file_name; declarations; imports; _ }
          when file_name = module_name ->
            Some (declarations, imports)
        | _ -> None)
      xcbs

  let find_prim n = function
    | Type_alias { name; type_ = Type_primitive prim } when name = n ->
        Some (`Prim prim)
    | Type_alias { name; type_ = Type_union _ } when name = n ->
        Some (`Prim Xid)
    | Type_alias { name; type_ = Type_ref ident } when name = n ->
        Some (`Ref ident)
    | _ -> None

  let rec resolve_name_as_prim { current_module; xcbs } name =
    let& decls, _ = find_module_by_name current_module xcbs in
    match List.find_map (find_prim name) decls with
    | Some (`Prim p) -> Some p
    | Some (`Ref { id_module; id_name }) ->
        resolve_name_as_prim { current_module = id_module; xcbs } id_name
    | None -> None

  let resolve_prim { xcbs; _ } = function
    | Type_primitive prim -> Some prim
    | Type_ref ident ->
        resolve_name_as_prim
          { current_module = ident.id_module; xcbs }
          ident.id_name
    | Type_union _ -> Some Xid
end

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
  | Fd -> "Unix.file_descr"
  | Card8 -> "int"
  | Card16 -> "int"
  | Card32 -> "int"
  | Card64 -> "int64"
  | Float -> "float"
  | Double -> "float"
  | Xid -> "xid"

let gen_to_int = function
  | Char | Byte -> Some "Char.code"
  | Bool -> Some "Bool.to_int"
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some "Obj.magic"
  (* | Int32 | Card32 -> Some "Int32.to_int" *)
  | Int32 | Card32 -> None
  | Card64 -> Some "Int64.to_int"
  | Void | Float | Double -> failwith "gen_to_int"

let gen_of_int = function
  | Char | Byte -> Some "Char.chr"
  | Bool -> Some "bool_of_int"
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> None
  | Card64 -> Some "Int64.of_int"
  | Void | Float | Double -> failwith "gen_of_int"

let gen_to_int64 = function
  | Char | Byte -> Some "char_to_int64"
  | Bool -> Some "bool_to_int64"
  | Int8 | Int16 | Card8 | Card16 | Xid -> Some "Int64.of_int"
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> Some "Int64.of_int"
  | Card64 -> None
  | Void | Float | Double -> failwith "gen_to_int64"

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
      Printf.fprintf out
        "List.fold_left (fun acc list_element_ref -> acc + (%a)) 0 %s"
        (gen_expr (Some "list_element_ref"))
        by_expr (Ident.snake field)
  (* Param refs just don't work this way. *)
  | Param_ref { param; type_ = _ } -> output_string out (Ident.snake param)
  | Enum_ref _ -> failwith "gen_expr enum_ref"
  | Pop_count _ -> failwith "gen_expr pop_count"
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

let gen_ident Ctx.{ current_module; _ } out { id_module; id_name } =
  if current_module = id_module then output_string out (Ident.snake id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake id_name)

let gen_decode_ident Ctx.{ current_module; _ } out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"decode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"decode" id_name)

let gen_encode_ident Ctx.{ current_module; _ } out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"encode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"encode" id_name)

let gen_size_of_ident Ctx.{ current_module; _ } out { id_module; id_name } =
  if current_module = id_module then
    output_string out (Ident.snake ~prefix:"size_of" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"size_of" id_name)

let gen_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref ident -> gen_ident ctx out ident
  | Type_union _ -> output_string out "xid"

let gen_decode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_decode_prim prim
  | Type_ref ident -> gen_decode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_decode_prim Xid

let gen_encode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_encode_prim prim
  | Type_ref ident -> gen_encode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_encode_prim Xid

let gen_size_of_type ctx out = function
  | Type_primitive prim ->
      output_string out @@ Int.to_string @@ gen_size_of_prim prim
  | Type_ref ident -> gen_size_of_ident ctx out ident
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
      (* (gen_type ctx) ft_type *)
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      Printf.fprintf out "(%a, %a) mask" (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
        (gen_type ctx) ft_type

let gen_list_type ctx out t =
  match Ctx.resolve_prim ctx t.ft_type with
  | Some Char | Some Void -> output_string out "string"
  | Some _ | None -> Printf.fprintf out "%a list" (gen_field_type ctx) t

let gen_list_length ctx out t =
  match Ctx.resolve_prim ctx t with
  | Some Char | Some Void -> output_string out "String.length"
  | None -> output_string out "(* invalid_argument *) List.length"
  | Some t ->
      Printf.fprintf out "(* %s *) " (show_prim t);
      output_string out "List.length"

let gen_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "%s : Unix.file_descr; " (Ident.snake name)
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
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "decode_enum %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "decode_alt_enum %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "decode_mask %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "decode_alt_mask %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = None } -> gen_decode_type ctx out ft_type

let gen_encode_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_encode_type ctx out ft_type
  | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "encode_enum %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "encode_mask %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"int_of_mask" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = Ctx.resolve_prim ctx ft_type |> Option.get in
      Printf.fprintf out "encode_alt_enum %a %s %a" (gen_encode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some _ } -> gen_encode_type ctx out ft_type

let gen_encode_list ctx out t =
  match Ctx.resolve_prim ctx t.ft_type with
  | Some (Char | Void) -> output_string out "encode_string"
  | Some _ | None ->
      Printf.fprintf out "encode_list %a" (gen_encode_field_type ctx) t

let gen_decode_list ctx out t =
  match Ctx.resolve_prim ctx t.ft_type with
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
  | Field_file_descriptor name ->
      Printf.fprintf out "let* %s, at = decode_file_descr buf ~at in"
        (Ident.snake name)
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
        (Ctx.resolve_prim ctx type_ |> Option.get |> gen_to_int);
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
  | Field_variant_tag { variant; type_ } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in"
        (Ident.snake variant ~suffix:"tag")
        (gen_decode_type ctx) type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "let* %s, at = %a %s buf ~at in" (Ident.snake name)
        (gen_ident ctx)
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
      Printf.fprintf out "let* at = %a buf v.%s ~at in"
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_file_descriptor name ->
      Printf.fprintf out "let* at = encode_file_descriptor buf v.%s ~at in"
        (Ident.snake name)
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "let at = at + %d in" n
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "let at = at + ((at - orig) mod %d) in" n
  | Field_expr { type_; expr; _ } ->
      Printf.fprintf out "let* at = %a buf (%a) ~at in"
        (gen_encode_field_type ctx)
        type_ (gen_expr (Some "v")) expr
  | Field_list_length { type_; list; list_type; _ } ->
      (* Can be wrong *)
      Printf.fprintf out "let* at = %a buf (%s (%a v.%s)) ~at in"
        (gen_encode_type ctx) type_
        (Option.value ~default:"identity"
           (gen_of_int (Ctx.resolve_prim ctx type_ |> Option.get)))
        (gen_list_length ctx) list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf v.%s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf v.%s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_variant _ -> Printf.fprintf out "(* field_variant *)"
  | Field_variant_tag _ -> Printf.fprintf out "(* field_variant_tag *)"
  | Field_optional _ | Field_optional_mask _ ->
      invalid_arg "there are no optional fields in structs"

let gen_encode_arg_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "let* at = %a buf %s ~at in"
        (gen_encode_field_type ctx)
        type_ (Ident.snake name)
  | Field_file_descriptor name ->
      Printf.fprintf out "let* at = encode_file_descriptor buf %s ~at in"
        (Ident.snake name)
  | Field_pad { pad = Pad_bytes n; _ } ->
      Printf.fprintf out "let at = at + %d in" n
  | Field_pad { pad = Pad_align n; _ } ->
      Printf.fprintf out "let at = at + ((at - orig) mod %d) in" n
  | Field_expr { type_; expr; _ } ->
      Printf.fprintf out "let* at = %a buf (%a) ~at in"
        (gen_encode_field_type ctx)
        type_
        (gen_expr_type type_ (Some "v"))
        expr
  | Field_list_length { type_; list; list_type; _ } ->
      (* Can be wrong TODO how?? *)
      Printf.fprintf out "let* at = %a buf (%s (%a %s)) ~at in"
        (gen_encode_type ctx) type_
        (Option.value ~default:"identity"
           (gen_of_int (Ctx.resolve_prim ctx type_ |> Option.get)))
        (gen_list_length ctx) list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf %s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf %s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_variant _ -> Printf.fprintf out "(* field_variant *)"
  | Field_variant_tag _ -> Printf.fprintf out "(* field_variant_tag *)"
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "let* at = match %s with None -> Some at | Some v -> "
        (Ident.snake name);
      Printf.fprintf out "%a buf v ~at in" (gen_encode_field_type ctx) type_
  | Field_optional_mask { type_; fields; _ } ->
      Printf.fprintf out "let* at = encode_optional_mask %a buf %a ~at in"
        (gen_encode_type ctx) type_ gen_encode_optional_mask_fields fields

let gen_size_of_field ctx out = function
  | Field { type_; _ } -> gen_size_of_field_type ctx out type_
  | Field_file_descriptor _ -> output_string out "2"
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
      Printf.fprintf out "(%a v.%s) * (%a)" (gen_list_length ctx) type_.ft_type
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
  | Field_file_descriptor name
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
  if List.length (List.filter_map name_of_field fields) = 0 then
    Printf.fprintf out "\nignore orig;\nSome ((), at)"
  else
    Printf.fprintf out "\nignore orig;\nSome ({ %s }, at)"
      (List.filter_map name_of_field fields
      |> List.map Ident.snake |> String.concat "; ")

let gen_decode_event_fields ctx out fields =
  output_string out "let orig = at in ";
  output_string out "let at = at + 1 in ";
  (match fields with
  | first :: rest ->
      gen_decode_field ctx [] out first;
      output_string out " let* _sequence_number, at = decode_uint16 buf ~at in ";
      list_sep " " (gen_decode_field ctx fields) out rest
  | [] -> ());
  if List.length (List.filter_map name_of_field fields) = 0 then
    Printf.fprintf out "\nignore orig;\nSome ((), at)"
  else
    Printf.fprintf out "\nignore orig;\nSome ({ %s }, at)"
      (List.filter_map name_of_field fields
      |> List.map Ident.snake |> String.concat "; ")

let gen_encode_fields ctx out fields =
  output_string out "let orig = at in ";
  list_sep " " (gen_encode_field ctx) out fields;
  Printf.fprintf out " ignore orig; Some at"

(* TODO make sure the total length of the request includes the request length field
    when there are less than two fields *)

(** Generate fields without the v. prefix *)
let gen_encode_arg_fields ctx opcode out fields =
  output_string out "let orig = at in\n";
  output_string out "(* opcode *)\n";
  Printf.fprintf out "let* at = encode_uint8 buf %d ~at in\n" opcode;
  (match fields with
  | first :: rest ->
      gen_encode_arg_field ctx out first;
      output_string out "\n(* reserve request length *)\nlet at = at + 2 in\n";
      list_sep "\n" (gen_encode_arg_field ctx) out rest
  | [] -> output_string out "(* reserve request length *)\nlet at = at + 3 in");
  output_string out
    "\n\
     (* write request length *)\n\
     let* _ = encode_uint16 buf ((at - orig) / 4) ~at:(orig + 2) in\n";
  output_string out "Some at"

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

let gen_named_arg ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "~(%s : Unix.file_descr) " (Ident.snake name)
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

let is_field_visible = function
  | Field _ | Field_file_descriptor _ | Field_optional _ | Field_list _
  | Field_list_simple _ | Field_variant _ ->
      true
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      false

let visible_fields fields =
  fields |> List.filter is_field_visible |> List.length

let gen_fields ctx out fields =
  if visible_fields fields = 0 then output_string out "unit"
  else Printf.fprintf out "{ %a}" (list (gen_field ctx)) fields

let gen_event_struct_field ctx out ident =
  Printf.fprintf out "%s of %a" (Ident.caml ident.id_name) (gen_ident ctx)
    { ident with id_name = Ident.snake ident.id_name ~suffix:"event" }

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
      Printf.fprintf out "let %s buf (v : %s) ~at : int option = %a;;\n"
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
        "let %s : int -> [> %s ] option = function %a | n -> Printf.printf \
         \"unknown %s: %%d\\n\" n; None;;\n"
        (Ident.snake name ~suffix:"enum_of_int")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_decode_enum_item)
        items
        (Ident.snake name ~suffix:"enum");
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
         function %a in mask_value_to_int to_mask to_enum mask;;"
        (Ident.snake name ~suffix:"int_of_mask")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_encode_enum_item)
        values
        (list_sep " | " gen_encode_mask_item)
        items
  | Mask { name; items; additional_values = None_value } ->
      Printf.fprintf out "type %s = [ %a ] list option [@@deriving sexp];;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items;
      Printf.fprintf out
        "let %s mask : %s option =\n\
         let of_mask = function %a | _ -> None in if mask = 0L then Some None \
         else (mask_of_int of_mask mask |> Option.map (fun m -> Some m));;\n"
        (Ident.snake name ~suffix:"mask_of_int64")
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " gen_decode_mask_item)
        items;
      Printf.fprintf out
        "let %s (mask : %s) : int = let to_mask = function %a in match mask \
         with None -> 0 | Some mask -> int_of_mask to_mask mask;;"
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
        "let %s tag buf ~at : (%s * int) option = match tag with %a | _ -> None"
        (Ident.snake name ~prefix:"decode" ~suffix:"variant")
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_decode_variant_item ctx))
        items
  | Event { name = "RedirectNotify" as name; fields; _ }
  | Event { name = "KeyPress" as name; fields; _ }
  | Event { name = "RawKeyPress" as name; fields; _ }
  | Event { name = "ButtonPress" as name; fields; _ }
  | Event { name = "RawButtonPress" as name; fields; _ }
  | Event { name = "TouchBegin" as name; fields; _ }
  | Event { name = "RawTouchBegin" as name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields
  | Event { name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields;
      Printf.fprintf out "let %s buf ~at : (%s * int) option = %a;;\n"
        (Ident.snake name ~prefix:"decode" ~suffix:"event")
        (Ident.snake name ~suffix:"event")
        (gen_decode_event_fields ctx)
        fields
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
      Printf.fprintf out "let %s %a buf ~at : int option =\n"
        (Ident.snake name ~prefix:"encode")
        (list (gen_named_arg ctx))
        fields;
      Printf.fprintf out "%a;;" (gen_encode_arg_fields ctx opcode) fields;
      reply
      |> Option.iter (function
           | fields when visible_fields fields = 0 ->
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
  | Event_copy { name; event; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_ident ctx)
        { event with id_name = Ident.snake event.id_name ~suffix:"event" }
  | Error_copy { name; error; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"error")
        (gen_ident ctx)
        { error with id_name = Ident.snake error.id_name ~suffix:"error" }
  | Event_struct { name; events } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;" (Ident.snake name)
        (list_sep " | " (gen_event_struct_field ctx))
        events

let gen_xcb xcbs out xcb =
  output_string out "[@@@warning \"-27\"]\n";
  output_string out "[@@@warning \"-11\"]\n";
  output_string out "open Codec\n";
  output_string out "open Sexplib.Conv\n";
  match xcb with
  | Core decls ->
      let ctx = Ctx.{ current_module = "xproto"; xcbs } in
      (list_sep "\n" (gen_declaration ctx)) out decls
  | Extension { declarations; name = _; file_name; _ } ->
      let ctx = Ctx.{ current_module = file_name; xcbs } in
      (list_sep "\n" (gen_declaration ctx)) out declarations

let gen out xcbs = List.iter (gen_xcb xcbs out) xcbs
