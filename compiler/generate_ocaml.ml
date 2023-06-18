open Elaboratetree

module Casing = struct
  let is_last_of_acronym name pos len =
    if pos < len - 1 then
      let next = name.[pos + 1] in
      (next < '0' || next > '9') && Char.lowercase_ascii next = next
    else false

  let%test "is_last_of_acronym example" = is_last_of_acronym "DRI2Buffer" 5 10

  let%test "is_last_of_acronym example 2" =
    not (is_last_of_acronym "DRI2Buffer" 3 10)

  let snek name =
    let buf = Buffer.create 16 in
    let len = String.length name in
    StringLabels.iteri name ~f:(fun i -> function
      | c when i = 0 -> Buffer.add_char buf (Char.lowercase_ascii c)
      | 'A' .. 'Z' as c ->
          let prev = name.[i - 1] in
          if
            prev <> '_'
            && (Char.lowercase_ascii prev = prev
               || is_last_of_acronym name i len)
          then Buffer.add_char buf '_';
          Buffer.add_char buf (Char.lowercase_ascii c)
      | c -> Buffer.add_char buf c);
    Buffer.contents buf

  let snake name =
    if name = "DECnet" then "decnet"
    else if String.lowercase_ascii name = name then name
    else if String.uppercase_ascii name = name then String.lowercase_ascii name
    else snek name

  let%test "C case" = snake "bigreq" = "bigreq"
  let%test "UPPERCASE" = snake "CHAR2B" = "char2b"
  let%test "snake_case" = snake "bits_per_rgb_value" = "bits_per_rgb_value"
  let%test "CamelCase" = snake "StaticGray" = "static_gray"
  let%test "weird case 1" = snake "GLXContext" = "glx_context"
  let%test "weird case 2" = snake "DECnet" = "decnet"
  let%test "weird case 3" = snake "Positive_HSync" = "positive_h_sync"
  let%test "weird case 4" = snake "DRI2Buffer" = "dri2_buffer"
  let%test "weird case 5" = snake "TestStriGS" = "test_stri_gs"
  let caml name = String.capitalize_ascii (snake name)
end

module Ident = struct
  let ocaml_reserved =
    [
      "and";
      "as";
      "asr";
      "assert";
      "begin";
      "class";
      "constraint";
      "do";
      "done";
      "downto";
      "else";
      "end";
      "exception";
      "external";
      "false";
      "for";
      "fun";
      "function";
      "functor";
      "if";
      "in";
      "include";
      "inherit";
      "initializer";
      "land";
      "lazy";
      "let";
      "lor";
      "lsl";
      "lsr";
      "lxor";
      "match";
      "method";
      "mod";
      "module";
      "open";
      "mutable";
      "new";
      "nonrec";
      "object";
      "of";
      "open";
      "open!";
      "or";
      "private";
      "rec";
      "sig";
      "struct";
      "then";
      "to";
      "true";
      "try";
      "type";
      "val";
      "virtual";
      "when";
      "while";
      "with";
    ]

  let sanitize_numbers name =
    if name.[0] >= '0' && name.[0] <= '9' then "D" ^ name else name

  let snake ?prefix ?suffix name =
    (match (prefix, suffix) with
    | Some prefix, Some suffix ->
        prefix ^ "_" ^ Casing.snake name ^ "_" ^ suffix
    | Some prefix, None -> prefix ^ "_" ^ Casing.snake name
    | None, Some suffix when String.ends_with ~suffix (Casing.snake name) ->
        let name = Casing.snake name in
        if List.mem name ocaml_reserved then name ^ "_" else name
    | None, Some suffix -> Casing.snake name ^ "_" ^ suffix
    | None, None ->
        let name = Casing.snake name in
        if List.mem name ocaml_reserved then name ^ "_" else name)
    |> sanitize_numbers

  let caml name = Casing.caml name |> sanitize_numbers
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

let find_module_by_name xcbs name =
  List.find_map
    (function
      | Core decls when name = "xproto" -> Some (decls, [])
      | Extension { file_name; declarations; imports; _ } when file_name = name
        ->
          Some (declarations, imports)
      | _ -> None)
    xcbs
  |> Option.get

let find_prim name_to_find = function
  | Type_alias { name; type_ = Type_primitive prim } when name = name_to_find ->
      Some (`Prim prim)
  | Type_alias { name; type_ = Type_union _ } when name = name_to_find ->
      Some (`Prim Xid)
  | Type_alias { name; type_ = Type_ref ident } when name = name_to_find ->
      Some (`Ref ident)
  | _ -> None

let rec resolve_type_to_prim (current_module, xcbs) name =
  let decls, _ = find_module_by_name xcbs current_module in
  match List.find_map (find_prim name) decls |> Option.get with
  | `Prim p -> p
  | `Ref { id_module; id_name } ->
      resolve_type_to_prim (id_module, xcbs) id_name

let resolve_as_prim (_, xcbs) = function
  | Type_primitive prim -> prim
  | Type_ref ident -> resolve_type_to_prim (ident.id_module, xcbs) ident.id_name
  | Type_union _ -> Xid

let gen_decode_prim = function
  (* | Void -> "failwith \"a\"" *)
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

let gen_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then output_string out (Ident.snake id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake id_name)

let gen_decode_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then
    output_string out (Ident.snake ~prefix:"decode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"decode" id_name)

let gen_encode_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then
    output_string out (Ident.snake ~prefix:"encode" id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake ~prefix:"encode" id_name)

let gen_size_of_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then
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
  match resolve_as_prim ctx t.ft_type with
  | Char | Void -> output_string out "string"
  | (exception Invalid_argument _) | _ ->
      Printf.fprintf out "%a list" (gen_field_type ctx) t

let gen_list_length ctx out t =
  match resolve_as_prim ctx t with
  | Char | Void -> output_string out "String.length"
  | exception Invalid_argument _ ->
      output_string out "(* invalid_argument *) List.length"
  | t ->
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
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "decode_enum %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "decode_alt_enum %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "decode_mask %a %s %a" (gen_decode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "decode_alt_mask %a %s %a" (gen_decode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_to_int64 p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask_of_int64" }
  | { ft_type; ft_allowed = None } -> gen_decode_type ctx out ft_type

let gen_encode_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_encode_type ctx out ft_type
  | { ft_type; ft_allowed = Some (Allowed_enum enum) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "encode_enum %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some (Allowed_mask mask) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "encode_mask %a %s %a" (gen_encode_type ctx) ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"int_of_mask" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      let p = resolve_as_prim ctx ft_type in
      Printf.fprintf out "encode_alt_enum %a %s %a" (gen_encode_type ctx)
        ft_type
        (Option.value ~default:"identity" (gen_of_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"int_of_enum" }
  | { ft_type; ft_allowed = Some _ } -> gen_encode_type ctx out ft_type

let gen_encode_list ctx out t =
  match resolve_as_prim ctx t.ft_type with
  | Char | Void -> output_string out "encode_string"
  | (exception Invalid_argument _) | _ ->
      Printf.fprintf out "encode_list %a" (gen_encode_field_type ctx) t

let gen_decode_list ctx out t =
  match resolve_as_prim ctx t.ft_type with
  | Char | Void -> output_string out "decode_string"
  | (exception Invalid_argument _) | _ ->
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
        (resolve_as_prim ctx type_ |> gen_to_int);
      Printf.fprintf out "let %s = %a in" (Ident.snake name) (gen_expr None)
        expr
  | Field_list_simple { name; type_; length } ->
      Printf.fprintf out "let* %s, at = %a %s buf ~at in" (Ident.snake name)
        (gen_decode_list ctx) type_ (Ident.snake length)
  | Field_optional_mask { name; type_ } ->
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
  | Field_list { name; type_ = _; length = None } -> failwith name
  | Field_expr _ | Field_variant _ | Field_variant_tag _ -> ()

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
           (gen_of_int (resolve_as_prim ctx type_)))
        (gen_list_length ctx) list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf v.%s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf v.%s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_variant _ -> Printf.fprintf out "(* field_variant *)"
  | Field_variant_tag _ -> Printf.fprintf out "(* field_variant_tag *)"
  | Field_optional _ -> Printf.fprintf out "(* field_optional *)"
  | Field_optional_mask _ -> Printf.fprintf out "(* field_optional_mask *)"

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
      (* Can be wrong *)
      (* Printf.fprintf out "let* at = %a buf (List.length %s) ~at in"
         (gen_encode_type ctx) type_ (Ident.snake list) *)
      Printf.fprintf out "let* at = %a buf (%s (%a %s)) ~at in"
        (gen_encode_type ctx) type_
        (Option.value ~default:"identity"
           (gen_of_int (resolve_as_prim ctx type_)))
        (gen_list_length ctx) list_type (Ident.snake list)
  | Field_list { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf %s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "let* at = %a buf %s ~at in" (gen_encode_list ctx)
        type_ (Ident.snake name)
  | Field_variant _ -> Printf.fprintf out "(* field_variant *)"
  | Field_variant_tag _ -> Printf.fprintf out "(* field_variant_tag *)"
  | Field_optional _ -> Printf.fprintf out "(* field_optional *)"
  | Field_optional_mask { type_; _ } ->
      Printf.fprintf out "(* field_optional_mask *)";
      Printf.fprintf out "let* at = %a buf 0 ~at in" (gen_encode_type ctx) type_

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
  Printf.fprintf out "\nignore orig;\nSome ({ %s }, at)"
    (List.filter_map name_of_field fields
    |> List.map Ident.snake |> String.concat "; ")

let gen_encode_fields ctx out fields =
  output_string out "let orig = at in ";
  list_sep " " (gen_encode_field ctx) out fields;
  Printf.fprintf out " ignore orig; Some at"

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
  | [] -> ());
  output_string out
    "\n\
     (* write request length *)\n\
     let* _ = encode_uint16 buf (at / 4) ~at:(orig + 2) in\n";
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
  | Mask { name; items; additional_values = [] } ->
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
  | Mask { name; items; additional_values = values } ->
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
  | Variant { name; items } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_variant_item ctx))
        items
  | Event { name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields
  | Error { name; fields; _ } ->
      Printf.fprintf out "type %s = %a [@@deriving sexp];;"
        (Ident.snake name ~suffix:"error")
        (gen_fields ctx) fields
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
      (* (if Option.is_some reply then Ident.snake name ~suffix:"reply"
         else "unit"); *)
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

let gen_xcb xcbs out = function
  | Core decls ->
      (* output_string out "module[@warning \"-27,-11\"] Xproto = struct\n"; *)
      output_string out "open Sexplib.Conv\n\n";
      let ctx = ("xproto", xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out decls
      (* output_string out "\nend\n" *)
  | Extension { declarations; name = _; file_name; _ } ->
      Printf.fprintf out "module[@warning \"-27\"] %s = struct\n"
        (String.capitalize_ascii file_name);
      let ctx = (file_name, xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out declarations;
      output_string out "\nend\n"

let gen out xcbs = List.iter (gen_xcb xcbs out) xcbs
