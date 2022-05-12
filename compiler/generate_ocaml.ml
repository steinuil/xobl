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
  | Int32 -> "int32"
  | Fd -> "file_descr"
  | Card8 -> "int"
  | Card16 -> "int"
  | Card32 -> "int32"
  | Card64 -> "int64"
  | Float -> "float"
  | Double -> "float"
  | Xid -> "xid"

let gen_to_int = function
  | Char | Byte -> Some "Char.code"
  | Bool -> Some "Bool.to_int"
  | Int8 | Int16 | Card8 | Card16 | Xid -> None
  | Fd -> Some "Obj.magic"
  | Int32 | Card32 -> Some "Int32.to_int"
  | Card64 -> Some "Int64.to_int"
  | Void | Float | Double -> failwith "a"

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
  | Void -> "failwith \"a\""
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

let gen_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Div -> "/"
  | Mul -> "*"
  | Bit_and -> "land"
  | Bit_left_shift -> "lsl"

let rec gen_expr out = function
  | Binop (op, e1, e2) ->
      Printf.fprintf out "(%a) %s (%a)" gen_expr e1 (gen_binop op) gen_expr e2
  | Unop (Bit_not, e) -> Printf.fprintf out "lnot (%a)" gen_expr e
  | Field_ref f -> output_string out (Ident.snake f)
  | Expr_value v -> Printf.fprintf out "%Ld" v
  | Expr_bit b -> Printf.fprintf out "1 lsl %d" b
  | Param_ref _ | Enum_ref _ | Pop_count _ | Sum_of _ | List_element_ref ->
      failwith "a"

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

let gen_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref ident -> gen_ident ctx out ident
  | Type_union _ -> output_string out "xid"

let gen_decode_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_decode_prim prim
  | Type_ref ident -> gen_decode_ident ctx out ident
  | Type_union _ -> output_string out @@ gen_decode_prim Xid

let gen_enum_item out (name, _) = Printf.fprintf out "`%s" (Ident.caml name)

let gen_decode_enum_item out (name, v) =
  Printf.fprintf out "%Ld -> Some `%s" v (Ident.caml name)

let gen_field_type ctx out = function
  | { ft_type; ft_allowed = None } -> gen_type ctx out ft_type
  | { ft_type = _; ft_allowed = Some (Allowed_enum enum) } ->
      gen_ident ctx out
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
  | { ft_type = _; ft_allowed = Some (Allowed_mask mask) } ->
      gen_ident ctx out
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
  | { ft_type; ft_allowed = Some (Allowed_alt_enum enum) } ->
      Printf.fprintf out "(%a, %a) alt" (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum" }
        (gen_type ctx) ft_type
  | { ft_type; ft_allowed = Some (Allowed_alt_mask mask) } ->
      Printf.fprintf out "(%a, %a) alt" (gen_ident ctx)
        { mask with id_name = Ident.snake mask.id_name ~suffix:"mask" }
        (gen_type ctx) ft_type

let gen_field ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "%s : file_descr; " (Ident.snake name)
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "%s : %a option; " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "%s : %a list; " (Ident.snake name)
        (gen_field_type ctx) type_
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
        (Option.value ~default:"(fun x -> x)" (gen_to_int p))
        (gen_ident ctx)
        { enum with id_name = Ident.snake enum.id_name ~suffix:"enum_of_int" }
  | { ft_type; ft_allowed = None } | { ft_type; ft_allowed = _ } ->
      gen_decode_type ctx out ft_type

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
  | Field_list_length { name; type_; expr } ->
      Printf.fprintf out "let* %s, at = %a buf ~at in " (Ident.snake name)
        (gen_decode_type ctx) type_;
      Option.iter
        (fun to_int ->
          Printf.fprintf out "let %s = %s %s in " (Ident.snake name) to_int
            (Ident.snake name))
        (resolve_as_prim ctx type_ |> gen_to_int);
      Printf.fprintf out "let %s = %a in" (Ident.snake name) gen_expr expr
  | Field_list_simple { name; type_; length } ->
      Printf.fprintf out "let* %s, at = decode_list %a %s buf ~at in"
        (Ident.snake name)
        (gen_decode_field_type ctx)
        type_ (Ident.snake length)
  | Field_expr _ | Field_list _ | Field_variant _ | Field_variant_tag _
  | Field_optional _ | Field_optional_mask _ ->
      ()

(* | Field_optional { name; type_; _ } *)

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
  list_sep " " (gen_decode_field ctx fields) out fields;
  Printf.fprintf out " ignore orig; Some ({ %s }, at)"
    (List.filter_map name_of_field fields
    |> List.map Ident.snake |> String.concat "; ")

(* output_string out " None" *)

let gen_variant_item ctx out { vi_name; vi_tag = _; vi_fields } =
  Printf.fprintf out "%s of { %a}" (Ident.caml vi_name)
    (list (gen_field ctx))
    vi_fields

let gen_named_arg ctx out = function
  | Field { name; type_ } ->
      Printf.fprintf out "~(%s : %a) " (Ident.snake name) (gen_field_type ctx)
        type_
  | Field_file_descriptor name ->
      Printf.fprintf out "~(%s : file_descr) " (Ident.snake name)
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "?(%s : %a option) " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_; _ } ->
      Printf.fprintf out "~(%s : %a list) " (Ident.snake name)
        (gen_field_type ctx) type_
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
      Printf.fprintf out "type %s = %a;;\n" (Ident.snake name) (gen_type ctx)
        type_;
      Printf.fprintf out "let %s = %a;;"
        (Ident.snake ~prefix:"decode" name)
        (gen_decode_type ctx) type_
  | Struct { name; fields } ->
      Printf.fprintf out "type %s = %a;;\n" (Ident.snake name) (gen_fields ctx)
        fields;
      Printf.fprintf out "let %s buf ~at : (%s * int) option = %a;;"
        (Ident.snake ~prefix:"decode" name)
        (Ident.snake name) (gen_decode_fields ctx) fields
  | Enum { name; items } ->
      Printf.fprintf out "type %s = [ %a ];;\n"
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_enum_item)
        items;
      Printf.fprintf out "let %s : int -> %s option = function %a | _ -> None;;"
        (Ident.snake name ~suffix:"enum_of_int")
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_decode_enum_item)
        items
  | Mask { name; items; additional_values = [] } ->
      Printf.fprintf out "type %s = [ %a ] list;;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items
  | Mask { name; items; additional_values = values } ->
      Printf.fprintf out "type %s = ([ %a ], [ %a ]) mask;;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items (list_sep " | " mask_item) values
  | Variant { name; items } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"variant")
        (list_sep " | " (gen_variant_item ctx))
        items
  | Event { name; fields; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"event")
        (gen_fields ctx) fields
  | Error { name; fields; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"error")
        (gen_fields ctx) fields
  | Request { name; fields; reply; _ } ->
      Option.iter
        (Printf.fprintf out "type %s = %a;;\n"
           (Ident.snake name ~suffix:"reply")
           (gen_fields ctx))
        reply;
      Printf.fprintf out
        "let %s %a() : %s Lwt.t = failwith \"not implemented\";;"
        (Ident.snake name)
        (list (gen_named_arg ctx))
        fields
        (if Option.is_some reply then Ident.snake name ~suffix:"reply"
        else "unit")
  | Event_copy { name; event; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"event")
        (gen_ident ctx)
        { event with id_name = Ident.snake event.id_name ~suffix:"event" }
  | Error_copy { name; error; _ } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"error")
        (gen_ident ctx)
        { error with id_name = Ident.snake error.id_name ~suffix:"error" }
  | Event_struct { name; events } ->
      Printf.fprintf out "type %s = %a;;" (Ident.snake name)
        (list_sep " | " (gen_event_struct_field ctx))
        events

let gen_xcb xcbs out = function
  | Core decls ->
      output_string out "module[@warning \"-27\"] Xproto = struct\n";
      let ctx = ("xproto", xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out decls;
      output_string out "\nend\n"
  | Extension { declarations; name = _; file_name; _ } ->
      Printf.fprintf out "module[@warning \"-27\"] %s = struct\n"
        (String.capitalize_ascii file_name);
      let ctx = (file_name, xcbs) in
      (list_sep "\n" (gen_declaration ctx)) out declarations;
      output_string out "\nend\n"

let gen out xcbs = List.iter (gen_xcb xcbs out) xcbs
