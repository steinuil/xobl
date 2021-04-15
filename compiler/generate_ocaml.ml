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
            && ( Char.lowercase_ascii prev = prev
               || is_last_of_acronym name i len )
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

  let snake ?suffix name =
    ( match suffix with
    | Some suffix -> Casing.snake name ^ "_" ^ suffix
    | None ->
        let name = Casing.snake name in
        if List.mem name ocaml_reserved then name ^ "_" else name )
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

let gen_ident (curr_module, _) out { id_module; id_name } =
  if curr_module = id_module then output_string out (Ident.snake id_name)
  else
    Printf.fprintf out "%s.%s"
      (String.capitalize_ascii id_module)
      (Ident.snake id_name)

let gen_type ctx out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref ident -> gen_ident ctx out ident
  | Type_union _ -> output_string out "xid"

let gen_enum_item out (name, _) = output_string out (Ident.caml name)

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
  | Field_list { name; type_; _ } | Field_list_simple { name; type_ } ->
      Printf.fprintf out "%s : %a list; " (Ident.snake name)
        (gen_field_type ctx) type_
  | Field_variant { name; variant } ->
      Printf.fprintf out "%s : %a; " (Ident.snake name) (gen_ident ctx)
        { variant with id_name = Ident.snake variant.id_name ~suffix:"variant" }
  | Field_expr _ | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()

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
  | Field_list { name; type_; _ } | Field_list_simple { name; type_ } ->
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
      Printf.fprintf out "type %s = %a;;" (Ident.snake name) (gen_type ctx)
        type_
  | Struct { name; fields } ->
      Printf.fprintf out "type %s = %a;;" (Ident.snake name) (gen_fields ctx)
        fields
  | Enum { name; items } ->
      Printf.fprintf out "type %s = %a;;"
        (Ident.snake name ~suffix:"enum")
        (list_sep " | " gen_enum_item)
        items
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
        ( if Option.is_some reply then Ident.snake name ~suffix:"reply"
        else "unit" )
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
  | Mask { name; items; additional_values = [] } ->
      Printf.fprintf out "type %s = [ %a ] list;;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items
  | Mask { name; items; additional_values = values } ->
      Printf.fprintf out "type %s = ([ %a ], [ %a ]) mask;;"
        (Ident.snake name ~suffix:"mask")
        (list_sep " | " mask_item) items (list_sep " | " mask_item) values

let gen_xcb out = function
  | Core decls ->
      output_string out "module[@warning \"-27\"] Xproto = struct\n";
      let ctx = ("xproto", ()) in
      (list_sep "\n" (gen_declaration ctx)) out decls;
      output_string out "\nend\n"
  | Extension { declarations; name = _; file_name; _ } ->
      Printf.fprintf out "module[@warning \"-27\"] %s = struct\n"
        (String.capitalize_ascii file_name);
      let ctx = (file_name, ()) in
      (list_sep "\n" (gen_declaration ctx)) out declarations;
      output_string out "\nend\n"
