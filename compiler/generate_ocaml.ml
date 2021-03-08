open Elaboratetree

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

let gen_ident out { id_module; id_name } =
  Printf.fprintf out "%s.%s" id_module id_name

let gen_type out = function
  | Type_primitive prim -> output_string out @@ gen_prim prim
  | Type_ref ident -> gen_ident out ident
  | Type_union _ -> output_string out "xid"

let gen_enum_item out (name, _) = output_string out name

(* let gen_fields out =

let gen_variant_items out =
  List.iter (fun { vi_name; vi_tag = _; vi_fields } ->
      Printf.fprintf "| %s of { %a } " vi_name gen_fields vi_fields) *)

let gen_field_type out = function
  | { ft_type; ft_allowed = None } -> gen_type out ft_type
  | { ft_type; ft_allowed = Some _ } -> gen_type out ft_type

let gen_field out = function
  | Field { name; type_ } ->
      Printf.fprintf out "%s : %a; " name gen_field_type type_
  | Field_file_descriptor name -> Printf.fprintf out "%s : file_descr; " name
  | Field_optional { name; type_; _ } ->
      Printf.fprintf out "%s : %a option; " name gen_field_type type_
  | Field_list { name; type_; _ } | Field_list_simple { name; type_ } ->
      Printf.fprintf out "%s : %a list; " name gen_field_type type_
  | Field_pad _ | Field_list_length _ | Field_variant_tag _
  | Field_optional_mask _ ->
      ()
  | _ -> ()

let gen_variant_item out { vi_name; vi_tag = _; vi_fields } =
  Printf.fprintf out "%s of { %a}" vi_name (list gen_field) vi_fields

let gen_declaration out = function
  | Type_alias { name; type_ } ->
      Printf.fprintf out "type %s = %a;;" name gen_type type_
  | Struct { name; fields } ->
      Printf.fprintf out "type %s = { %a};;" name (list gen_field) fields
  | Enum { name; items } ->
      Printf.fprintf out "type %s = %a;;" name
        (list_sep " | " gen_enum_item)
        items
  | Variant { name; items } ->
      Printf.fprintf out "type %s = %a;;" name
        (list_sep " | " gen_variant_item)
        items
  | Event { name; fields; _ } ->
      Printf.fprintf out "type %s = { %a};;" name (list gen_field) fields
  | Error { name; fields; _ } ->
      Printf.fprintf out "type %s = { %a};;" name (list gen_field) fields
  | Request { name; fields; _ } ->
      Printf.fprintf out "type %s = { %a};;" name (list gen_field) fields
  | _ -> ()

let gen_xcb out = function
  | Core decls ->
      output_string out "module Xproto = struct";
      (list_sep "\n" gen_declaration) out decls;
      output_string out "end"
  | Extension { declarations; name; _ } ->
      Printf.fprintf out "module %s = struct\n" name;
      (list_sep "\n" gen_declaration) out declarations;
      output_string out "\nend\n"
