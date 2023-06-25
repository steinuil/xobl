open Hir

let find_module xcbs n =
  xcbs
  |> List.find_map (function
       | Core declarations when n = "xproto" -> Some declarations
       | Extension { file_name; declarations; _ } when file_name = n ->
           Some declarations
       | _ -> None)
  |> Option.get

let rec resolve_type xcbs { id_module; id_name } :
    [ `Prim of prim | `Struct of field list | `Event_struct of ident list ] =
  find_module xcbs id_module
  |> List.find_map (function
       | Type_alias { name; type_ = Type_primitive prim } when name = id_name ->
           Some (`Prim prim)
       | Type_alias { name; type_ = Type_union _ } when name = id_name ->
           Some (`Prim Xid)
       | Type_alias { name; type_ = Type_ref id } when name = id_name ->
           Some (resolve_type xcbs id)
       | Struct { name; fields } when name = id_name -> Some (`Struct fields)
       | Event_struct { name; events } when name = id_name ->
           Some (`Event_struct events)
       | _ -> None)
  |> Option.get

let of_prim = function
  | Char | Byte | Bool | Int8 | Card8 -> 1
  | Int16 | Card16 | Fd -> 2
  | Int32 | Card32 | Float | Xid -> 4
  | Card64 | Double -> 8
  | Void -> 1

let rec of_type xcbs = function
  | Type_primitive prim -> of_prim prim
  | Type_union _ -> of_prim Xid
  | Type_ref ident -> (
      match resolve_type xcbs ident with
      | `Prim prim -> of_prim prim
      | `Struct _ | `Event_struct _ -> failwith "a")

and of_field_type xcbs { ft_type; _ } = of_type xcbs ft_type

and of_fields xcbs _parents fields =
  fields
  |> List.map (function
       | Field { type_; _ } | Field_expr { type_; _ } ->
           of_field_type xcbs type_
       | Field_file_descriptor _ -> of_prim Fd
       | Field_pad { pad = Pad_bytes n; _ } -> n
       | Field_pad { pad = Pad_align _; _ } -> failwith "a"
       | _ -> failwith "a")

(* this approach doesn't work, because the size of some structs can only be
   known at runtime *)

(* and of_field xcbs = function
   | Field { type_; _ } | Field_expr { type_; _ } -> of_field_type xcbs type_
   | Field_file_descriptor _ -> of_prim Fd *)
