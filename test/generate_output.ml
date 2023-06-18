let modules =
  [
    (* "bigreq"; *)
    (* "composite"; *)
    (* "damage"; *)
    (* "dpms"; *)
    (* "dri2"; *)
    (* "dri3"; *)
    (* "ge"; *)
    (* "glx"; *)
    (* "present"; *)
    (* "randr"; *)
    (* "record"; *)
    (* "render"; *)
    (* "res"; *)
    (* "screensaver"; *)
    (* "shape"; *)
    (* "shm"; *)
    (* "sync"; *)
    (* "xc_misc"; *)
    (* (* "xevie"; *) *)
    (* "xf86dri"; *)
    (* "xf86vidmode"; *)
    (* "xfixes"; *)
    (* "xinerama"; *)
    (* "xinput"; *)
    (* (* "xkb"; *) *)
    (* "xprint"; *)
    "xproto";
    (* "xselinux"; *)
    (* "xtest"; *)
    (* "xv"; *)
    (* "xvmc"; *)
  ]

let parse_module m =
  let f = open_in (Printf.sprintf "../xml-xcb/%s.xml" m) in
  try
    let m = Xobl_compiler.Parser.parse (`Channel f) |> Result.get_ok in
    close_in f;
    m
  with exn ->
    close_in f;
    raise exn

let sort_topological (nodes : (string * string list) list) =
  let rec dfs out (node, dependencies) =
    if List.mem node out then out
    else
      node
      :: List.fold_left
           (fun out node ->
             let dependencies = List.assoc node nodes in
             dfs out (node, dependencies))
           out dependencies
  in
  List.fold_left dfs [] nodes |> List.rev

let sort_xcbs xcbs =
  xcbs
  |> List.map (function
       | Xobl_compiler__Elaboratetree.Core _ -> ("xproto", [])
       | Extension { file_name; imports; _ } -> (file_name, imports))
  |> sort_topological
  |> List.map (fun name ->
         xcbs
         |> List.find (function
              | Xobl_compiler__Elaboratetree.Core _ when name = "xproto" -> true
              | Extension { file_name; _ } when file_name = name -> true
              | _ -> false))

let () =
  let xcbs =
    modules |> List.map parse_module
    |> List.map Xobl_compiler.Elaborate.unions_to_switches
    |> Xobl_compiler.Elaborate.resolve_idents
  in
  output_string stdout
    {|type xid = int
type file_descr = int
type ('flags, 'vals) mask = F of 'flags | V of 'vals
type 't alt_enum = E of 't | Custom of int
let (let*) = Option.bind

let identity x = x

let char_to_int64 c = Char.code c |> Int64.of_int

let bool_to_int64 b = Bool.to_int b |> Int64.of_int

let bool_of_int b = if b then 1 else 0

let decode f buf ~at ~size =
  if Bytes.length buf < at + size - 1 then None else Some (f buf at, at + size)

let decode_char buf ~at = decode Bytes.get buf ~at ~size:1

let decode_uint8 buf ~at = decode Bytes.get_uint8 buf ~at ~size:1

let decode_int8 buf ~at = decode Bytes.get_int8 buf ~at ~size:1

let decode_bool buf ~at =
  decode_uint8 buf ~at |> Option.map (fun (n, at) -> (n <> 0, at))

let decode_uint16 buf ~at = decode Bytes.get_uint16_le buf ~at ~size:2

let decode_int16 buf ~at = decode Bytes.get_int16_le buf ~at ~size:2

let decode_int32 buf ~at = decode Bytes.get_int32_le buf ~at ~size:4 |> Option.map (fun (n, at) -> (Int32.to_int n, at))

let decode_int64 buf ~at = decode Bytes.get_int64_le buf ~at ~size:8

let decode_float buf ~at =
  decode_int64 buf ~at
  |> Option.map (fun (n, at) -> (Int64.float_of_bits n, at))

let decode_file_descr buf ~at =
  decode Bytes.get_int16_le buf ~at ~size:2
  |> Option.map (fun (n, at) -> ((Obj.magic n : Unix.file_descr), at))

let decode_xid = decode_int16

let decode_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    match of_int (to_int n) with
    | None -> None
    | Some e -> Some (e, at)

let decode_alt_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    let n = to_int n in
    match of_int n with
    | Some e -> Some (E e, at)
    | None -> Some (Custom n, at)

let decode_list decode_item len buf ~at =
  let rec loop items at len =
    if len = 0 then Some ((List.rev items), at)
    else
      match decode_item buf ~at with
      | None -> None
      | Some (item, at) ->
        loop (item :: items) at (len - 1)
  in
  loop [] at len

let mask_of_int of_bit mask =
  let rec iter mask pos acc =
    if mask = 0L then Some acc
    else if Int64.logand mask 1L <> 0L then
      match of_bit pos with
      | Some item -> iter Int64.(shift_right mask 1) Int64.(pos + 1) (item :: acc)
      | None -> None
    else iter Int64.(shift_right mask 1) Int64.(pos + 1) acc
  in
  iter mask 0 []

let mask_value_of_int of_bit of_value mask =
  match of_value mask with
  | Some v -> Some (V v)
  | None ->
    match mask_of_int of_bit mask with
    | Some f -> Some (F f)
    | None -> None

let decode_mask decode to_int64 of_int64 buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    match of_int64 (to_int64 n) with
    | None -> None
    | Some e -> Some (e, at)

let decode_alt_mask decode to_int64 of_int64 buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    match of_int64 (to_int64 n) with
    | Some e -> Some (F e, at)
    | None -> Some (V n, at)

let encode f buf v ~at ~size =
  if Bytes.length buf < at + size then None else (
    f buf at v;
    Some (at + size)
  )

let encode_char buf v ~at = encode Bytes.set buf  v ~at ~size:1

let encode_uint8 buf v ~at = encode Bytes.set_uint8 buf v ~at ~size:1

let encode_int8 buf v ~at = encode Bytes.set_int8 buf v ~at ~size:1

let encode_bool buf v ~at = encode_uint8 buf (if v then 1 else 0) ~at

let encode_uint16 buf v ~at = encode Bytes.set_uint16_le buf v ~at ~size:2

let encode_int16 buf v ~at = encode Bytes.set_int16_le buf v ~at ~size:2

let encode_int32 buf v ~at = encode Bytes.set_int32_le buf (Int32.of_int v) ~at ~size:4

let encode_int64 buf v ~at = encode Bytes.set_int64_le buf v ~at ~size:8

let encode_float buf v ~at =
  encode_int64 buf (Int64.bits_of_float v) ~at

let encode_file_descr buf (v : Unix.file_descr) ~at =
  encode_int16 buf (Obj.magic v) ~at

let encode_xid = encode_int16

let encode_list encode_item buf ls ~at =
  let rec loop at = function
    | [] -> Some at
    | item :: rest -> (
      match encode_item buf item ~at with
      | Some at -> loop at rest
      | None -> None )
  in
  loop at ls

let encode_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) ->
    match of_int (to_int n) with
    | None -> None
    | Some e -> Some (e, at)

let encode_enum encode of_int to_int buf v ~at =
  let v = to_int v in
  encode buf (of_int v) ~at

let encode_mask encode of_int to_int buf v ~at =
  let v = to_int v in
  encode buf (of_int v) ~at

let int_of_mask to_bit mask =
  List.fold_left (fun mask v -> mask lor (to_bit v)) 0 mask

let mask_value_to_int to_mask to_enum = function
  | F f -> int_of_mask to_mask f
  | V v -> to_enum v

let encode_alt_enum encode of_int to_int buf v ~at =
  match v with
  | E v -> encode_enum encode of_int to_int buf v ~at
  | Custom v -> encode buf v ~at

|};
  List.map (Xobl_compiler.Elaborate.do_stuff xcbs) xcbs
  |> sort_xcbs
  |> Xobl_compiler__.Generate_ocaml.gen stdout
