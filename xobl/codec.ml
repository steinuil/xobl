open Sexplib.Conv

let hexch =
  "000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F202122232425262728292A2B2C2D2E2F303132333435363738393A3B3C3D3E3F404142434445464748494A4B4C4D4E4F505152535455565758595A5B5C5D5E5F606162636465666768696A6B6C6D6E6F707172737475767778797A7B7C7D7E7F808182838485868788898A8B8C8D8E8F909192939495969798999A9B9C9D9E9FA0A1A2A3A4A5A6A7A8A9AAABACADAEAFB0B1B2B3B4B5B6B7B8B9BABBBCBDBEBFC0C1C2C3C4C5C6C7C8C9CACBCCCDCECFD0D1D2D3D4D5D6D7D8D9DADBDCDDDEDFE0E1E2E3E4E5E6E7E8E9EAEBECEDEEEFF0F1F2F3F4F5F6F7F8F9FAFBFCFDFEFF"

let hex_string_of_bytes bytes =
  let b = Bytes.make (Bytes.length bytes * 2) '0' in
  Bytes.iteri
    (fun i ch ->
      Bytes.set b (i * 2) hexch.[Char.code ch * 2];
      Bytes.set b ((i * 2) + 1) hexch.[(Char.code ch * 2) + 1])
    bytes;
  Bytes.unsafe_to_string b

type xid = int [@@deriving sexp]
type file_descr = int [@@deriving sexp]
type ('flags, 'vals) mask = F of 'flags | V of 'vals [@@deriving sexp]
type 't alt_enum = E of 't | Custom of int [@@deriving sexp]

let ( let* ) opt f =
  match opt with
  | Some o -> f o
  | None -> raise (Invalid_argument "option is none")

(* Option.bind *)
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

let decode_int32 buf ~at =
  decode Bytes.get_int32_le buf ~at ~size:4
  |> Option.map (fun (n, at) -> (Int32.to_int n, at))

let decode_int64 buf ~at = decode Bytes.get_int64_le buf ~at ~size:8

let decode_float buf ~at =
  decode_int64 buf ~at
  |> Option.map (fun (n, at) -> (Int64.float_of_bits n, at))

let decode_file_descr buf ~at =
  decode Bytes.get_int16_le buf ~at ~size:2
  |> Option.map (fun (n, at) -> ((Obj.magic n : Unix.file_descr), at))

let decode_xid = decode_int32

let decode_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) -> (
      match of_int (to_int n) with None -> None | Some e -> Some (e, at))

let decode_alt_enum decode to_int of_int buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) -> (
      let n = to_int n in
      match of_int n with
      | Some e -> Some (E e, at)
      | None -> Some (Custom n, at))

let decode_list decode_item len buf ~at =
  let rec loop items at len =
    if len = 0 then Some (List.rev items, at)
    else
      match decode_item buf ~at with
      | None -> raise (Invalid_argument (Printf.sprintf "%d" at))
      (* None *)
      | Some (item, at) -> loop (item :: items) at (len - 1)
  in
  loop [] at len

let decode_string len buf ~at =
  let str = Bytes.sub_string buf at len in
  Some (str, at + len)

let mask_of_int of_bit mask =
  let rec iter mask pos acc =
    if mask = 0L then Some acc
    else if Int64.logand mask 1L <> 0L then
      match of_bit pos with
      | Some item -> iter Int64.(shift_right mask 1) (pos + 1) (item :: acc)
      | None -> None
    else iter Int64.(shift_right mask 1) (pos + 1) acc
  in
  iter mask 0 []

let mask_value_of_int of_bit of_value mask =
  match of_value mask with
  | Some v -> Some (V v)
  | None -> (
      match mask_of_int of_bit mask with Some f -> Some (F f) | None -> None)

let decode_mask decode to_int64 of_int64 buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) -> (
      match of_int64 (to_int64 n) with None -> None | Some e -> Some (e, at))

let decode_alt_mask decode to_int64 of_int64 buf ~at =
  match decode buf ~at with
  | None -> None
  | Some (n, at) -> (
      match of_int64 (to_int64 n) with
      | Some e -> Some (F e, at)
      | None -> Some (V n, at))

let encode f buf v ~at ~size =
  if Bytes.length buf < at + size then None
  else (
    f buf at v;
    Some (at + size))

let encode_char buf v ~at = encode Bytes.set buf v ~at ~size:1
let encode_uint8 buf v ~at = encode Bytes.set_uint8 buf v ~at ~size:1
let encode_int8 buf v ~at = encode Bytes.set_int8 buf v ~at ~size:1
let encode_bool buf v ~at = encode_uint8 buf (if v then 1 else 0) ~at
let encode_uint16 buf v ~at = encode Bytes.set_uint16_le buf v ~at ~size:2
let encode_int16 buf v ~at = encode Bytes.set_int16_le buf v ~at ~size:2

let encode_int32 buf v ~at =
  encode Bytes.set_int32_le buf (Int32.of_int v) ~at ~size:4

let encode_int64 buf v ~at = encode Bytes.set_int64_le buf v ~at ~size:8
let encode_float buf v ~at = encode_int64 buf (Int64.bits_of_float v) ~at

let encode_file_descr buf (v : Unix.file_descr) ~at =
  encode_int16 buf (Obj.magic v) ~at

let encode_xid = encode_int32

let encode_list encode_item buf ls ~at =
  let rec loop at = function
    | [] -> Some at
    | item :: rest -> (
        match encode_item buf item ~at with
        | Some at -> loop at rest
        | None -> None)
  in
  loop at ls

let encode_string buf str ~at =
  let len = String.length str in
  Bytes.blit_string str 0 buf at len;
  Some (at + len)

let encode_enum encode of_int to_int buf v ~at =
  let v = to_int v in
  encode buf (of_int v) ~at

let encode_mask encode of_int to_int buf v ~at =
  let v = to_int v in
  encode buf (of_int v) ~at

let int_of_mask to_bit mask =
  List.fold_left (fun mask v -> mask lor (1 lsl to_bit v)) 0 mask

let mask_value_to_int to_mask to_enum = function
  | F f -> int_of_mask to_mask f
  | V v -> to_enum v

let encode_alt_enum encode of_int to_int buf v ~at =
  match v with
  | E v -> encode_enum encode of_int to_int buf v ~at
  | Custom v -> encode buf v ~at

(* TODO should we use an Int64 here? *)
let encode_optional_mask encode buf fields ~at =
  let rec loop mask = function
    | [] -> mask
    | (exists, pos) :: rest ->
        let mask = if exists then mask lor (1 lsl pos) else mask in
        loop mask rest
  in
  let mask = loop 0 fields in
  encode buf mask ~at
