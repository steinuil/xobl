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

let identity x = x
let char_to_int64 c = Char.code c |> Int64.of_int
let bool_to_int64 b = Bool.to_int b |> Int64.of_int
let bool_of_int b = if b then 1 else 0

let sum_of_expr get_field =
  List.fold_left (fun acc elem -> acc + get_field elem) 0

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
  | Some v -> Some (X11_types.V v)
  | None -> (
      match mask_of_int of_bit mask with Some f -> Some (F f) | None -> None)

let int_of_mask to_bit mask =
  List.fold_left (fun mask v -> mask lor (1 lsl to_bit v)) 0 mask

let int_of_mask_value to_mask to_enum = function
  | X11_types.F f -> int_of_mask to_mask f
  | V v -> to_enum v
