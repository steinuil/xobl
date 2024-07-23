module Cursor = struct
  type t = { buf : Bytes.t; mutable pos : int; start : int }

  let of_bytes ?(pos = 0) buf = { buf; pos; start = pos }
end

open Cursor

let[@inline] pad cur bytes = cur.pos <- cur.pos + bytes

let[@inline] align cur size =
  cur.pos <- cur.pos + ((cur.pos - cur.start) mod size)

let[@inline] decode f cur ~size =
  let v = f cur.buf cur.pos in
  pad cur size;
  v

let bool cur = decode Bytes.get ~size:1 cur <> '\000'
let i8 = decode Bytes.get_int8 ~size:1
let u8 = decode Bytes.get_uint8 ~size:1
let i16 = decode Bytes.get_int16_le ~size:2
let u16 = decode Bytes.get_uint16_le ~size:2
let i32 cur = decode Bytes.get_int32_le ~size:4 cur
let u32 cur = decode Bytes.get_int32_le ~size:4 cur
let i64 = decode Bytes.get_int64_le ~size:8
let u64 = decode Bytes.get_int64_le ~size:8
let float cur = i32 cur |> Int32.float_of_bits
let double cur = i64 cur |> Int64.float_of_bits
let char = decode Bytes.get ~size:1
let void = decode Bytes.get ~size:1
let byte = char
let file_descr cur = Types.File_descr (i16 cur)
let xid cur = Types.Xid (i32 cur)

let string ~len cur =
  let str = BytesLabels.sub_string cur.buf ~len ~pos:cur.pos in
  pad cur len;
  str

let utf16_string ~len cur =
  let str = BytesLabels.sub_string cur.buf ~len:(len * 2) ~pos:cur.pos in
  pad cur (len * 2);
  Types.Utf16_string str

let list ~item ~len cur =
  let rec loop ls len =
    if len = 0 then List.rev ls
    else
      let v = item cur in
      loop (v :: ls) (len - 1)
  in
  loop [] len
