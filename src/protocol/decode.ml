module Buf = struct
  type t = { buf : Bytes.t; mutable pos : int; start : int }

  let of_bytes ?(pos = 0) buf = { buf; pos; start = pos }
end

open Buf

let[@inline] pad buf bytes = buf.pos <- buf.pos + bytes

let[@inline] align buf size =
  buf.pos <- buf.pos + ((buf.pos - buf.start) mod size)

(* let%test "align when it is already aligned" =
   let b = Buf.of_bytes (Bytes.of_string "abcd") in
   b.pos <- 4;
   align b 4;
   b.pos = 4 *)

let[@inline] decode f buf ~size =
  let v = f buf.buf buf.pos in
  pad buf size;
  v

let bool buf = decode Bytes.get ~size:1 buf <> '\000'
let i8 = decode Bytes.get_int8 ~size:1
let u8 = decode Bytes.get_uint8 ~size:1
let i16 = decode Bytes.get_int16_le ~size:2
let u16 = decode Bytes.get_uint16_le ~size:2
let i32 buf = decode Bytes.get_int32_le ~size:4 buf
let u32 buf = decode Bytes.get_int32_le ~size:4 buf
let i64 = decode Bytes.get_int64_le ~size:8
let u64 = decode Bytes.get_int64_le ~size:8
let float buf = i64 buf |> Int64.float_of_bits
let double = float
let char = decode Bytes.get ~size:1
let void = decode Bytes.get ~size:1
let byte = char
let file_descr buf = Types.File_descr (i16 buf)
let xid buf = Types.Xid (i32 buf)

let string ~len buf =
  let str = BytesLabels.sub_string buf.buf ~len ~pos:buf.pos in
  pad buf len;
  str

let utf16_string ~len buf =
  let str = BytesLabels.sub_string buf.buf ~len:(len * 2) ~pos:buf.pos in
  pad buf (len * 2);
  Types.Utf16_string str

let list ~item ~len buf =
  let rec loop ls len =
    if len = 0 then List.rev ls
    else
      let v = item buf in
      loop (v :: ls) (len - 1)
  in
  loop [] len

(* let[@inline] identity x = x *)

(* let decode_enum :
        't.
        decode:(Buf.t -> 't) ->
        int_of_t:('t -> int) ->
        enum_of_int:(int -> 'enum) ->
        Buf.t ->
        'enum =
   fun ~decode ~int_of_t ~enum_of_int buf ->
    let t = decode buf in
    enum_of_int (int_of_t t) *)
