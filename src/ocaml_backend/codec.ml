module Decode = struct
  module Buf = struct
    type t = { buf : Bytes.t; mutable pos : int; start : int }

    let of_bytes ?(pos = 0) buf = { buf; pos; start = pos }
  end

  open Buf

  let[@inline] pad buf bytes = buf.pos <- buf.pos + bytes

  let[@inline] align buf size =
    buf.pos <- buf.pos + ((buf.pos - buf.start) mod size)

  let%test "align when it is already aligned" =
    let b = Buf.of_bytes (Bytes.of_string "abcd") in
    b.pos <- 4;
    align b 4;
    b.pos = 4

  let[@inline] decode f buf ~size =
    let v = f buf.buf buf.pos in
    pad buf size;
    v

  let decode_bool buf = decode Bytes.get ~size:1 buf <> '\000'
  let decode_i8 = decode Bytes.get_int8 ~size:1
  let decode_u8 = decode Bytes.get_uint8 ~size:1
  let decode_i16 = decode Bytes.get_int16_le ~size:2
  let decode_u16 = decode Bytes.get_uint16_le ~size:2
  let decode_i32 = decode Bytes.get_int32_le ~size:4
  let decode_u32 = decode Bytes.get_int32_le ~size:4
  let decode_i64 = decode Bytes.get_int64_le ~size:8
  let decode_u64 = decode Bytes.get_int64_le ~size:8
  let decode_float buf = decode_i64 buf |> Int64.float_of_bits
  let decode_double = decode_float
  let decode_char = decode Bytes.get ~size:1
  let decode_void = decode Bytes.get ~size:1
  let decode_byte = decode_char
  let decode_file_descr buf = Types.File_descr (decode_i16 buf)
  let decode_xid buf = Types.Xid (decode_i32 buf)

  let decode_string ~len buf =
    let str = BytesLabels.sub_string buf.buf ~len ~pos:buf.pos in
    pad buf len;
    str

  let decode_utf16_string ~len buf =
    let str = BytesLabels.sub_string buf.buf ~len:(len * 2) ~pos:buf.pos in
    pad buf (len * 2);
    Types.Utf16_string str

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
end
