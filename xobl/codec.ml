open Sexplib.Conv
open X11_types

let ( let* ) opt f =
  match opt with
  | Some o -> f o
  | None -> raise (Invalid_argument "option is none")

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

module Encode_buffer = struct
  type t = { buffer : Buffer.t; offset : int }

  let of_buffer buffer =
    let offset = Buffer.length buffer in
    { buffer; offset }

  (* Current offset relative to the buffer initial offset. *)
  let current_offset buf = Buffer.length buf.buffer - buf.offset
end

let encode f (buf : Encode_buffer.t) v = f buf.buffer v
let encode_char buf v = encode Buffer.add_char buf v
let encode_uint8 buf v = encode Buffer.add_uint8 buf v
let encode_int8 buf v = encode Buffer.add_uint8 buf v
let encode_bool buf v = encode_uint8 buf (if v then 1 else 0)
let encode_uint16 buf v = encode Buffer.add_uint16_le buf v
let encode_int16 buf v = encode Buffer.add_int16_le buf v
let encode_int32 buf v = encode Buffer.add_int32_le buf (Int32.of_int v)
let encode_int64 buf v = encode Buffer.add_int64_le buf v
let encode_float buf v = encode_int64 buf (Int64.bits_of_float v)
let encode_file_descr buf (v : Unix.file_descr) = encode_int16 buf (Obj.magic v)
let encode_xid = encode_int32
let encode_list encode_item buf ls = List.iter (encode_item buf) ls
let encode_string buf str = encode Buffer.add_string buf str

let encode_enum encode of_int to_int buf v =
  let v = to_int v in
  encode buf (of_int v)

let encode_mask encode of_int to_int buf v =
  let v = to_int v in
  encode buf (of_int v)

let encode_alt_enum encode of_int to_int buf v =
  match v with
  | E v -> encode_enum encode of_int to_int buf v
  | Custom v -> encode buf v

(* TODO should we use an Int64 here? *)
let encode_optional_mask encode buf fields =
  let rec loop mask = function
    | [] -> mask
    | (exists, pos) :: rest ->
        let mask = if exists then mask lor (1 lsl pos) else mask in
        loop mask rest
  in
  let mask = loop 0 fields in
  encode buf mask

let encode_pad buf len = encode Buffer.add_bytes buf (Bytes.create len)

let encode_align buf align =
  let len = Encode_buffer.current_offset buf mod align in
  encode_pad buf len

(* TODO Bytes can't grow but in Buffer you can't access arbitrary elements.
   we need to use a data structure that can do both to avoid the copying. *)
let encode_request_length buf =
  let len = Encode_buffer.current_offset buf in
  let bytes = Bytes.create len in
  Buffer.blit buf.buffer buf.offset bytes 0 len;
  Bytes.set_int16_le bytes 2 (len / 4);
  Buffer.truncate buf.buffer buf.offset;
  Buffer.add_bytes buf.buffer bytes
