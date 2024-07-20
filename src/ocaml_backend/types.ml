type void = Bytes.t
(** Void are just bytes *)

type nonrec char = char
type byte = char
type nonrec bool = bool
type i8 = int
type i16 = int

(* TODO: Int32 and Card32 should be mapped to int32 to ensure compatibility with
   32-bit platforms maybe. *)
type i32 = int
type u8 = int
type u16 = int
type u32 = int
type u64 = Int64.t
type nonrec float = float
type double = float
type xid = Xid of int

(** A string where characters are two bytes *)
type utf16_string = Utf16_string of string

module Mask (T : sig
  type prim
end) : sig
  type t
end = struct
  type t = T.prim
end
