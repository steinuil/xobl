type void = Bytes.t
(** Void are just bytes *)

type nonrec char = char
type byte = char
type nonrec bool = bool
type i8 = int
type i16 = int
type i32 = int32
type u8 = int
type u16 = int
type u32 = int32
type u64 = Int64.t
type nonrec float = float
type double = float
type file_descr = File_descr of int
type xid = Xid of int32

(** A string where characters are two bytes *)
type utf16_string = Utf16_string of string

(* module Mask (T : sig
     type prim
   end) : sig
     type t
   end = struct
     type t = T.prim
   end *)
