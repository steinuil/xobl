open Sexplib.Conv

type void = bytes [@@deriving sexp]
(** Void are just bytes *)

type nonrec char = char [@@deriving sexp]
type byte = char [@@deriving sexp]
type nonrec bool = bool [@@deriving sexp]
type i8 = int [@@deriving sexp]
type i16 = int [@@deriving sexp]
type i32 = int [@@deriving sexp]
type u8 = int [@@deriving sexp]
type u16 = int [@@deriving sexp]
type u32 = int [@@deriving sexp]
type u64 = int64 [@@deriving sexp]
type nonrec float = float [@@deriving sexp]
type double = float [@@deriving sexp]
type file_descr = File_descr of int [@@deriving sexp]
type xid = Xid of int32 [@@deriving sexp]

(** A string where characters are two bytes *)
type utf16_string = Utf16_string of string [@@deriving sexp]

type 'a alt = [ `Alt of 'a ] [@@deriving sexp]

module Mask : sig
  type t = private int64

  val ( & ) : t -> t -> bool
  val ( || ) : t -> t -> t
  val of_int64 : int64 -> t
  val to_int64 : t -> int64
end = struct
  type t = int64

  let ( & ) a b = Int64.logand a b <> 0L
  let ( || ) = Int64.logor
  let of_int64 = Fun.id
  let to_int64 = Fun.id
end
