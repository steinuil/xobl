open Sexplib.Conv

type void = bytes [@@deriving sexp]
(** Void are just bytes *)

type nonrec char = char [@@deriving sexp]
type byte = char [@@deriving sexp]
type nonrec bool = bool [@@deriving sexp]
type i8 = int [@@deriving sexp]
type i16 = int [@@deriving sexp]
type i32 = Optint.t

let sexp_of_i32 n = Optint.to_int32 n |> sexp_of_int32
let i32_of_sexp n = int32_of_sexp n |> Optint.of_int32

type u8 = int [@@deriving sexp]
type u16 = int [@@deriving sexp]
type u32 = Optint.t

let sexp_of_u32 n = Optint.to_unsigned_int32 n |> sexp_of_int32
let u32_of_sexp n = int32_of_sexp n |> Optint.of_unsigned_int32

type u64 = int64 [@@deriving sexp]
type nonrec float = float [@@deriving sexp]
type double = float [@@deriving sexp]
type file_descr = File_descr of int [@@deriving sexp]
type xid = Xid of u32 [@@deriving sexp]

(** A string where characters are two bytes *)
type utf16_string = Utf16_string of string [@@deriving sexp]

type 'a alt = [ `Alt of 'a ] [@@deriving sexp]

module type Mask = sig
  type t

  val ( & ) : t -> t -> bool
  val ( || ) : t -> t -> t
  val of_int32 : Optint.t -> t
  val to_int32 : t -> Optint.t
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val t_of_sexp : Sexplib0.Sexp.t -> t
end

module type Event = sig
  type t

  val name : string
  val number : int
end

module type Error = sig
  type t

  val name : string
  val number : int
end

module type Extension = sig
  module Meta : sig
    type t

    val version : int * int
    val query_name : string
  end

  module Error : sig
    type t
  end

  module Event : sig
    type t
  end
end
