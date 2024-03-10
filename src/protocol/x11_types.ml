open Sexplib.Conv

module Xid = struct
  type t = Xid of int [@@deriving sexp]

  let of_int n = Xid n
  let to_int (Xid n) = n
end

type file_descr = int [@@deriving sexp]
type ('flags, 'vals) mask = F of 'flags | V of 'vals [@@deriving sexp]
type 't alt_enum = E of 't | Custom of int [@@deriving sexp]
