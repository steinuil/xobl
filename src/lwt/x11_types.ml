open Sexplib.Conv

type xid = int [@@deriving sexp]
type file_descr = (Unix.file_descr[@sexp.opaque]) [@@deriving sexp]
type ('flags, 'vals) mask = F of 'flags | V of 'vals [@@deriving sexp]
type 't alt_enum = E of 't | Custom of int [@@deriving sexp]
