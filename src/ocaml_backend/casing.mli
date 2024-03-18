val snake : string -> string
val caml : string -> string

module OCaml : sig
  val snake : ?prefix:string -> ?suffix:string -> string -> string
  val caml : string -> string
end
