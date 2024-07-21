val snake : string -> string
val caml : string -> string

module OCaml : sig
  val snake : ?sanitize:string -> ?prefix:string -> ?suffix:string -> string -> string
  val caml : ?sanitize:string -> ?prefix:string -> ?suffix:string -> string -> string
end
