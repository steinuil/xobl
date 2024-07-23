type hostname =
  | Unix_domain_socket of string
  | Internet_domain of ([ `Ipv4 | `Ipv6 ] * string * int)

val sexp_of_hostname : hostname -> Sexplib0.Sexp.t

type t = { hostname : hostname; display : int; screen : int } [@@deriving sexp]

val sexp_of_t : t -> Sexplib0.Sexp.t

val default : t
(** Default X11 display name equivalent to ":0" *)

val parse : string -> t option
(** Parse an X11 display name *)

val from_env : unit -> t option
(** Shortcut to get the DISPLAY env variable and parse it into a Display_name.t *)
