module Attr : sig
  type input = Xmlm.attribute list

  type error =
    [ Combinators.error
    | `Attribute_not_found of string
    | `Trailing_elements_detected
    | `Conversion_failed of string * string * string ]

  type ('a, 'err) t = ('a, input, ([> error ] as 'err)) Combinators.parser

  val str : string -> (string, 'err) t

  val str_o : string -> (string option, 'err) t

  val int : string -> (int, 'err) t

  val int_o : string -> (int option, 'err) t

  val bool : ?default:bool -> string -> (bool, 'err) t

  val bool_o : string -> (bool option, 'err) t

  val run : ('a, 'err) t -> input -> ('a, 'err) result
end

type input = Xmlm.signal Lazy_list.t Lazy.t

type error =
  [ Attr.error
  | Lazy_list.Parsers.error
  | `Expected_data of Xmlm.signal
  | `Expected_dtd of Xmlm.signal
  | `Expected_el_start of string * Xmlm.signal
  | `Expected_empty_el_start of string * Xmlm.attribute list
  | `Expected_el_end of Xmlm.signal ]

type ('a, 'err) t = ('a, input, ([> error ] as 'err)) Combinators.parser

val make_input : Xmlm.input -> input

val peek :
  input -> (Xmlm.signal * input, ([> Lazy_list.Parsers.error ] as 'err)) result

val data : (string, 'err) t

val dtd : (Xmlm.dtd, 'err) t

val el_ab : string -> ('a, 'err) Attr.t -> ('b, 'err) t -> ('a * 'b, 'err) t

val el_a : string -> ('a, 'err) Attr.t -> ('a, 'err) t

val el_b : string -> ('a, 'err) t -> ('a, 'err) t

val el : string -> (unit, 'err) t

val el_discard : string -> (unit, 'err) t

val run : ('a, 'err) t -> input -> ('a, 'err) result
