open Combinators

let ( let* ) = Result.bind

module Attr = struct
  open Unordered.Parsers

  type input = Xmlm.attribute list

  type error =
    [ Combinators.error
    | `Attribute_not_found of string
    | `Trailing_elements_detected
    | `Conversion_failed of string * string * string ]

  type ('a, 'err) t = ('a, input, ([> error ] as 'err)) parser

  let named name = function ("", n), v when n = name -> Some v | _ -> None

  let optional name ls = Ok (list_remove_opt (named name) ls)

  let required name ls =
    match list_remove_opt (named name) ls with
    | Some v, rest -> Ok (v, rest)
    | None, _ -> Error (`Attribute_not_found name)

  let str = required

  let str_o = optional

  let opt_conv v f err =
    match v with
    | None -> return None
    | Some v -> (
        match f v with Some i -> return (Some i) | None -> error (err v) )

  open Infix

  let int_o name =
    let& v = optional name in
    opt_conv v int_of_string_opt (fun a -> `Conversion_failed (name, "int", a))

  let int name =
    let& v = required name in
    int_of_string_opt v
    |> from_option ~none:(`Conversion_failed (name, "int", v))

  let bool_o name =
    let& v = optional name in
    opt_conv v bool_of_string_opt (fun a ->
        `Conversion_failed (name, "bool", a))

  let bool ?default name attrs =
    match (bool_o name attrs, default) with
    | Ok (Some b, rest), _ | Ok (None, rest), Some b -> Ok (b, rest)
    | Ok (None, _), None -> Error (`Attribute_not_found name)
    | (Error _ as err), _ -> err

  let run p attrs =
    let* res, attrs = p attrs in
    let* (), _ = eoi attrs in
    Ok res
end

open Infix
open Lazy_list.Parsers

type input = Xmlm.signal Lazy_list.t Lazy.t

type error =
  [ Attr.error
  | Lazy_list.Parsers.error
  | `Expected_data of Xmlm.signal
  | `Expected_dtd of Xmlm.signal
  | `Expected_el_start of string * Xmlm.signal
  | `Expected_empty_el_start of string * Xmlm.attribute list
  | `Expected_el_end of Xmlm.signal ]

type ('a, 'err) t = ('a, input, ([> error ] as 'err)) parser

let peek = peek

let data =
  apply @@ function `Data data -> Ok data | s -> Error (`Expected_data s)

let dtd : (Xmlm.dtd, [> error ]) t =
  apply @@ function `Dtd dtd -> Ok dtd | s -> Error (`Expected_dtd s)

let el_start_any =
  apply @@ function
  | `El_start _ -> Ok ()
  | s -> Error (`Expected_el_start ("", s))

let el_start_a name attr =
  apply @@ function
  | `El_start ((_, n), attrs) when n = name -> (Attr.run attr) attrs
  | s -> Error (`Expected_el_start (name, s))

let el_start name =
  apply @@ function
  | `El_start ((_, n), []) when n = name -> Ok ()
  | `El_start ((_, n), attrs) when n = name ->
      Error (`Expected_empty_el_start (name, attrs))
  | s -> Error (`Expected_el_start (name, s))

let el_end =
  apply @@ function `El_end -> Ok () | s -> Error (`Expected_el_end s)

let el_ab name attr body = el_start_a name attr *<> body *< el_end

let el_a name attr = el_start_a name attr *< el_end

let el_b name body = el_start name *> body *< el_end

let el name = el_start name *< el_end

let el_discard name =
  let inner =
    fix @@ fun el_discard ->
    many (discard data <|> el_start_any *> el_discard *> el_end) |> discard
  in
  el_start name *> inner *> el_end

let run p inp =
  let* res, inp = p inp in
  let* (), _ = eoi inp in
  Ok res

let inp str =
  let i =
    Xmlm.make_input ~strip:true (`String (0, str)) |> Lazy_list.of_xml_input
  in
  match Lazy.force i with
  | Lazy_list.Nil -> failwith "what"
  | Lazy_list.Cons (_, rest) -> rest

let make_input = Lazy_list.of_xml_input

let%test_module "el_discard" =
  ( module struct
    let%test "empty" = run (el_discard "el") (inp "<el></el>") = Ok ()

    let%test "data" = run (el_discard "el") (inp "<el>a</el>") = Ok ()

    let%test "single element" =
      run (el_discard "el") (inp "<el><a /></el>") = Ok ()

    let%test "multiple elements" =
      run (el_discard "el") (inp "<el><a /><a /></el>") = Ok ()

    let%test "nested elements" =
      run (el_discard "el")
        (inp "<el><a><b>c<c><d />d</c></b><ee>asd</ee></a></el>")
      = Ok ()
  end )
