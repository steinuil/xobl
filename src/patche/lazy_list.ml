type 'a t = Cons of 'a * 'a t Lazy.t | Nil

let of_xml_input inp =
  let rec next () =
    match Xmlm.input inp with
    | v -> Cons (v, Lazy.from_fun next)
    | exception Xmlm.Error _ -> Nil
  in
  Lazy.from_fun next

module Parsers = struct
  type error = [ Combinators.error | `End_of_input ]

  let any inp =
    match Lazy.force inp with
    | Cons (v, rest) -> Ok (v, rest)
    | Nil -> Error `End_of_input

  let peek inp =
    match Lazy.force inp with
    | Cons (v, _) -> Ok (v, inp)
    | Nil -> Error `End_of_input

  let eoi inp =
    match Lazy.force inp with
    | Nil -> Ok ((), inp)
    | Cons _ -> Error `Trailing_elements_detected

  let apply f inp =
    match any inp with
    | Ok (v, rest) -> f v |> Result.map (fun v -> (v, rest))
    | Error _ as err -> err
end
