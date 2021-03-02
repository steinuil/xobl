type error =
  [ `Error of string | `Custom of string | `Test_failed | `Choice_failed ]

type ('t, 'inp, 'err) parser =
  'inp -> ('t * 'inp, ([> error ] as 'err)) Result.t

let return v inp = Ok (v, inp)

let error e _ = Error e

let from_result = function Ok v -> return v | Error e -> error e

let from_option ~none o = Option.to_result ~none o |> from_result

(* Monad *)

let bind parser f inp =
  match parser inp with Ok (result, rest) -> f result rest | Error _ as e -> e

let ( let& ) = bind

let map f p =
  let& res = p in
  return (f res)

let map_result f p inp =
  match p inp with
  | Ok (v, rest) -> f v |> Result.map (fun v -> (v, rest))
  | Error _ as err -> err

let map_option ~none f p inp =
  match p inp with
  | Error _ as err -> err
  | Ok (v, rest) -> (
      match f v with Some v -> Ok (v, rest) | None -> Error (none v) )

(* Various *)

let satisfies test p =
  let& res = p in
  if test res then return res else error `Test_failed

let discard_with v p =
  let& _ = p in
  return v

let discard p = discard_with () p

let discard_right p1 p2 =
  let& res = p1 in
  let& _ = p2 in
  return res

let discard_left p1 p2 =
  let& _ = p1 in
  p2

let opt p inp =
  match p inp with
  | Ok (v, rest) -> Ok (Some v, rest)
  | Error _ -> Ok (None, inp)

(* Sequencing *)

let many p inp =
  let rec loop acc inp =
    match p inp with
    | Ok (v, rest) ->
        if rest == inp then
          invalid_arg
            "infinite loop detected in many: parser did not consume anything"
        else loop (v :: acc) rest
    | Error _ -> Ok (List.rev acc, inp)
  in
  loop [] inp

let map2 f p1 p2 =
  let& res1 = p1 in
  let& res2 = p2 in
  return (f res1 res2)

let many1 p = map2 List.cons p (many p)

let map3 f p1 p2 p3 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  return (f res1 res2 res3)

let map4 f p1 p2 p3 p4 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  return (f res1 res2 res3 res4)

let map5 f p1 p2 p3 p4 p5 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  let& res5 = p5 in
  return (f res1 res2 res3 res4 res5)

let map6 f p1 p2 p3 p4 p5 p6 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  let& res5 = p5 in
  let& res6 = p6 in
  return (f res1 res2 res3 res4 res5 res6)

let tuple2 p1 p2 =
  let& res1 = p1 in
  let& res2 = p2 in
  return (res1, res2)

let tuple3 p1 p2 p3 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  return (res1, res2, res3)

let tuple4 p1 p2 p3 p4 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  return (res1, res2, res3, res4)

let tuple6 p1 p2 p3 p4 p5 p6 =
  let& res1 = p1 in
  let& res2 = p2 in
  let& res3 = p3 in
  let& res4 = p4 in
  let& res5 = p5 in
  let& res6 = p6 in
  return (res1, res2, res3, res4, res5, res6)

(* Choice *)

let or_ p1 p2 inp = match p1 inp with Ok _ as ok -> ok | Error _ -> p2 inp

let choice ps inp =
  let rec loop = function
    | [] -> Error `Choice_failed
    | p :: ps -> ( match p inp with Ok _ as ok -> ok | Error _ -> loop ps )
  in
  loop ps

let rec fix p inp = p (fix p) inp

module Infix = struct
  let ( let& ) = bind

  (* In ascending order of precedence *)

  let ( <|> ) = or_

  let ( ->> ) p f = map f p

  let ( ->= ) p f = map_result f p

  let ( *> ) = discard_left

  let ( *< ) = discard_right

  let ( *<> ) = tuple2
end
