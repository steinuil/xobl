module Parsers = struct
  let list_remove_opt f =
    let rec loop prev = function
      | [] -> (None, List.rev prev)
      | el :: rest -> (
          match f el with
          | Some el -> (Some el, List.rev_append prev rest)
          | None -> loop (el :: prev) rest)
    in
    loop []

  let eoi = function
    | [] -> Ok ((), [])
    | _ -> Error `Trailing_elements_detected
end
