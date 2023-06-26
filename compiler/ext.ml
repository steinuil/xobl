module ListExt = struct
  (** Finds the first element of [list] that satisfies [pred]
      and returns it along with the list without that element.
      @raise Invalid_argument when [list] does not contain any
      element that satisfies [pred]. *)
  let find_remove ~pred list =
    let rec remove' acc = function
      | [] -> invalid_arg "list"
      | item :: rest when pred item -> (item, List.rev acc @ rest)
      | item :: rest -> remove' (item :: acc) rest
    in
    remove' [] list

  (** Finds the first element of [list] that satisfies [pred]
      and returns the list with [item] inserted between
      the elements of the list up to the element satisfying [pred]
      and the rest of the list, including the item satisfying [pred].
      @raise Invalid_argument when [list] does not contain any element
      that satisfies [pred]. *)
  let insert_before ~pred ~item list =
    let rec splice' acc = function
      | [] -> invalid_arg "list"
      | curr :: _ as rest when pred curr -> List.rev acc @ [ item ] @ rest
      | curr :: rest -> splice' (curr :: acc) rest
    in
    splice' [] list

  let find_map_exn ~f list = List.find_map f list |> Option.get
end

module SexpExt = struct
  let print_hum ?indent ~conv t =
    conv t |> Sexplib.Sexp.to_string_hum ?indent |> print_endline
end
