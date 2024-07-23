let alt_enum ~int_of_t ~enum_of_int t =
  let i = int_of_t t in
  match enum_of_int i with
  | enum -> enum
  | exception Invalid_argument _ -> `Alt t
