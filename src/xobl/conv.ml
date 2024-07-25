let alt_enum ~int_of_t ~enum_of_int t =
  let i = int_of_t t in
  match enum_of_int i with
  | enum -> enum
  | exception Invalid_argument _ -> `Alt t

module To_int = struct
  let bool = function true -> 1 | false -> 0
  let i8 = Fun.id
  let u8 = Fun.id
  let i16 = Fun.id
  let u16 = Fun.id
  let i32 = Optint.to_int
  let u32 = Optint.to_int

  let u64 i =
    match Int64.unsigned_to_int i with
    | None -> Printf.ksprintf failwith "Failed to convert u64 to int: %Ld" i
    | Some i -> i

  let char = Char.code
  let void = Char.code
  let byte = Char.code
  let file_descr _ = failwith "should not happen"
  let xid (Types.Xid i) = u32 i
end
