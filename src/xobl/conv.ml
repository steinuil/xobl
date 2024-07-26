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

module To_i32 = struct
  let bool = function true -> Optint.one | false -> Optint.zero
  let i8 = Optint.of_int
  let u8 = Optint.of_int
  let i16 = Optint.of_int
  let u16 = Optint.of_int
  let i32 = Fun.id
  let u32 = Fun.id
  let u64 = Optint.of_int64
  let char i = Char.code i |> Optint.of_int
  let void = char
  let byte = char
  let file_descr _ = failwith "should not happen"
  let xid (Types.Xid i) = i
end

(* module To_int64 = struct
     let bool = function true -> 1L | false -> 0L
     let i8 = Int64.of_int
     let u8 = Int64.of_int
     let i16 = Int64.of_int
     let u16 = Int64.of_int
     let i32 = Optint.to_int64
     let u32 = Optint.to_int64
     let u64 = Fun.id
     let char i = Char.code i |> Int64.of_int
     let void = char
     let byte = char
     let file_descr _ = failwith "should not happen"
     let xid (Types.Xid i) = u32 i
   end *)
