let alt_enum ~int_of_t ~enum_of_int t =
  let i = int_of_t t in
  match enum_of_int i with
  | enum -> enum
  | exception Invalid_argument _ -> `Alt t

module To_int = struct
  let[@inline] bool = function true -> 1 | false -> 0
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
  let[@inline] xid (Types.Xid i) = u32 i
end

module To_i32 = struct
  let[@inline] bool = function true -> Optint.one | false -> Optint.zero
  let i8 = Optint.of_int
  let u8 = Optint.of_int
  let i16 = Optint.of_int
  let u16 = Optint.of_int
  let i32 = Fun.id
  let u32 = Fun.id
  let u64 = Optint.of_int64
  let[@inline] char i = Char.code i |> Optint.of_int
  let void = char
  let byte = char
  let file_descr _ = failwith "should not happen"
  let[@inline] xid (Types.Xid i) = i
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

let sum = List.fold_left ( + ) 0

(* These two functions only take optints because they're only ever used for masks. *)
let sum_map ~f ls =
  List.fold_left (fun acc item -> Optint.add acc (f item)) Optint.zero ls
  |> Optint.to_int

let pop_count n =
  let open Optint.Infix in
  let rec iter pos acc =
    if pos > Optint.of_int 31 then acc
    else if n land (Optint.one lsl Optint.to_int pos) <> Optint.zero then
      iter (pos + Optint.one) acc + Optint.one
    else iter (pos + Optint.one) acc
  in
  iter Optint.zero Optint.zero
