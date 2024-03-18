type 'a t = {
  mutable buf : 'a Array.t;
  mutable start : int;
  mutable length : int;
  init : 'a;
}

let create initial_size init =
  { buf = Array.make initial_size init; start = 0; length = 0; init }

let is_empty t = t.length = 0
let length t = t.length
let capacity t = Array.length t.buf

let next_index t =
  let len = Array.length t.buf in
  (t.start + t.length) mod len

let%test _ =
  let b = create 4 0 in
  next_index b = 0

let%test _ =
  let b = create 4 0 in
  b.length <- 2;
  next_index b = 2

let%test _ =
  let b = create 4 0 in
  b.length <- 4;
  next_index b = 0

let%test _ =
  let b = create 4 0 in
  b.start <- 2;
  b.length <- 2;
  next_index b = 0

let current_index t =
  let len = Array.length t.buf in
  (t.start + t.length - 1) mod len

let%test _ =
  let b = create 4 0 in
  current_index b = -1

let%test _ =
  let b = create 4 0 in
  b.length <- 2;
  current_index b = 1

let%test _ =
  let b = create 4 0 in
  b.length <- 4;
  current_index b = 3

let%test _ =
  let b = create 4 0 in
  b.start <- 1;
  b.length <- 4;
  current_index b = 0

let%test _ =
  let b = create 4 0 in
  b.start <- 2;
  b.length <- 4;
  current_index b = 1

let realloc t new_size =
  if new_size >= t.length then
    invalid_arg "new_size is not big enough to handle t";
  let len = Array.length t.buf in
  let new_buf = Array.make new_size t.init in
  (if t.start + t.length <= len then Array.blit t.buf t.start new_buf 0 t.length
   else
     let len_until_end = len - t.start in
     Array.blit t.buf t.start new_buf 0 len_until_end;
     Array.blit t.buf 0 new_buf len_until_end (len - len_until_end));
  t.buf <- new_buf;
  t.start <- 0

let%test _ =
  let b = create 4 0 in
  b.start <- 1;
  b.length <- 3;
  b.buf <- [| 0; 1; 2; 3 |];
  realloc b 5;
  b.buf = [| 1; 2; 3; 0; 0 |]

let%test _ =
  let b = create 4 0 in
  b.start <- 3;
  b.length <- 2;
  b.buf <- [| 2; 0; 0; 1 |];
  realloc b 4;
  b.buf = [| 1; 2; 0; 0 |]

let%test _ =
  let b = create 4 0 in
  b.start <- 2;
  b.length <- 4;
  b.buf <- [| 3; 4; 1; 2 |];
  realloc b 5;
  b.buf = [| 1; 2; 3; 4; 0 |]

let push_back t item =
  let len = Array.length t.buf in
  if len = t.length then realloc t (len * 2);
  let i = next_index t in
  t.buf.(i) <- item;
  t.length <- t.length + 1

let%test "push_to_end adds items to the end of the ring buffer" =
  let b = create 4 0 in
  for n = 1 to 4 do
    push_back b n
  done;
  b.buf = [| 1; 2; 3; 4 |]

let%test "push_to_end reallocs when the buffer is not big enough" =
  let b = create 4 0 in
  for n = 1 to 8 do
    push_back b n
  done;
  b.buf = [| 1; 2; 3; 4; 5; 6; 7; 8 |]

let%test "push_to_end respects the start index" =
  let b = create 4 0 in
  b.start <- 2;
  for n = 1 to 4 do
    push_back b n
  done;
  b.buf = [| 3; 4; 1; 2 |]

let%test "push_to_end respects the start index and reallocates" =
  let b = create 4 0 in
  b.start <- 2;
  for n = 1 to 8 do
    push_back b n
  done;
  b.buf = [| 1; 2; 3; 4; 5; 6; 7; 8 |]

let pop_front t =
  if t.length = 0 then invalid_arg "attempted to pop an empty ring buffer";
  let item = t.buf.(t.start) in
  let len = Array.length t.buf in
  t.start <- (t.start + 1) mod len;
  t.length <- t.length - 1;
  item

let%test "pop_from_start returns the first item of the ring buffer" =
  let b = create 4 0 in
  push_back b 1;
  push_back b 2;
  pop_front b = 1

let%test "pop_from_start shifts the ring buffer" =
  let b = create 4 0 in
  push_back b 1;
  push_back b 2;
  pop_front b |> ignore;
  realloc b 4;
  b.buf = [| 2; 0; 0; 0 |]

let%test "pop_from_start respects array bounds" =
  let b = create 4 0 in
  b.start <- 3;
  for n = 1 to 4 do
    push_back b n
  done;
  pop_front b = 1 && pop_front b = 2

let peek_front t =
  if t.length = 0 then invalid_arg "attempted to peek an empty ring buffer";
  t.buf.(t.start)

let%test _ =
  let b = create 4 0 in
  push_back b (-3);
  push_back b (-2);
  push_back b (-1);
  for i = 0 to 100 do
    push_back b i;
    pop_front b |> ignore
  done;
  Array.length b.buf = 4
  &&
  (realloc b 4;
   b.buf = [| 98; 99; 100; 0 |])
