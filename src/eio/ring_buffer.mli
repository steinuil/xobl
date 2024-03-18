type 'a t 

val create : int -> 'a -> 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val capacity : 'a t -> int

val peek_front : 'a t -> 'a
(** Returns the item at the front of the ring buffer without popping it.
    @raise Invalid_argument when ring buffer is empty *)

val pop_front : 'a t -> 'a
(** Returns the item at the front of the ring buffer.
    Does not clear the underlying array.
    @raise Invalid_argument when ring buffer is empty *)

val push_back : 'a t -> 'a -> unit
(** Pushes an item to the back of the buffer.
    Reallocates to double the capacity when the ring buffer is full. *)

val realloc : 'a t -> int -> unit
(** Reallocates the ring buffer to a new capacity,
    reordering the internal array and removing unused items.
    @raise Invalid_argument when the new size is smaller than the current length
           of the ring buffer. *)
