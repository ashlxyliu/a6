(** @author Ashley Liu awl77 *)

type 'a t
(** Type of a set with elements of type 'a. *)

val empty : 'a t
(** The empty set. *)

val is_empty : 'a t -> bool
(** [is_empty s] checks if the set is empty.
    Returns true if [s] is empty, otherwise false. *)

val mem : 'a -> 'a t -> bool
(** [mem x s] checks for membership of an element in the set.
    Returns true if [x] is a member of [s], otherwise false. *)

val insert : 'a -> 'a t -> 'a t
(** [insert x s] inserts an element into the set.
    Returns a new set with [x] added. If [x] was already a member of [s], 
    returns [s] unchanged. *)
