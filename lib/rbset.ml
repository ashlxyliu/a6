(** @author Ashley Liu awl77 *)

type color = Red | Black
type 'a rb_tree = Leaf | Node of color * 'a rb_tree * 'a * 'a rb_tree
type 'a t = 'a rb_tree

let empty = Leaf
let is_empty = function Leaf -> true | _ -> false

let rec mem x = function
  | Leaf -> false
  | Node (_, a, y, b) ->
      if x < y then mem x a else if x > y then mem x b else true

let balance = function
  | Black, Node (Red, Node (Red, a, x, b), y, c), z, d
  | Black, Node (Red, a, x, Node (Red, b, y, c)), z, d
  | Black, a, x, Node (Red, Node (Red, b, y, c), z, d)
  | Black, a, x, Node (Red, b, y, Node (Red, c, z, d)) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | color, a, x, b -> Node (color, a, x, b)

let rec insert x = function
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (color, a, y, b) as s ->
      if x < y then balance (color, insert x a, y, b)
      else if x > y then balance (color, a, y, insert x b)
      else s

let rec insert x = function
  | Leaf -> Node (Red, Leaf, x, Leaf) (* Inserting into an empty tree *)
  | Node (color, a, y, b) as s ->
      if x < y then balance (color, insert x a, y, b)
      else if x > y then balance (color, a, y, insert x b)
      else s

let insert x s =
  match insert x s with Node (_, a, y, b) -> Node (Black, a, y, b)
(* Ensure the root is always black *)
(* Removed the 'Leaf -> failwith "Invariant violation"' case *)

(* let insert x s =
   match insert x s with
   | Node (_, a, y, b) -> Node (Black, a, y, b)
   | Leaf -> failwith "insert: Invariant violation" *)
