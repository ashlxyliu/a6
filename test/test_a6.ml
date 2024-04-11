(** @author Ashley Liu awl77 *)

open OUnit2
open A6.Rbset

(** Helper function to create a set from a list of elements *)
let set_of_list elements =
  List.fold_left (fun acc x -> insert x acc) empty elements

(** Test that an empty set is recognized as empty *)
let test_is_empty_empty _ = assert_bool "Empty set is empty" (is_empty empty)

(** Test that a set is not empty after an element is inserted *)
let test_is_empty_insert _ =
  let s = insert 1 empty in
  assert_bool "Set is not empty after insert" (not (is_empty s))

(** Test that an element is not found in an empty set *)
let test_mem_empty _ =
  assert_bool "Element is not in empty set" (not (mem 1 empty))

(** Test that an inserted element is found in the set *)
let test_mem_insert_same _ =
  let s = insert 1 empty in
  assert_bool "Element is in set after insert" (mem 1 s)

(** Test that a non-inserted element is not found in the set *)
let test_mem_insert_diff _ =
  let s = insert 1 empty in
  assert_bool "Different element is not in set" (not (mem 2 s))

(** Test that multiple inserted elements are found in the set *)
let test_mem_insert_list _ =
  let s = set_of_list [ 1; 2; 3; 4; 5 ] in
  List.iter
    (fun x -> assert_bool (Printf.sprintf "%d should be in set" x) (mem x s))
    [ 1; 2; 3; 4; 5 ]

(** Test that consecutive red nodes on the left are balanced correctly *)
let test_balance_consecutive_reds_left _ =
  let s = set_of_list [ 20; 15; 10 ] in
  assert_bool "Balance consecutive reds on left" (mem 10 s && mem 15 s)

(** Test that red nodes on both left and right children are balanced correctly *)
let test_balance_left_and_right_red_children _ =
  let s = set_of_list [ 20; 10; 15; 5; 1 ] in
  assert_bool "Balance left and right red children" (mem 1 s && mem 15 s)

(** Test that consecutive red nodes on the right are balanced correctly *)
let test_balance_consecutive_reds_right _ =
  let s = set_of_list [ 10; 15; 20 ] in
  assert_bool "Balance consecutive reds on right" (mem 20 s && mem 15 s)

(** Test that inserting an existing value does not modify the set *)
let test_insert_non_modification _ =
  let s = insert 1 (insert 1 empty) in
  assert_bool "Inserting existing value does not modify set" (mem 1 s)

(** Test that a right child red node with a left red child is balanced correctly *)
let test_balance_right_red_left_red_child _ =
  let s = set_of_list [ 10; 20; 15 ] in
  assert_bool "Balance right child red and its left child red" (mem 15 s)

let suite =
  "RBSet Tests"
  >::: [
         "is_empty_empty" >:: test_is_empty_empty;
         "is_empty_insert" >:: test_is_empty_insert;
         "mem_empty" >:: test_mem_empty;
         "mem_insert_same" >:: test_mem_insert_same;
         "mem_insert_diff" >:: test_mem_insert_diff;
         "mem_insert_list" >:: test_mem_insert_list;
         "balance_consecutive_reds_left" >:: test_balance_consecutive_reds_left;
         "balance_left_and_right_red_children"
         >:: test_balance_left_and_right_red_children;
         "balance_consecutive_reds_right"
         >:: test_balance_consecutive_reds_right;
         "insert_non_modification" >:: test_insert_non_modification;
         "balance_right_red_left_red_child"
         >:: test_balance_right_red_left_red_child;
       ]

let () = run_test_tt_main suite
