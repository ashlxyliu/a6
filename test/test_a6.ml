open OUnit2
open A6.Rbset

(* Helper function to create a set from a list of elements *)
let set_of_list elements =
  List.fold_left (fun acc x -> insert x acc) empty elements

let test_is_empty_empty _ = assert_bool "Empty set is empty" (is_empty empty)

let test_is_empty_insert _ =
  let s = insert 1 empty in
  assert_bool "Set is not empty after insert" (not (is_empty s))

let test_mem_empty _ =
  assert_bool "Element is not in empty set" (not (mem 1 empty))

let test_mem_insert_same _ =
  let s = insert 1 empty in
  assert_bool "Element is in set after insert" (mem 1 s)

let test_mem_insert_diff _ =
  let s = insert 1 empty in
  assert_bool "Different element is not in set" (not (mem 2 s))

let test_mem_insert_list _ =
  let s = set_of_list [ 1; 2; 3; 4; 5 ] in
  List.iter
    (fun x -> assert_bool (Printf.sprintf "%d should be in set" x) (mem x s))
    [ 1; 2; 3; 4; 5 ]

let suite =
  "RBSet Tests"
  >::: [
         "is_empty_empty" >:: test_is_empty_empty;
         "is_empty_insert" >:: test_is_empty_insert;
         "mem_empty" >:: test_mem_empty;
         "mem_insert_same" >:: test_mem_insert_same;
         "mem_insert_diff" >:: test_mem_insert_diff;
         "mem_insert_list" >:: test_mem_insert_list;
       ]

let () = run_test_tt_main suite
