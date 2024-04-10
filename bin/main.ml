(** @author Ashley Liu awl77 *)

open Unix
open Random

let time f x =
  let start = gettimeofday () in
  let _ = f x in
  let stop = gettimeofday () in
  stop -. start

(* Function to insert n distinct elements into a set *)
let insert_n_elements n =
  let rec insert_aux set current_element =
    if current_element < n then
      insert_aux (RBSet.insert current_element set) (current_element + 1)
    else set
  in
  insert_aux RBSet.empty 0

let measure_insertion n =
  let times = Array.init 10 (fun _ -> time insert_n_elements n) in
  Array.sort compare times;
  times.(Array.length times / 2)
(* Return the median time *)

(* Generating a range of n values for testing *)
let generate_n_values min_n max_n step =
  let rec aux current acc =
    if current > max_n then List.rev acc
    else aux (current * step) (current :: acc)
  in
  aux min_n []

(* Main function to run the measurements and output CSV *)
let () =
  let min_n = 1000 in
  (* Adjust based on your system's performance *)
  let max_n = 1000000 in
  let step = 10 in
  let n_values = generate_n_values min_n max_n step in
  Printf.printf "N,Time\n";
  List.iter
    (fun n ->
      let median_time = measure_insertion n in
      Printf.printf "%d,%g\n" n median_time)
    n_values
