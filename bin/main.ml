(** @author Ashley Liu awl77 *)

open Unix
open A6.Rbset

let time f x =
  let start = gettimeofday () in
  let _ = f x in
  let stop = gettimeofday () in
  stop -. start

(** Function to insert n distinct elements into a set *)
let insert_n_elements n =
  let rec insert_aux set current_element =
    if current_element < n then
      insert_aux (insert current_element set) (current_element + 1)
    else set
  in
  insert_aux empty 0

(** Return the median time *)
let measure_insertion n =
  let times = Array.init 10 (fun _ -> time insert_n_elements n) in
  Array.sort compare times;
  times.(Array.length times / 2)

(** Generating a range of n values for testing *)
let generate_n_values min_n max_n step =
  let rec aux current acc =
    if current > max_n then List.rev acc
    else aux (current * step) (current :: acc)
  in
  aux min_n []

(** Main function to run the measurements and output CSV *)
let () =
  let output_filename =
    if Array.length Sys.argv > 1 then Sys.argv.(1)
    else "data.csv" (** Default filename if none specified *)
  in
  let min_n = 1000 in
  let max_n = 1000000 in
  let step = 10 in
  let n_values = generate_n_values min_n max_n step in
  let out_channel = open_out output_filename in
  Printf.fprintf out_channel "N,Time,N log N\n";
  List.iter
    (fun n ->
      let median_time = measure_insertion n in
      let n_log_n = float_of_int n *. log (float_of_int n) in
      Printf.fprintf out_channel "%d,%g,%g\n" n median_time n_log_n)
    n_values;
  close_out out_channel;
  Printf.printf "Output written to %s\n" output_filename
