(** This file provides support function for solver-output compatible with MiniZinc.
    These are entries of the form `%%%mzn-stat: solveTime=0.008161` *)

open Core

let has_time_option = true

let minizinc_to_kobe_stats = [
  ("restarts", `Restarts);
  ("failures", `Fails);
  ("nodes", `Nodes);
  ("peakMem", `Memory `MB);
  ("solveTime", `Time `Sec);
  ("objective", `Optimum);
  ("peakDepth", `DepthMax)];;

let clean_entry (name, value) =
  match name with
  | `Solutions -> [(name, "0")]
  | `Optimum -> []
  | x -> [x]

let contains_entry name l = l |> List.map fst |> List.exists (fun x-> x=name)

(* Clean some default values added by MiniZinc solvers.
   For example, Chuffed outputs `objective=-1` in case of unsatisfiability. *)
let clean_up_entries mzn_entries lines =
  let unsat_msg = "=====UNSATISFIABLE=====" in
  let unsat_msg_len = String.length unsat_msg in
  let is_unsat_msg l =
    if String.length l >= unsat_msg_len then
      let unsat_part = String.sub (String.trim l) 0 unsat_msg_len in
      unsat_part = unsat_msg
    else false in
  if List.exists is_unsat_msg lines then
  begin
    let mzn_entries = mzn_entries |> List.map clean_entry |> List.flatten in
    if contains_entry mzn_entries `Solutions then
      mzn_entries
    else
      mzn_entries@[(`Solutions, 0)]
  end
  else mzn_entries

let parse_output lines =
  let is_mzn_entry l = start_with l "%" in
  let is_not_mzn_entry l = not (is_mzn_entry l) in
  let mzn_entries = Generic.parse_output lines is_mzn_entry ' ' '=' minizinc_to_kobe_stats in
  let mzn_entries =
    if contains_entry mzn_entries `Optimum then
      mzn_entries
    else
      try
        let lines = List.filter is_not_mzn_entry lines in
        let obj = Scanf.sscanf (List.hd lines) "%s = %d;" (fun _ i -> string_of_int i) in
        mzn_entries@[(`Optimum, obj)]
      with _ -> mzn_entries in
  clean_up_entries mzn_entries lines

let make_command exec timeout strategy input_file =
  exec ^ " -t " ^ timeout ^ " " ^ strategy ^ " " ^ input_file
