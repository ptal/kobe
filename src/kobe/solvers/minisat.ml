open Core
open Bench_desc_j

let has_time_option = false

(* List of `(minisat-kw,kobe-kw)` keywords where `minisat-kw` is the beginning the output line where we can find the value `kobe-kw`. *)
let minisat_to_kobe_stats = [
  ("restarts", `Restarts);
  ("conflicts", `Fails);
  ("propagations", `Nodes);
  ("Memory used", `Memory `MB);
  ("CPU time", `Time `Sec)];;

let is_interesting line =
  minisat_to_kobe_stats |> List.map fst |> List.exists (start_with line)

let parse_last_line lines =
  let last_line = List.nth ((List.length lines) - 1) lines in
  let (satisfiability, solutions) =
    if last_line = "INDETERMINATE" then "unknown", "0"
    else if last_line = "SATISFIABLE" then "sat", "1"
    else if last_line = "UNSATISFIABLE" then "unsat", "0"
    else eprintf_and_exit
      ("Could not parse the last line of MiniSAT output, which is `" ^ last_line ^ "`")
  in
  [(`Satisfiability, satisfiability); (`Solutions, solutions)]

let parse_output lines =
  let lines = List.filter (fun s -> not (String.is_empty s)) lines in
  let entries = Generic.parse_output lines is_interesting ':' ' ' minisat_to_kobe_stats in
  entries@(parse_last_line lines)

let make_command exec _ strategy input_file =
  exec ^ " -verb=1 " ^ strategy ^ " " ^ input_file
