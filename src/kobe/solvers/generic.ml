open Kobecore.Bench_desc_j

(** A set of statistics in case the problem is unsatisfiable at the root node. *)

let root_node_unsat =
  [(`Time `Sec, "0.01"); (`Restarts, "0");
   (`Solutions, "0"); (`Fails, "1"); (`Nodes, "1");
   (`Satisfiability, "unsat"); (`Optimum, "unsat")]

(** This function tries to extract for each interesting lines a kobe statistics with its value.
    We expect interesting lines to be of the form `key_name <sep1> value <sep2> ...`.
    `key_name` not in `solver_to_kobe` are ignored.  *)
let parse_output lines is_interesting sep1 sep2 solver_to_kobe =
     lines
  |> List.map String.trim lines
  |> List.filter is_interesting
  |> List.map (String.split_on_char sep1)
  |> List.filter (fun l -> (List.length l) = 2)
  |> List.map (fun l ->
      try
        let value = String.trim (List.nth l 1) in
        let value = String.split_on_char sep2 value in
        let value = String.trim (List.hd value) in
        let key = List.assoc (String.trim (List.hd l)) solver_to_kobe in
        [(key, value)]
      with Not_found -> []
     )
  |> List.flatten
