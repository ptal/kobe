let has_time_option = Minizinc_generic.has_time_option

let top_level_error lines =
  List.exists (fun l -> (String.trim l) = "% Top level failure!") lines

let parse_output lines =
  (* In this case it is unsatisfiable at top-level, which means that Chuffed does not output any stat, not even time. *)
  if top_level_error lines then
    Generic.root_node_unsat
  else
    Minizinc_generic.parse_output lines

let make_command = Minizinc_generic.make_command
