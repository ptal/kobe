(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements `Solver_sig` for the Chuffed constraint solver. *)

let has_time_option = Minizinc_generic.has_time_option

let top_level_error lines =
  List.exists (fun l -> (String.trim l) = "% Top level failure!") lines

let parse_output lines =
  (* In this case it is unsatisfiable at top-level, which means that Chuffed does not output any stat, not even time. *)
  if top_level_error lines then
    Generic.root_node_unsat
  else
    Minizinc_generic.parse_output lines

let make_command exec = Minizinc_generic.make_command (exec ^ " -v")
