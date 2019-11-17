(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Some utility to parse the output of different constraint solvers. *)

open Kobecore.Bench_desc_j

(** Default statistics when the problem is immediately detected unsatisfiable at the root node. *)
val root_node_unsat: (csv_field * string) list

(** [parse_output lines is_interesting sep solver_to_kobe extract_key_value].
    This function tries to extract statistics from each interesting `lines`.
    `solver_to_kobe` is a dictionnary from the string name of the statistics output by the solver to its meaning in Kobe.
    The `key_name` not in `solver_to_kobe` are ignored. *)
val parse_output:
  string list
  -> (string -> bool)
  -> char
  -> (string * csv_field) list
  -> (string list -> string * string)
  -> (csv_field * string) list
