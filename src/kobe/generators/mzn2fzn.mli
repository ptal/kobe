(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** `Mzn2fzn` is not a solver per se, but a converter between MiniZinc files (.mzn) and FlatZinc files (.fzn).
     The executable `mz2fzn` must be specified in the `solvers_config` of the JSON bench configuration file.
     It usually either maps to the executable `mzn2fzn` or `minizinc` depending on the version and system. *)

open Kobecore

(** [make_command exec solver_config mzn_file dzn_file fzn_file] creates the FZN file from the MZN and DZN files. *)
val make_command: string -> Bench_desc_j.solver_config -> string -> string -> string -> string