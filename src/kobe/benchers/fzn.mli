(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore.Bench_instance_j

(** Same as `Mzn.bench_mzn` but with FZN solvers.
    In comparison with MZN solver, the MiniZinc model is already provided and must not be built. *)
val bench_fzn: bench_instance -> fzn_solver -> unit
