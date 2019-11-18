(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Execute a solver according to `Solver`.
    After conversion of the solver's output to `Measure`, we print it as a CSV on the standard output. *)

open Solvers
open Kobecore.Bench_instance_j
open Kobecore.Bench_desc_j

module type S =
sig
  val run: bench_instance -> solver_config -> solver_option option -> string -> string -> unit
end

module Make(Solver: Solver_sig.S): S