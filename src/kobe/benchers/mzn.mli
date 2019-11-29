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
open Parsers.Dispatch

val dzn_file: string
val mzn_file: string
val fzn_file: string

(** [bench_mzn' bench fzn_kind_solver make_mzn_file make_dzn_file] is similar to `bench_mzn` but is parametrized by functions to create the MZN and DZN files. *)
val bench_mzn': bench_instance -> fzn_kind_solver -> (problem -> string) -> (problem -> string) -> unit

(** Benchmark a set of problems with a MZN-based solver.
    It prints the benchmark result in a CSV format on the standard output.
    Temporary files (.mzn, .dzn, .fzn) are created in "/tmp" with fresh name (e.g. `tmp12.mzn`).  *)
val bench_mzn: bench_instance -> mzn_solver -> unit
