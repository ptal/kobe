(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore.Bench_instance_j

val skm_file: string

(** Benchmark a set of problems with a SKM-based solver.
    It prints the benchmark result in a CSV format on the standard output.
    Temporary files (.skm) are created in "/tmp" with fresh name (e.g. `/tmp/tmp12.skm`).  *)
val bench_skm: bench_instance -> skm_solver -> unit
