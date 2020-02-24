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
open Kobecore.System

let run_bench bench =
  match bench.solver_instance with
  | `AbsoluteSolver(solver) -> Benchers.Absolute.bench_absolute bench solver
  | `MznSolver(solver) -> Benchers.Mzn.bench_mzn bench solver
  | `FznSolver(solver) -> Benchers.Fzn.bench_fzn bench solver
  | `StandaloneSolver(solver) -> Benchers.Standalone.bench_standalone bench solver

let bench_from_json json_data =
  try
    bench_instance_of_string json_data
  with
  | Atdgen_runtime__Oj_run.Error(msg)
  | Yojson.Json_error(msg) ->
      eprintf_and_exit (Printf.sprintf
        "The benchmarks description file contains an error:\n\n\
         %s\n\n\
        [help] Be careful to the case: \"int\" is not the same as \"Int\".\n\
        [help] You can find a full example of the JSON format in benchmark/data/benchmarks.json." msg)

let () =
  Printexc.record_backtrace false;
  let bench = bench_from_json (get_bench_desc ()) in
  run_bench bench
  (* Printf.printf "%s" (Yojson.Safe.prettify (string_of_bench_instance bench)) *)
