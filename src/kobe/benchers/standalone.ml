(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore
open Kobecore.Bench_instance_j
open Solvers

let bench_instance bench (solver: standalone_solver) problem_path =
  let (module Solver: Solver_sig.S) = Solver_sig.make_solver solver.solver.name in
  let (module Runner: Solver_runner.S) = (module Solver_runner.Make(Solver)) in
  Runner.run bench solver.solver solver.option problem_path problem_path

let bench_standalone bench solver =
  Csv_printer.print_csv_header bench;
  let problems = System.list_of_problems bench in
  List.iter (bench_instance bench solver) problems
