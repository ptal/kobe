(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Generators
open Parsers.Dispatch
open Solvers
open Kobecore.System
open Kobecore.Bench_instance_j
open Lang.Ast

let xcsp3_file = "tmp.xml"

let make_xcsp3_file (bf: bab_qformula) =
  let xcsp3_file = make_unique_file_name xcsp3_file in
  let model = Formula2xcsp3.xcsp3_of_bab_qformula bf in
  let _ = string_to_file xcsp3_file model in
  xcsp3_file

let bench_instance bench (xcsp3_kind_solver: xcsp3_solver) problem_path =
    match dispatch problem_path with
    | WARNING msg -> print_warning msg
    | IGNORE -> ()
    | PROBLEM problem ->
        let bf = Models.formula_of_problem xcsp3_kind_solver.decompositions problem in
        let xcsp3_file = make_xcsp3_file bf in
        let (module Solver: Solver_sig.S) = Solver_sig.make_solver xcsp3_kind_solver.solver.name in
        let (module Runner: Solver_runner.S) = (module Solver_runner.Make(Solver)) in
        Runner.run bench xcsp3_kind_solver.solver xcsp3_kind_solver.option problem_path xcsp3_file

let bench_xcsp3 bench (solver: xcsp3_solver) =
  Csv_printer.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter
    (bench_instance bench solver)
    problems
