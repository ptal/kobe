(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Parsers.Dispatch
open Solvers
open Kobecore.System
open Kobecore.Bench_instance_j
open Lang
open Lang.Ast

let skm_file = "tmp.skm"

let make_skm_file (bf: bab_qformula) =
  let skm_file = make_unique_file_name skm_file in
  let model = Pretty_print.string_of_bab_qformula bf in
  let _ = string_to_file skm_file model in
  skm_file

let bench_instance bench (skm_kind_solver: skm_solver) problem_path =
    match dispatch problem_path with
    | WARNING msg -> print_warning msg
    | IGNORE -> ()
    | PROBLEM problem ->
        let bf = Models.formula_of_problem skm_kind_solver.decompositions problem in
        let skm_file = make_skm_file bf in
        let (module Solver: Solver_sig.S) = Solver_sig.make_solver skm_kind_solver.solver.name in
        let (module Runner: Solver_runner.S) = (module Solver_runner.Make(Solver)) in
        Runner.run bench skm_kind_solver.solver skm_kind_solver.option problem_path skm_file

let bench_skm bench (solver: skm_solver) =
  Csv_printer.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter
    (bench_instance bench solver)
    problems
