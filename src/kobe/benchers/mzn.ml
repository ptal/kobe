(* Copyright 2019 Pierre Talbot

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
open Kobecore
open Kobecore.System
open Kobecore.Bench_instance_j

let dzn_file = "tmp.dzn"
let mzn_file = "tmp.mzn"
let fzn_file = "tmp.fzn"

let create_search_annot strategy =
  "solve::" ^ strategy.Bench_desc_j.plain ^ ";"

let make_mzn_file (solver: mzn_solver) _ =
  let model = file_to_string solver.model in
  let search = create_search_annot solver.fzn.strategy in
  let data = model ^ "\n" ^ search in
  make_file data mzn_file

let make_dzn_file problem =
  let data =
    match problem with
    | JOBSHOP jobshop -> Jobshop2dzn.make_dzn_data jobshop
    | RCPSP rcpsp -> Rcpsp2dzn.make_dzn_data rcpsp
    | SAT _ -> "" (* NOTE: the model already contains everything required. *)
  in
  make_file data dzn_file

let make_fzn_file fzn_kind_solver mzn_file dzn_file =
  let fzn_file = make_unique_file_name fzn_file in
  let command = Mzn2fzn.make_command fzn_kind_solver.mzn2fzn.exec
    fzn_kind_solver.solver mzn_file dzn_file fzn_file in
  let _ = call_command command in
  fzn_file

let bench_instance bench fzn_kind_solver make_mzn_file make_dzn_file problem_path =
    match dispatch problem_path with
    | WARNING msg -> print_warning msg
    | IGNORE -> ()
    | PROBLEM problem ->
        let mzn_file, dzn_file = make_mzn_file problem, make_dzn_file problem in
        let fzn_file = make_fzn_file fzn_kind_solver mzn_file dzn_file in
        let (module Solver: Solver_sig.S) = Solver_sig.make_solver fzn_kind_solver.solver.name in
        let (module Runner: Solver_runner.S) = (module Solver_runner.Make(Solver)) in
        Runner.run bench fzn_kind_solver.solver fzn_kind_solver.option problem_path fzn_file

let bench_mzn' bench fzn_kind_solver make_mzn_file make_dzn_file =
  Csv_printer.print_csv_header bench;
  let problems = list_of_problems bench in
  List.iter
    (bench_instance bench fzn_kind_solver make_mzn_file make_dzn_file)
    problems

let bench_mzn bench (solver: mzn_solver) =
  bench_mzn' bench solver.fzn (make_mzn_file solver) make_dzn_file
