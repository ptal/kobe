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
open Generators

let make_mzn_file (solver: fzn_solver) problem =
  let formula = Models.formula_of_problem solver.decompositions problem in
  let mzn_data = Formula2mzn.mzn_of_bab_qformula formula solver.fzn.strategy in
  System.make_file mzn_data Mzn.mzn_file

let bench_fzn bench solver =
  Mzn.bench_mzn' bench solver.fzn (make_mzn_file solver) (fun _ -> "")
