(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Scanf
open Jobshop_data
open Parse_utility
open Jobshop_jss

let read_operation file _ =
  let number_of_machines = bscanf file " %d " (fun a -> a) in
  List.map (read_machine_op file) (Tools.range 1 number_of_machines)

let read_job file jobshop _ =
  let number_of_operations = bscanf file " %d " (fun a -> a) in
  let ops = List.map (read_operation file) (Tools.range 1 number_of_operations) in
  { jobshop with jobs=ops::jobshop.jobs }

let read_jobs file jobshop =
  let jobshop = List.fold_left (read_job file) jobshop (Tools.range 1 jobshop.jobs_number) in
  { jobshop with jobs=(List.rev jobshop.jobs) }

let read_jobshop file =
  ignore_comments file;
  let jobshop = read_jobshop_info file in
  (* Skip the potential third number representing the average of machine per operation. *)
  ignore_lines file 1;
  read_jobs file jobshop

let read_flexible_jobshop_file (problem_path: string) : jobshop =
  let file = Scanning.open_in problem_path in
  let jobshop = read_jobshop file in
  Scanning.close_in file;
  let jobshop = compute_horizon jobshop in
  compute_is_flexible jobshop
