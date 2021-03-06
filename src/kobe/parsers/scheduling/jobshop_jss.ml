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

let ignore_comments file =
  try
    let rec ignore_line () =
      ignore(bscanf file " #" (fun _ -> ()) "");
      ignore_lines file 1;
      ignore_line ()
    in
      ignore_line ()
  with Scanf.Scan_failure _ -> ()

let read_jobshop_info file =
  let (jobs_number, machines_number) = bscanf file " %d %d" (fun a b -> (a,b)) in
  { jobs_number; machines_number; jobs = []; horizon=max_int; is_flexible=false }

let read_machine_op file _ =
  let (machine_idx, duration) = bscanf file " %d %d " (fun a b -> (a,b)) in
  {machine_idx; duration}

let read_job file jobshop _ =
  let ops = List.map (read_machine_op file) (Tools.range 1 jobshop.machines_number) in
  let job = List.map (fun opm -> [opm]) ops in
  { jobshop with jobs=job::jobshop.jobs }

let read_jobs file jobshop =
  let jobshop = List.fold_left (read_job file) jobshop (Tools.range 1 jobshop.jobs_number) in
  { jobshop with jobs=(List.rev jobshop.jobs) }

let read_jobshop file =
  ignore_comments file;
  read_jobshop_info file |>
  read_jobs file

let read_jobshop_file (problem_path: string) : jobshop =
  let file = Scanning.open_in problem_path in
  let jobshop = read_jobshop file in
  Scanning.close_in file;
  let jobshop = compute_horizon jobshop in
  compute_is_flexible jobshop
