(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(**  *)

open Core
open Scanf
open Rcpsp_data
open Parse_utility

let read_rcpsp_info file =
  let (jobs_number, resources_number) = bscanf file " %d %d " (fun a b -> (a,b)) in
  let resources_capacities = read_trailing_int_list file resources_number in
  let project =
  { project_idx = 0;
    jobs_number=jobs_number;
    horizon=0;
    precedence_relations=[];
    jobs=[];
    resources_idx=Tools.range 0 (resources_number-1)
  } in
  { resources_capacities=resources_capacities;
    projects=[project] }

let read_job file project job_index =
  let duration = bscanf file " %d " (fun a -> a) in
  let resources_usage = read_trailing_int_list file (List.length project.resources_idx) in
  let successor_number = bscanf file " %d " (fun a -> a) in
  let job_successors = read_trailing_int_list file successor_number in
  let job = {
    job_index=job_index;
    mode=1;
    duration=duration;
    resources_usage=resources_usage;
  } in
  let precedence = {
    job_index=job_index;
    mode=1;
    successors=successor_number;
    job_successors=job_successors;
    weights=List.map (fun _ -> duration) job_successors;
  } in
  { project with
      jobs=project.jobs@[job];
      precedence_relations=project.precedence_relations@[precedence] }

let read_jobs file rcpsp =
  map_projects (fun project ->
    List.fold_left (read_job file) project (Tools.range 1 project.jobs_number)
  ) rcpsp

let read_patterson file =
  read_rcpsp_info file |>
  read_jobs file |>
  map_projects compute_horizon

let read_patterson_file (problem_path: string) : rcpsp =
  let file = Scanning.open_in problem_path in
  let rcpsp = read_patterson file in
  Scanning.close_in file;
  rcpsp
