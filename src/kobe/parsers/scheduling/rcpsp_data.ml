(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type resources_info = {
  renewable: int;
  nonrenewable: int;
  doubly_constrained: int;
}

let check_resources_info r =
  if r.nonrenewable > 0 || r.doubly_constrained > 0 then
    failwith "ProGenMax model with nonrenewable or doubly constrained resources: not yet implemented."
  else
    r

type precedence = {
  job_index: int;
  mode: int;
  successors: int;
  job_successors: int list;
  weights: int list;
}

type job = {
  job_index: int;
  mode: int;
  duration: int;
  resources_usage: int list
}

type project = {
  project_idx: int;
  jobs_number: int;
  horizon: int;
  precedence_relations: precedence list;
  jobs: job list;
  resources_idx: int list;
}

type rcpsp = {
  resources_capacities: int list;
  projects: project list;
}

let map_projects f rcpsp = { rcpsp with projects = (List.map f rcpsp.projects) }

let number_of_resources rcpsp = List.length rcpsp.resources_capacities

let compute_horizon project =
  let horizon = List.fold_left (fun a j ->
    let weights = List.flatten (List.map (fun (p:precedence) ->
      if p.job_index = j.job_index then p.weights else []) project.precedence_relations) in
    let max_dur = List.fold_left max j.duration weights in
    a + max_dur
    ) 0 project.jobs in
  {project with horizon = horizon}
