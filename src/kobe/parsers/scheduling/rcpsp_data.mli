(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Generic data structure of the RCPSP/max problem.
    It actually includes more data than what is currently supported by the model generator.
    For instance, multiple projects or nonrenewable resources are not yet supported.
    "Task" and "job" are synonyms. *)

(** The information on the number and kind of resources available.
    We only store renewable resources for now. *)
type resources_info = {
  renewable: int;
  nonrenewable: int;
  doubly_constrained: int;
}

(** Check the validity of the resources because we only support renewable resources for now. *)
val check_resources_info: resources_info -> resources_info

(** Describe a task `job_index` with all the other tasks with precedence constraints relative to this one. *)
type precedence = {
  job_index: int;
  (** Index of the task relatively to `project.jobs`. *)

  mode: int;
  (** The mode is currently not supported (it models a task with different operating mode). *)

  successors: int;
  job_successors: int list;
  weights: int list;
  (** For all successor job_j: job_i + weights_ij <= job_j *)
}

type job = {
  job_index: int;
  (** Index of the task relatively to `project.jobs`. *)

  mode: int;
  (** The mode is currently not supported (it models a task with different operating mode). *)

  duration: int;
  resources_usage: int list
  (** The length of `resources_usage` must match `rcpsp.resources_capacities`.
      The resources really used by the project are given in `project.resources`. *)
}

(** A project is a set of tasks using resources and subject to temporal constraints.
    Two projects have distinct tasks but rely on a same pool of resources. *)
type project = {
  project_idx: int;
  jobs_number: int;
  (** Include dummy source and sink. *)

  horizon: int;
  precedence_relations: precedence list;
  jobs: job list;
  resources_idx: int list;
  (** The indexes of the resources used by the tasks.
      It maps into the list `rcpsp.resources_capacities`. *)
}

(** Data structure for the multi-project RCPSP/max problem.
    Currently only single project are supported. *)
type rcpsp = {
  resources_capacities: int list;
  projects: project list;
}

val map_projects: (project -> project) -> rcpsp -> rcpsp
val number_of_resources: rcpsp -> int

(** Simple computation of the horizon by adding the maximum durations of all tasks.
    The maximum duration includes the time delay between this task and the possible next. *)
val compute_horizon: project -> project
