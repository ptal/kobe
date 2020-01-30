(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** An operation lasts `duration` time on the machine `machine_idx`. *)
type operation = {
  machine_idx: int;
  duration: int;
}

(** A job is a series of ordered operations. *)
type job = operation list

(** A jobshop problem tries to assign each operation of each job on the machines. *)
type jobshop = {
  jobs_number: int;
  machines_number: int;
  jobs: job list;
  horizon: int;
}

(** Simple computation of the horizon by adding the durations of all operations for all tasks.
    It is a correct horizon since a simple (non optimal) solution is a schedule "job1-op1 ... job1-opM ... jobN-opM".
    No job is performed in parallel. *)
val compute_horizon: jobshop -> jobshop
