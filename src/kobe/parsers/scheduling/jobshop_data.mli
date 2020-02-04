(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Data representation of various job shop scheduling problems:
      1. The classic job shop scheduling problem (JSSP).
         In this case, an operation is always described by exactly one machine.
      2. The flexible job shop scheduling problem (FJSP).
         In this case, an operation can be performed on one of the given machine. *)

(** A machine operation lasts `duration` time on the machine `machine_idx`. *)
type machine_operation = {
  machine_idx: int;
  duration: int;
}

(** An operation is a work that must be realized on one of the machine in the list.
    If the list is always unary, then the problem is a classic jobshop, otherwise it is the flexible extension. *)
type operation = machine_operation list

(** A job is a series of ordered operations. *)
type job = operation list

(** A jobshop problem tries to assign each operation of each job on the machines. *)
type jobshop = {
  jobs_number: int;
  machines_number: int;
  jobs: job list;
  horizon: int;
  is_flexible: bool;
}

(** Simple computation of the horizon by adding the durations of all operations for all tasks.
    It is a correct horizon since a simple (non optimal) solution is a schedule "job1-op1 ... job1-opM ... jobN-opM".
    No job is performed in parallel. *)
val compute_horizon: jobshop -> jobshop

val compute_is_flexible: jobshop -> jobshop

(** Apply the function if the operation is flexible.
    Otherwise return the empty list. *)
val if_flexible: (int -> int -> 'a) -> operation -> int -> int -> 'a list

val map_operation: jobshop -> (operation -> int -> int -> 'a) -> 'a list
val map_flexible_operation: jobshop -> (operation -> int -> int -> 'a) -> 'a list
