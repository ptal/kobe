(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Fixpoint
open Kobecore.Bench_instance_j

type measure = {
  problem_path: string;
  time: int64 option;
  (** In this context `int64` represents the number of nano-seconds.
      An empty option represents a timeout. *)

  memory: int64 option;
  (** The number of bytes.
      It is empty if we lack this information. *)

  stats: Transformer.global_statistics;
  optimum: Bound_rat.t option;
  satisfiability: Kleene.t;
}

(** [init stats problem_path] initializes a measure with the given statistics and problem path. *)
val init: Transformer.global_statistics -> string -> measure

(** [default problem_path] initializes a measure to default values.
    It sets to `-1` non-optional integer fields, to `None` optional field. *)
val default: string -> measure

(** [update_time bench stats measure] updates the time entry of `measure` according to `stats`.
    It takes into account possible timeout according to `bench`. *)
val update_time: bench_instance -> Transformer.global_statistics -> measure -> measure

(** [guess_missing_measures measure] fills some blank in the measure that can be deduced from existing data. *)
val guess_missing_measures: measure -> measure
