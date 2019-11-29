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
open Kobecore

type measure = {
  problem_path: string;
  time: int64 option;
  memory: int64 option;
  stats: Transformer.global_statistics;
  optimum: Bound_rat.t option;
  satisfiability: Kleene.t;
}

let init stats problem_path =
  { problem_path=problem_path;
    time=None;
    memory=None;
    stats=stats;
    optimum=None;
    satisfiability=Kleene.Unknown }

let default problem_path =
  init {
    start = Mtime_clock.counter ();
    elapsed = Mtime.Span.zero;
    nodes = -1;
    fails = -1;
    sols = -1;
    prunes = -1;
    depth_max = -1;
    restarts = -1;
  } problem_path

let root_unsat problem_path =
  let stats = Transformer.{
    start = Mtime_clock.counter ();
    elapsed = Mtime.Span.zero;
    nodes = 1;
    fails = 1;
    sols = 0;
    prunes = 0;
    depth_max = 0;
    restarts = 0;
  } in
  { problem_path=problem_path;
    time=Some Int64.zero;
    memory=None;
    stats=stats;
    optimum=None;
    satisfiability=Kleene.False }

let update_time bench stats measure =
  let time_out = System.timeout_of_bench bench in
  let open Transformer in
  if Mtime.Span.compare time_out stats.elapsed <= 0 then measure
  else { measure with time=(Some (Mtime.Span.to_uint64_ns stats.elapsed)) }

let guess_missing_measures m =
  match m.optimum, m.satisfiability, m.time, m.stats.sols with
  | Some _, _, _, _ -> { m with satisfiability=True }
  | _, _, _, x when x > 0 -> { m with satisfiability=True }
  | None, Unknown, Some _, (-1)
  | None, False, Some _, (-1)
  | None, _, Some _, 0 ->
      { m with satisfiability=False; stats={m.stats with sols=0 } }
  | _ -> m
