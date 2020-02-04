(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type machine_operation = {
  machine_idx: int;
  duration: int;
}

type operation = machine_operation list

type job = operation list

type jobshop = {
  jobs_number: int;
  machines_number: int;
  jobs: job list;
  horizon: int;
  is_flexible: bool;
}

let compute_horizon jobshop =
  let horizon = List.fold_left (fun acc ops ->
      List.fold_left (fun acc machines ->
        acc + List.fold_left (fun acc op -> max acc op.duration) 0 machines
      ) acc ops
    ) 0 jobshop.jobs
  in { jobshop with horizon }

let compute_is_flexible jobshop =
  let is_flexible = List.exists (fun job ->
      List.exists (fun op -> List.length op > 1) job)
    jobshop.jobs in
  { jobshop with is_flexible }

let if_flexible f op i j =
  if List.length op > 1 then
    [f i j]
  else
    []

let map_operation jobshop f =
  List.flatten (
    List.mapi (fun i ops ->
      List.mapi (fun j op -> f op i j) ops) jobshop.jobs)

let map_flexible_operation jobshop f =
  List.flatten (
    map_operation jobshop (
      fun op i j -> if_flexible (f op) op i j))
