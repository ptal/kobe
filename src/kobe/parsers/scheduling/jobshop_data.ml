(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

type operation = {
  machine_idx: int;
  duration: int;
}

type job = operation list

type jobshop = {
  jobs_number: int;
  machines_number: int;
  jobs: job list;
  horizon: int;
}

let compute_horizon jobshop =
  let horizon = List.fold_left (fun acc job ->
      List.fold_left (fun acc op -> acc + op.duration) acc job)
    0
    jobshop.jobs
  in { jobshop with horizon }
