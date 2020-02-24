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
open Parsers_scheduling.Jobshop_data
open Mzn_utility

let index_set_of l =
  snd (Tools.fold_map (fun n x ->
    n + (List.length x), (n, n + (List.length x) - 1)
  ) 1 l)

(* Note that indices starts at 1 in the MiniZinc model. *)
let make_dzn_data jobshop =
  if not jobshop.is_flexible then ""
  else
    let no_task, no_optt =
      List.fold_left (fun (no_task, no_optt) job ->
        let no_task = no_task + List.length job in
        let no_optt = List.fold_left (fun no_optt operations ->
          no_optt + List.length operations) no_optt job in
        no_task, no_optt
      ) (0,0) jobshop.jobs in
    let tasks = index_set_of jobshop.jobs in
    let flat_ops = List.flatten jobshop.jobs in
    let optts = index_set_of flat_ops in
    let machine_ops = List.flatten flat_ops in
    (Printf.sprintf "no_mach = %d;\n" jobshop.machines_number) ^
    (Printf.sprintf "no_jobs = %d;\n" jobshop.jobs_number) ^
    (Printf.sprintf "no_task = %d;\n" no_task) ^
    (Printf.sprintf "no_optt = %d;\n" no_optt) ^
    (string_of_set_list "tasks" tasks) ^
    (string_of_set_list "optts" optts) ^
    (list_to_mzn "optt_mach" (List.map (fun mo -> mo.machine_idx) machine_ops)) ^
    (list_to_mzn "optt_dur" (List.map (fun mo -> mo.duration) machine_ops))
