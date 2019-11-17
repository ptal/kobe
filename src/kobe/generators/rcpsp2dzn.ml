(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Parsers_scheduling.Rcpsp_data

let string_of_list to_string l =
  List.fold_left (fun s e -> s ^ (to_string e) ^ ", ") "" l

let list_to_mzn name l =
  name ^ " = [" ^
  (string_of_list string_of_int l) ^
  "];\n"

let string_of_2D_list name l =
  name ^ " = [|\n" ^
  (List.fold_left (fun a r -> a ^ (string_of_list string_of_int r) ^ "\n  |") "" l) ^
  "];\n"

let string_of_resources project =
  let rr = List.map
    (fun r_idx -> List.map (fun j -> List.nth j.resources_usage r_idx) project.jobs)
    project.resources_idx in
  string_of_2D_list "rr" rr

let string_of_temporal_constraints project =
  let dc = List.flatten (List.map (fun (p:precedence) ->
      List.map2 (fun w s -> [p.job_index; w; s]) p.weights p.job_successors
    ) project.precedence_relations) in
  string_of_2D_list "dcons" dc

let make_dzn_data rcpsp project =
  (Printf.sprintf "n_res = %d;\n" (List.length rcpsp.resources_capacities)) ^
  (list_to_mzn "rcap" (List.map (List.nth rcpsp.resources_capacities) project.resources_idx)) ^
  (Printf.sprintf "n_tasks = %d;\n" project.jobs_number) ^
  (list_to_mzn "dur" (List.map (fun j -> j.duration) project.jobs)) ^
  (string_of_resources project) ^
  (Printf.sprintf "n_dc = %d;\n" (List.fold_left (+) 0 (List.map (fun j -> j.successors) project.precedence_relations))) ^
  (string_of_temporal_constraints project)
