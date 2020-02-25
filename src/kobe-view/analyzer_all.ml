(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Data_analyzer

type strategy_2 = {
  solver_name : string;
  strategy_name : string;
  all: (string, instance) Hashtbl.t;
  steps : (float*int) list; (*time * value*)
}

type instances_set_2 = {
  problem_name : string;
  instance_name : string;
  nb_instances: int;
  strategies : strategy_2 list;
}

let timeout = 600.

let add_key key _ li =
  key::li

let get_keys tbl =
  Hashtbl.fold (add_key) tbl []

let remove_last_char chars =
  let size = String.length chars in
  if size > 0 then
    String.sub chars 0 ((String.length chars) -1)
  else chars

let float_option_to_string t =
  match t with
  | Some(t) -> (string_of_float t)^"0"
  | None -> (string_of_float timeout)^"0"

let convert_solver (solver : solver) (strategy : strategy) =
  {solver_name = solver.name; strategy_name = strategy.name; all = strategy.all; steps = [];}

let append_solvers solvers =
  let rec aux (solvers : solver list) strategies =
    match solvers with
    | [] -> strategies
    | s::solvers ->
        let li = List.map (convert_solver s) s.strategies in
        aux solvers (List.append strategies li)
  in aux solvers []

let convert_instance problem (instances_set : instances_set) =
  Printf.printf "%s\n" instances_set.name;
  let nb_instances = (Hashtbl.length (List.hd (List.hd instances_set.solvers).strategies).all) in
  {problem_name = problem.name; instance_name = instances_set.name; nb_instances = nb_instances;strategies = append_solvers instances_set.solvers}

let append_problems (database : problem list) =
  let rec aux database instances =
    match database with
    | [] -> instances
    | p::database ->
        let li = List.map (convert_instance p) p.instances_set in
        aux database (instances@li)
  in aux database []
