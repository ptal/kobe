(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Core
open Lang
open Lang.Ast
open Lang_decompositions
open Parsers_scheduling.Jobshop_data
open Model_utility

module S = Scheduling.Make(Bound_int)

let name_of head i j = head ^ "_j" ^ (string_of_int i) ^ "_o" ^ (string_of_int j)
let start_job_name = name_of "start"
let duration_job_name = name_of "duration"
let machine_job_name = name_of "machine"

let pv_duration_of op i j =
  match op with
  | [] -> failwith "Malformed data: A job without operation."
  | [op_machine] -> S.Param op_machine.duration
  | _ -> S.Variable (duration_job_name i j)

let makespan_name = "makespan"

let start_variables jobshop =
  map_operation jobshop (fun _ i j -> start_job_name i j)

let duration_variables jobshop =
  List.flatten
    (map_operation jobshop (if_flexible duration_job_name))

let machine_variables jobshop =
  List.flatten
    (map_operation jobshop (if_flexible machine_job_name))

let quantify jobshop qf =
  let vars =
    makespan_name::
    (start_variables jobshop)@
    (machine_variables jobshop)@
    (duration_variables jobshop) in
  quantify_vars vars (Concrete Int) qf

let start_vars_domain jobshop =
  let start_vars = (start_variables jobshop)@[makespan_name] in
  dom_of_vars start_vars Int Bound_rat.zero (Bound_rat.of_int jobshop.horizon)

let min_max_dom jobshop int_of_op make_name =
  map_flexible_operation jobshop (fun op i j ->
    let (min_val, max_val) = List.fold_left
      (fun (min_val,max_val) op_machine ->
         min min_val (int_of_op op_machine), max max_val (int_of_op op_machine))
      (max_int, 0) op in
    let name = make_name i j in
    dom_of_var Int (Bound_rat.of_int min_val) (Bound_rat.of_int max_val) name)

let duration_vars_domain jobshop =
  min_max_dom jobshop (fun op -> op.duration) duration_job_name

let machine_vars_domain jobshop =
  min_max_dom jobshop (fun op -> op.machine_idx) machine_job_name

let var_domain_constraints jobshop =
  (start_vars_domain jobshop)@
  (duration_vars_domain jobshop)@
  (machine_vars_domain jobshop)

(* let start_job i j = Var (start_job_name i j) *)
(* let end_job jobshop i j = Binary (start_job i j, ADD, duration_of jobshop i j) *)
(* let last_job_op jobshop i = end_job jobshop i ((List.length (List.nth jobshop.jobs i)) - 1) *)
(* let makespan_constraint jobshop =
  let rec max job_idx =
    let last_op = last_job_op jobshop job_idx in
    if job_idx = (jobshop.jobs_number - 1) then
      last_op
    else
      Funcall("max", [last_op; max (job_idx+1)])
  in
    Cmp (Var makespan_name, EQ, max 0) *)

let makespan_constraint jobshop =
  let precedences = List.mapi (fun i ops ->
      let j = (List.length ops) - 1 in
      let op = List.nth ops j in
      S.precedence (start_job_name i j) makespan_name (pv_duration_of op i j)
    ) jobshop.jobs in
  precedences

let all_operations_on_machine jobshop machine_idx =
  List.flatten (map_operation jobshop (fun op i j ->
    if List.exists (fun op_machine -> op_machine.machine_idx = machine_idx) op then
      [(op, i, j)]
    else
      []
  ))

let make_task (op, i, j) =
  S.{
    start=start_job_name i j;
    duration=pv_duration_of op i j
  }

let flexible_non_overlap (op1, i1, j1) (op2, i2, j2) =
  if i1 < i2 then
    let t1, t2 =
      make_task (op1, i1, j1),
      make_task (op2, i2, j2) in
    match t1.duration, t2.duration with
    | S.Param _, S.Param _ -> []
    | _ ->
      let make_machine op i j =
        if List.length op = 1 then
          let idx = Bound_rat.of_int (List.hd op1).machine_idx in
          Cst (idx, Int)
        else
          Var (machine_job_name i j)
      in
      let l = make_machine op1 i1 j1 in
      let r = make_machine op2 i2 j2 in
      [Imply(Cmp (l, EQ, r), S.non_overlap t1 t2)]
  else
    []

let no_same_time_same_machine_job jobshop machine_idx =
  let disjunctive_ops = all_operations_on_machine jobshop machine_idx in
  let single_machines, multi_machines =
    if jobshop.is_flexible then
      let single_machine = List.filter (fun (op,_,_) -> List.length op = 1) disjunctive_ops in
      let disjunctives = Tools.for_all_pairs disjunctive_ops flexible_non_overlap in
      single_machine, disjunctives
    else
      disjunctive_ops, [] in
  let tasks = List.map make_task single_machines in
  match tasks with
  | [] | [_] -> multi_machines
  | l -> (S.disjunctive l)::multi_machines

(* Ensure two jobs using the same machine are not scheduled at the same time. *)
let disjunctive_machine jobshop =
  let disjunctives = List.map
    (no_same_time_same_machine_job jobshop)
    (Tools.range 1 jobshop.machines_number) in
  List.flatten disjunctives

(* Generalized temporal constraints: ensure a precedence between the operations of the same job. *)
let temporal_constraints jobshop =
  List.flatten (map_operation jobshop (fun op i j ->
    if j < ((List.length (List.nth jobshop.jobs i)) - 1) then
      [S.precedence
        (start_job_name i j)
        (start_job_name i (j+1))
        (pv_duration_of op i j)]
    else
      []
  ))

let op_duration_constraints jobshop =
  map_flexible_operation jobshop (fun op i j ->
    let m = Var (machine_job_name i j) in
    let d = Var (duration_job_name i j) in
    let possible_durations =
      List.map (fun op_machine ->
        let m' = Cst (Bound_rat.of_int op_machine.machine_idx, Int) in
        let d' = Cst (Bound_rat.of_int op_machine.duration, Int) in
        And(Cmp(m, EQ, m'), Cmp(d, EQ, d'))
      ) op in
    Rewritting.disjunction possible_durations
  )

let formula_of_jobshop jobshop =
  let var_domains = var_domain_constraints jobshop in
  let makespan = makespan_constraint jobshop in
  let precedences = temporal_constraints jobshop in
  let disjunctives = disjunctive_machine jobshop in
  let op_duration = op_duration_constraints jobshop in
  let all_constraints = QFFormula (Rewritting.conjunction
    (var_domains@precedences@disjunctives@op_duration@makespan)) in
  { qf=(quantify jobshop all_constraints);
    optimise=(Minimize makespan_name) }
