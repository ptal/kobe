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

(* let duration_of' jobshop i j =
  let op = List.nth (List.nth jobshop.jobs i) j in
  op.duration *)

(* let duration_of jobshop i j = Cst (Bound_rat.of_int (duration_of' jobshop i j),Int) *)

let start_job_name i j = "start_j" ^ (string_of_int i) ^ "_o" ^ (string_of_int j)
(* let start_job i j = Var (start_job_name i j) *)

(* let end_job jobshop i j = Binary (start_job i j, ADD, duration_of jobshop i j) *)
(* let last_job_op jobshop i = end_job jobshop i ((List.length (List.nth jobshop.jobs i)) - 1) *)

let makespan_name = "makespan"

let time_variables jobshop =
  List.flatten (
    List.mapi (fun i job ->
      List.mapi (fun j _ -> start_job_name i j) job) jobshop.jobs)

let quantify jobshop qf =
  let vars = time_variables jobshop in
  let f = quantify_vars vars (Concrete Int) qf in
  Exists (makespan_name, Concrete Int,f)

let var_domain_constraints jobshop =
  let vars = makespan_name::(time_variables jobshop) in
  dom_of_vars vars Int Bound_rat.zero (Bound_rat.of_int jobshop.horizon)

(* let makespan_constraint jobshop =
  let rec max job_idx =
    let last_op = last_job_op jobshop job_idx in
    if job_idx = (jobshop.jobs_number - 1) then
      last_op
    else
      Funcall("max", [last_op; max (job_idx+1)])
  in
    Cmp (Var makespan_name, EQ, max 0) *)

module S = Scheduling.Make(Bound_int)

let makespan_constraint jobshop =
  let precedences = List.mapi (fun i ops ->
      let j = (List.length ops) - 1 in
      let op = List.nth ops j in
      S.precedence (start_job_name i j) makespan_name op.duration
    ) jobshop.jobs in
  Rewritting.conjunction precedences

let no_same_time_same_machine_job jobshop machine_idx =
  let tasks = List.flatten (List.mapi (fun i ops ->
       ops
    |> List.mapi (fun j op -> (j,op))
    |> List.filter (fun (_, op) -> op.machine_idx = machine_idx)
    |> List.map (fun (j, (op:operation)) ->
      S.{
        start=start_job_name i j;
        duration=op.duration
      })
    ) jobshop.jobs) in
  S.disjunctive tasks

(* Ensure two jobs using the same machine are not scheduled at the same time. *)
let disjunctive_machine jobshop =
  let disjunctives = List.map
    (no_same_time_same_machine_job jobshop)
    (Tools.range 0 (jobshop.machines_number - 1)) in
  Rewritting.conjunction disjunctives

(* Generalized temporal constraints: ensure a precedence between the operations. *)
let temporal_constraints jobshop =
  let precedences = List.flatten (List.mapi (fun i ops ->
    List.flatten (List.mapi (fun j op ->
      if j < ((List.length ops) - 1) then
        [S.precedence (start_job_name i j) (start_job_name i (j+1)) op.duration]
      else
        []
    ) ops)
  ) jobshop.jobs) in
  Rewritting.conjunction precedences

let formula_of_jobshop jobshop =
  let var_domains = var_domain_constraints jobshop in
  let makespan = makespan_constraint jobshop in
  let precedences = temporal_constraints jobshop in
  let disjunctive = disjunctive_machine jobshop in
  let all_constraints = QFFormula (
    Rewritting.conjunction [var_domains; precedences; disjunctive; makespan]) in
  { qf=(quantify jobshop all_constraints);
    optimise=(Minimize makespan_name) }
