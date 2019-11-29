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
open Lang
open Lang.Ast
open Lang_decompositions.Cumulative
open Parsers_scheduling.Rcpsp_data
open Kobecore

module Rcpsp_model(D: Cumulative_decomposition) =
struct
  module Resource = D.C
  module Duration = D.D

  let start_job_name i = "start_j" ^ (string_of_int i)
  (* NOTE: this makespan is only correct with a dummy sink job. *)
  let makespan_name project = start_job_name project.jobs_number

  let make_task ri job =
    let r = List.nth job.resources_usage ri in
    let task = D.{
      start=start_job_name job.job_index;
      duration=Duration.of_int_up job.duration; } in
    D.{ task; id=job.job_index;
      resources_usage = Resource.of_int_up r }

  let make_tasks project ri = List.map (make_task ri) project.jobs

  let make_cumulative rcpsp project horizon ri capi =
    let capacity = List.nth rcpsp.resources_capacities capi in
    let capacity = Resource.of_int_down capacity in
    let tasks = make_tasks project ri in
    D.cumulative tasks horizon capacity D.default_name_factory

  let all_cumulatives rcpsp project =
    (* note that the index 0 does not matter here, as `resources_usage` is not used by `shared_constraint`. *)
    let tasks = make_tasks project 0 in
    let horizon = Duration.of_int_up project.horizon in
    let shared_constraints = D.shared_constraints tasks horizon D.default_name_factory in
    let cumulatives = List.mapi (make_cumulative rcpsp project horizon)
      project.resources_idx in
    let cumulatives = QFFormula (Rewritting.conjunction cumulatives) in
    Rewritting.q_conjunction [shared_constraints; cumulatives]

  let time_variables project =
    List.map (fun job -> start_job_name job.job_index) project.jobs

  let quantify_time_variables project formula =
    let exists formula name =
      Exists (name, (Concrete Duration.concrete_ty), formula) in
    let vars = time_variables project in
    List.fold_left exists formula (List.rev vars)

  let var_domain_constraints project =
    let dom u v = And(
      Cmp (Var v, GEQ, Cst (Bound_rat.zero, Int)),
      Cmp (Var v, LEQ, Cst (Bound_rat.of_int u, Int)))
    in
    let vars = time_variables project in
    Rewritting.conjunction
      ([(dom 0 (List.hd vars))]@(List.map (dom project.horizon) (List.tl vars)))

  (* Generalized temporal constraints: ensure a precedence (with a possible timelag) between the tasks. *)
  let temporal_constraints project =
    let precedence_constraint (prec:precedence) (j,w) =
      D.precedence (start_job_name prec.job_index) (start_job_name j) (Duration.of_int_up w) in
    let all_successors (precedence:precedence) =
      List.map (precedence_constraint precedence)
        (List.map2 (fun x y -> (x,y)) precedence.job_successors precedence.weights) in
    Rewritting.conjunction
      (List.flatten (List.map all_successors project.precedence_relations))

  let formula_of_rcpsp rcpsp =
    let project = List.hd rcpsp.projects in
    let makespan = makespan_name project in
    let var_domains = var_domain_constraints project in
    let precedences = temporal_constraints project in
    let cumulatives = all_cumulatives rcpsp project in
    let all_constraints = Rewritting.q_conjunction
      [(QFFormula (Rewritting.conjunction [var_domains; precedences]));
      cumulatives] in
    { qf=(quantify_time_variables project all_constraints);
      optimise=(Minimize makespan) }
end

let help_msg = "Currently, only `TimeRD` or `TaskRD` decomposition for cumulative is supported."

module RCPSP_TaskRD = Rcpsp_model(MakeTaskRD(Bound_int)(Bound_int))
module RCPSP_TimeRD = Rcpsp_model(MakeTimeRD(Bound_int))

let formula_of_rcpsp rcpsp (decompositions: Bench_instance_j.decomposition list) =
  if List.length decompositions > 1 then
    System.eprintf_and_exit ("More than one decomposition for RCPSP were given.\n" ^ help_msg)
  else
    let decomposition = List.hd decompositions in
    match decomposition.name with
    | "TimeRD" -> RCPSP_TimeRD.formula_of_rcpsp rcpsp
    | "TaskRD" -> RCPSP_TaskRD.formula_of_rcpsp rcpsp
    | _ -> System.eprintf_and_exit (
        "Unknown cumulative decomposition for RCPSP `" ^ decomposition.name ^ "`.\n" ^ help_msg)
