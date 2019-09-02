(* Constraint model of the RCPSP. *)
open Bounds
open Lang.Ast
open Rcpsp_data

(* I. Utility functions to create the model of the RCPSP. *)

let for_all_pairs jobs f =
  List.flatten (List.map (fun j1 ->
    List.flatten (List.map (fun j2 ->
      f j1 j2
    ) jobs)
  ) jobs)

let for_all_distinct_pairs jobs f =
  for_all_pairs jobs (fun j1 j2 ->
    if j1.job_index <> j2.job_index then
      [f j1 j2]
    else
      [])

(* II. Creation of the variables for the RCPSP. *)

(* Name factory of the variables. *)
let start_job_name i = "start_j" ^ (string_of_int i)
let start_job i = Var (start_job_name i)
let start_job' job = Var (start_job_name job.job_index)

let constant_of i = Cst (Bound_rat.of_int i, Int)
let duration_of job = constant_of job.duration
let mduration_of job = constant_of (-job.duration)

(* NOTE: this makespan is only correct with a dummy sink job. *)
let makespan_name project = start_job_name project.jobs_number

(* These variables are generated for the "task decomposition" of the cumulative constraint.
   We have `job_1_runs_when_2_starts = 1` if the job `1` starts when the job `2` is running.
   Importantly: these variables are shared across all cumulatives. *)
let job_start_when_name j1 j2 =
  "job_" ^ (string_of_int j1.job_index) ^ "_runs_when_" ^ (string_of_int j2.job_index) ^ "_starts"
let job_start_when j1 j2 = Var (job_start_when_name j1 j2)

(* The octagonal variables are the starting dates of the jobs. *)
let octagonal_variables project =
  let name_of job = start_job_name job.job_index in
  List.map name_of project.jobs

(* Create boolean variables modelling the overlapping of two tasks. *)
let overlap_boolean_variables project = for_all_distinct_pairs project.jobs job_start_when_name

(* III. Constraints of the RCPSP. *)

(* Domain of the variables *)

let var_domain_constraints project =
  let dom u v = [
    (Var v, GEQ, Cst (Bound_rat.zero, Int));
    (Var v, LEQ, Cst (Bound_rat.of_int u, Int))
  ] in
  let ov = octagonal_variables project in
  List.flatten (
    [(dom 0 (List.hd ov))]@
    (List.map (dom project.horizon) (List.tl ov))@
    (List.map (dom 1) (overlap_boolean_variables project))@
    [(dom project.horizon (makespan_name project))])

(* Generalized temporal constraints: ensure a precedence (with a possible timelag) between the tasks. *)
let temporal_constraints project =
  (* s1 + d1 <= s2 rewritten to s1 - s2 <= -d1 *)
  let precedence_constraint (prec:precedence) (j,w) =
    let i = prec.job_index in
    let m_weight_i = constant_of (-w) in
    (Binary (start_job i, SUB, start_job j), LEQ, m_weight_i) in
  let all_successors (precedence:precedence) =
    List.map (precedence_constraint precedence)
      (List.map2 (fun x y -> (x,y)) precedence.job_successors precedence.weights) in
  List.flatten (List.map all_successors project.precedence_relations)

(* job_1_runs_when_2_starts = 1 <=> s[1] <= s[2] /\ s[2] < s[1] + d[1] *)
let overlap_reified_constraints project =
  for_all_distinct_pairs project.jobs (fun j1 j2 ->
    let c1 = (start_job' j1, LEQ, start_job' j2) in
    let c2 = (Binary (start_job' j2, SUB, start_job' j1), LT, duration_of j1) in
    (job_start_when_name j1 j2, [c1; c2]))

(* Tasks decomposition of cumulative:
      forall j1, capacity_ri >= r[j1] + sum (job_2_runs_when_1_starts * r[j2]) where j2 <> j1 *)
let cumulative_constraint project capacity ri =
  (* We retrieve the jobs that use the resource at index `ri`. *)
  let jobs = List.filter (fun job -> (List.nth job.resources_usage ri) > 0) project.jobs in
  List.map (fun j1 ->
    let resources_j2 = List.flatten (List.map (fun j2 ->
      if j1.job_index <> j2.job_index then
        [Binary (job_start_when j2 j1, MUL, constant_of (List.nth j2.resources_usage ri))]
      else
        []
    ) jobs)
    in
    let sum_resource_j2 = List.fold_left (fun sum mul -> Binary (sum, ADD, mul))
      (constant_of 0) resources_j2 in
    let capacity = constant_of capacity in
    let resource_j1 = constant_of (List.nth j1.resources_usage ri) in
    (capacity, GEQ, Binary (resource_j1, ADD, sum_resource_j2))
  ) jobs

let all_cumulatives rcpsp project =
  List.flatten (List.mapi (
    fun ri capi ->
      let capacity = List.nth rcpsp.resources_capacities capi in
      cumulative_constraint project capacity ri)
    project.resources_idx)

(* The octagonal variables are the starting dates.
   The box variables are the boolean overlap variables. *)
type rcpsp_model = {
  makespan: var;
  box_vars: var list;
  octagonal_vars: var list;
  constraints: bconstraint list;
  reified_bconstraints: (var * bconstraint list) list;
}

let create_rcpsp rcpsp =
  let project = List.hd rcpsp.projects in
  {
    makespan=(makespan_name project);
    box_vars=overlap_boolean_variables project;
    octagonal_vars=octagonal_variables project;
    constraints=(var_domain_constraints project)@(all_cumulatives rcpsp project)@(temporal_constraints project);
    reified_bconstraints=(overlap_reified_constraints project);
  }
