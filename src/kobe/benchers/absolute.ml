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
open Domains.Abstract_domain
open Logic_product
open Ordered_product
open Event_loop
open Box
open Kobecore.System
open Kobecore.Bench_instance_j
open Models.Bab_qformula
open Parsers_scheduling
open Measure

module type Bencher_sig =
sig
  val bench: bench_instance -> absolute_solver -> unit
end

module type Make_AD_sig =
sig
  module A: Abstract_domain
  val init: unit -> A.t
end

module BoxIntLogic(SPLIT: Box_split.Box_split_sig) =
struct
  module Box = Box_base(SPLIT)(Bound_int)
  module L = Logic_product(LProd_atom(Box))
  module E = Event_loop(Event_cons(Box)(Event_atom(L)))

  module A = Ordered_product(
    Prod_cons(Box)(
    Prod_cons(L)(
    Prod_atom(E))))

  let init () : A.t =
    let new_uid =
      let uid = ref (-1) in
      (fun () -> uid := !uid + 1; !uid) in
    let box = ref (Box.empty (new_uid ())) in
    let logic = ref (L.init (new_uid ()) box) in
    let event = ref (E.init (new_uid ()) (box, logic)) in
    A.init (new_uid ()) (box, (logic, event))
end

module Bencher(MA: Make_AD_sig): Bencher_sig =
struct
  module A = MA.A
  module Solver = Fixpoint.Solver.Make(A)
  module T = Fixpoint.Transformer.Make(A)

  let make_solver_kind a bf =
    let make_bab v kind =
      let (v_id,_) = A.I.to_abstract_var (A.interpretation a) v in
      T.BAB T.{ kind; objective=(v_id, v); best=None}
    in
    match bf.optimise with
    | Minimize(v) -> make_bab v LT
    | Maximize(v) -> make_bab v GT
    | Satisfy -> T.BoundSolutions 1

  let update_result gs kind measure =
    match kind with
    | Minimize _ | Maximize _ ->
        let best = T.unwrap_best gs in
        { measure with optimum=best }
    | Satisfy -> measure (* `gs.stats` already contains the solutions count. *)

  let bench_instance bench bf problem_path =
    try
      (* Format.printf "%a\n" Lang.Pretty_print.print_qformula bf.qf; *)
      let a = MA.init () in
      let a = A.qinterpret a Exact bf.qf in
      (* let _ = Printf.printf "Successful interpreting of the formula.\n"; flush_all () in *)
      let solver = make_solver_kind a bf in
      let timeout = T.Timeout(timeout_of_bench bench) in
      let transformer = T.init a [timeout; solver] in
      let (gs, _) =
        try Solver.solve transformer
        with Solver.T.StopSearch t -> t in
      Measure.init gs.stats problem_path
      |> update_time bench gs.stats
      |> update_result gs bf.optimise
      |> Measure.guess_missing_measures
      |> Csv_printer.print_as_csv bench
    with Lang.Ast.Wrong_modelling msg ->
      Csv_printer.print_exception problem_path ("\n" ^ msg)
    | e -> begin
      Printexc.print_backtrace stdout;
      Csv_printer.print_exception problem_path (Printexc.to_string e)
    end

  let bench_rcpsp_instance bench solver problem_path =
    let rcpsp = Scheduling.read_rcpsp problem_path in
    if (List.length rcpsp.projects) > 1 then
      eprintf_and_exit "AbSolute model for multi-project RCPSP is not yet supported.";
    let bf = Models.Rcpsp.formula_of_rcpsp rcpsp solver.decompositions in
    bench_instance bench bf problem_path

  let bench bench solver =
    Csv_printer.print_csv_header bench;
    let problems = list_of_problems bench in
    List.iter (bench_rcpsp_instance bench solver) problems
end

(* let make_octagon_strategy : string -> (module Octagon_split.Octagon_split_sig) = function
| "MSLF" -> (module Octagon_split.MSLF)
| "MSLF_all" -> (module Octagon_split.MSLF_all)
| "MSLF_simple" -> (module Octagon_split.MSLF_simple)
| "Max_min_LB" -> (module Octagon_split.Max_min_LB)
| "Min_max_LB" -> (module Octagon_split.Min_max_LB)
| "Max_min_Bisect" -> (module Octagon_split.Max_min_Bisect)
| "Min_max_Bisect" -> (module Octagon_split.Min_max_Bisect)
| "Anti_first_fail_LB" -> (module Octagon_split.Anti_first_fail_LB)
| "Anti_first_fail_Bisect" -> (module Octagon_split.Anti_first_fail_Bisect)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Octagon. Please look into `make_octagon_strategy` for a list of the supported strategies.")
 *)

let make_box_strategy : string -> (module Box_split.Box_split_sig) = function
| "First_fail_LB"  -> (module Box_split.First_fail_LB)
| "MSLF_simple" -> (module Box_split.MSLF_simple)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Box. Please look into `make_box_strategy` for a list of the supported strategies.")

let bench_absolute bench solver =
  match solver.domain with
(*
  | "Octagon" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: RCPSP_sig) = (module RCPSP_Octagon(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.start_benchmarking bench *)
  | "Box" ->
      let (module S: Box_split.Box_split_sig) = make_box_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module BoxIntLogic(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | d -> eprintf_and_exit ("The AbSolute domain `" ^ d ^ "` is unknown. Please look into `Absolute_bench.bench_absolute` for a list of the supported domains.")
