(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Bounds
open Lang.Ast
open Domains.Abstract_domain
open Logic_completion
open Direct_product
open Event_loop
open Box
open Octagon
open Sat
open Kobecore.System
open Kobecore.Bench_instance_j
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

module SAT =
struct
  module A = Sat
  let init () : A.t = Sat.empty 0
end

module BoxIntLogic(SPLIT: Box_split.Box_split_sig) =
struct
  module Box = Box_base(SPLIT)(Bound_int)
  module L = Logic_completion(Box)
  module E = Event_loop(Event_cons(Box)(Event_atom(L)))

  module A = Direct_product(
    Prod_cons(Box)(
    Prod_cons(L)(
    Prod_atom(E))))

  let init () : A.t =
    let box = ref (Box.empty 1) in
    let lc = ref (L.init L.I.({uid=2; a=box})) in
    let event = ref (E.init 3 (box, lc)) in
    A.init 0 (box, (lc, event))
end

module BoxOctLogic(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Box = Box_base(Box_split.First_fail_bisect)(Bound_int)
  module Octagon = OctagonZ(SPLIT)
  module BoxOct = Direct_product(Prod_cons(Octagon)(Prod_atom(Box)))
  module L = Logic_completion(BoxOct)
  module E = Event_loop(Event_cons(Box)(Event_atom(L)))

  module A = Direct_product(
    Prod_cons(Octagon)(
    Prod_cons(Box)(
    Prod_cons(BoxOct)(
    Prod_cons(L)(
    Prod_atom(E))))))

  let init () : A.t =
    let octagon = ref (Octagon.empty 1) in
    let box = ref (Box.empty 2) in
    let box_oct = ref (BoxOct.init 3 (octagon, box)) in
    let lc = ref (L.init {uid=4;a=box_oct}) in
    let event = ref (E.init 5 (box, lc)) in
    A.init 0 (octagon, (box, (box_oct, (lc, event))))
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

  let debug = false

  let debug make_msg =
    if debug then
      let _ = Printf.printf "%s\n" (make_msg ()); flush_all () in
      ()
    else ()

  let count tf =
    let open Typing.Tast in
    let rec faux (_,f) =
      match f with
      | TFVar _ -> (0,1)
      | TCmp _ -> (0,1)
      | TAnd (tf1, tf2) ->
          let (vn,cn),(vn',cn') = faux tf1, faux tf2 in
          (vn+vn', cn+cn')
      | _ -> (0,1) in
    let rec aux = function
      | TQFFormula tf -> faux tf
      | TExists(_,tqf) ->
          let (vn,cn) = aux tqf in
          (vn+1, cn)
    in aux tf

  let bench_instance bench bf problem_path =
    try
      (* Format.printf "%a\n" Lang.Pretty_print.print_qformula bf.qf; *)
      let a = MA.init () in
      let adty = match MA.A.type_of a with
        | Some adty -> adty
        | None -> raise (Wrong_modelling "The given abstract domain does not have a type.") in
      let tf = Typing.Infer.infer_type adty bf.qf in
      debug (fun () -> "Successfully typed the formula.");
      debug (fun () ->
        let vn, cn = count tf in
        let vn, cn = string_of_int vn, string_of_int cn in
        "There are" ^ vn ^ " variables and " ^ cn ^ " constraints (shared by AND connector).");
      let a, cs = A.interpret a Exact tf in
      debug (fun () -> "Successfully interpreted the formula.");
      let a = List.fold_left A.weak_incremental_closure a cs in
      debug (fun () -> "Successfully added constraints.");
      (* Format.printf "AD: \n %a\n" A.print a; *)
      let solver = make_solver_kind a bf in
      let timeout = T.Timeout(timeout_of_bench bench) in
      let transformer = T.init a [timeout; solver] in
      let (gs,_) =
        try Solver.solve transformer
        with Solver.T.StopSearch t -> t in
      Measure.init gs.stats problem_path
      |> update_time bench gs.stats
      |> update_result gs bf.optimise
      |> Measure.guess_missing_measures
      |> Csv_printer.print_as_csv bench
    with
    | Bot.Bot_found ->
        Csv_printer.print_as_csv bench (Measure.root_unsat problem_path)
    | Lang.Ast.Wrong_modelling msg ->
        Csv_printer.print_exception problem_path ("\n" ^ msg)
    | e -> begin
        Printexc.print_backtrace stdout;
        Csv_printer.print_exception problem_path (Printexc.to_string e)
    end

  let bench_problem_instance bench solver problem_path =
    match Parsers.Dispatch.dispatch problem_path with
    | IGNORE -> ()
    | WARNING msg -> print_warning msg
    | PROBLEM problem ->
        let bf = Models.formula_of_problem solver.decompositions problem in
        bench_instance bench bf problem_path

  let bench bench solver =
    Csv_printer.print_csv_header bench;
    let problems = list_of_problems bench in
    List.iter (bench_problem_instance bench solver) problems
end

let make_octagon_strategy : string -> (module Octagon_split.Octagon_split_sig) = function
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

let make_box_strategy : string -> (module Box_split.Box_split_sig) = function
| "First_fail_LB"  -> (module Box_split.First_fail_LB)
| "MSLF_simple" -> (module Box_split.MSLF_simple)
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for Box. Please look into `make_box_strategy` for a list of the supported strategies.")

let check_sat_strategy : string -> unit = function
| "VSIDS"  -> ()
| s -> eprintf_and_exit ("The AbSolute strategy `" ^ s ^ "` is unknown for SAT. Please look into `check_sat_strategy` for a list of the supported strategies.")

let bench_absolute bench solver =
  match solver.domain with
   | "SAT" ->
      check_sat_strategy solver.strategy;
      let (module B: Bencher_sig) = (module Bencher(SAT)) in
      B.bench bench solver
  | "Octagon" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module BoxOctLogic(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "Box" ->
      let (module S: Box_split.Box_split_sig) = make_box_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module BoxIntLogic(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | d -> eprintf_and_exit ("The AbSolute domain `" ^ d ^ "` is unknown. Please look into `Absolute_bench.bench_absolute` for a list of the supported domains.")
