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
open Lang
open Lang.Ast
open Typing
open Typing.Tast
open Domains.Abstract_domain
open Logic_completion
open Propagator_completion
open Cascade_product
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
  val type_formula: Ad_type.ad_ty -> qformula -> tqformula
  val search_strategy: tqformula -> Domains.Abstract_domain.search_strategy
end

module SAT =
struct
  module A = Sat
  let init () : A.t = Sat.empty 0
  let type_formula = Typing.Infer.infer_type
  let search_strategy _ = Simple
end

module LC_Box(SPLIT: Box_split.Box_split_sig) =
struct
  module Box = Box_base(SPLIT)(Bound_int)
  module PC = Propagator_completion(Box.Vardom)(Box)
  module L = Logic_completion(PC)
  module E = Event_loop(Event_cons(PC)(Event_atom(L)))

  module A = Direct_product(
    Prod_cons(Box)(
    Prod_cons(PC)(
    Prod_cons(L)(
    Prod_atom(E)))))

  let box_uid = 1
  let pc_uid = 2
  let lc_uid = 3

  let init () : A.t =
    let box = ref (Box.empty box_uid) in
    let pc = ref (PC.init PC.I.{uid=pc_uid; a=box}) in
    let lc = ref (L.init L.I.({uid=lc_uid; a=pc})) in
    let event = ref (E.init 4 (pc, lc)) in
    A.init 0 (Owned box, (Owned pc, (Owned lc, Owned event)))

  let type_formula adty tf =
    let tf = Typing.Infer.infer_type adty tf in
    let rec aux (u, f) =
      let uid = if u = box_uid then pc_uid else u in
      match f with
      | TCmp c -> uid, TCmp c
      | TFVar v -> uid, TFVar v
      | TEquiv (tf1,tf2) -> uid, TEquiv (aux tf1, aux tf2)
      | TImply (tf1,tf2) -> uid, TImply (aux tf1, aux tf2)
      | TAnd (tf1,tf2) -> uid, TAnd (aux tf1, aux tf2)
      | TOr (tf1,tf2) -> uid, TOr (aux tf1, aux tf2)
      | TNot tf -> uid, TNot (aux tf)
    in map_tformula aux tf

  let search_strategy _ = Sequence([
    (lc_uid, Simple);
    (box_uid, Simple)])
end

module BoxOctLogic(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Box = Box_base(Box_split.First_fail_bisect)(Bound_int)
  module PC = Propagator_completion(Box.Vardom)(Box)
  module Octagon = OctagonZ(SPLIT)
  module BoxOct = Direct_product(
    Prod_cons(Octagon)(
    Prod_cons(Box)(
    Prod_atom(PC))))
  module L = Logic_completion(BoxOct)
  module E = Event_loop(Event_cons(PC)(Event_atom(L)))

  module A = Direct_product(
    Prod_cons(BoxOct)(
    Prod_cons(L)(
    Prod_atom(E))))

  let init () : A.t =
    let octagon = ref (Octagon.empty 1) in
    let box = ref (Box.empty 2) in
    let pc = ref (PC.init {uid=3; a=box}) in
    let box_oct = ref (BoxOct.init 4 (Owned octagon, (Owned box, Owned pc))) in
    let lc = ref (L.init {uid=5;a=box_oct}) in
    let event = ref (E.init 6 (pc, lc)) in
    A.init 0 (Owned box_oct, (Owned lc, Owned event))

  let type_formula = Typing.Infer.infer_type
  let search_strategy _ = Simple
end

module LC_Oct(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Octagon = OctagonZ(SPLIT)
  module L = Logic_completion(Octagon)
  module E = Event_loop(Event_atom(L))

  module A = Direct_product(
    Prod_cons(Octagon)(
    Prod_cons(L)(
    Prod_atom(E))))

  let oct_uid = 1
  let lc_uid = 2

  let init () : A.t =
    let octagon = ref (Octagon.empty oct_uid) in
    let lc = ref (L.init {uid=lc_uid;a=octagon}) in
    let event = ref (E.init 3 lc) in
    A.init 0 (Owned octagon, (Owned lc, Owned event))

  let type_formula = Typing.Infer.infer_type
  let search_strategy _ = Sequence([
    (lc_uid, Simple);
    (oct_uid, Simple)])
end

module LC_PC_BoxOct(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Octagon = OctagonZ(SPLIT)
  module Box = Box_base(Box_split.First_fail_LB)(Bound_int)
  module BoxOct = Direct_product(Prod_cons(Box)(Prod_atom(Octagon)))
  module PC = Propagator_completion(Box.Vardom)(BoxOct)
  module LC = Logic_completion(PC)
  module E = Event_loop(Event_cons(PC)(Event_atom(LC)))

  module A = Direct_product(
    Prod_cons(BoxOct)(
    Prod_cons(PC)(
    Prod_cons(LC)(
    Prod_atom(E)))))

  let oct_uid = 1
  let box_uid = 2
  let box_oct_uid = 3
  let pc_uid = 4
  let lc_uid = 5

  let init () : A.t =
    let octagon = ref (Octagon.empty oct_uid) in
    let box = ref (Box.empty box_uid) in
    let box_oct = ref (BoxOct.init box_oct_uid (Owned box, Owned octagon)) in
    let pc = ref (PC.init {uid=pc_uid; a=box_oct}) in
    let lc = ref (LC.init {uid=lc_uid;a=pc}) in
    let event = ref (E.init 6 (pc, lc)) in
    A.init 0 (Owned box_oct, (Owned pc, (Owned lc, Owned event)))

  (* This function is just for testing purposes, but we should fix the inference engine to support multiple variables, and use that instead. *)
  let type_formula _ formula =
    let uid_of_var name =
      match name.[0] with
      | 's' -> pc_uid
      | 'd' -> box_uid
      | 'm' ->
          if name = "makespan" then box_uid
          else box_uid
      | _ -> failwith ("unknown variable " ^ name) in
    let rec classify c =
      match c with
      (* Domain of variables. *)
      | Cmp (Var n, op, e) -> (uid_of_var n, TCmp(Var n, op, e))
      (* Precedence constraints. *)
      | Cmp (Binary(Var a,b, Var c), op, Cst (d,e)) when a <> "makespan" && c <> "makespan" -> (oct_uid, TCmp(Binary(Var a,b, Var c), op, Cst (d,e)))
      | Cmp (Binary(a,b,c), op, e2) -> (pc_uid, TCmp(Binary(a,b,c), op, e2))
      (* Disjunctive machine *)
      | Imply (Cmp b, Or(Cmp c1, Cmp c2)) ->
          (lc_uid, TImply((pc_uid, TCmp b), (lc_uid, TOr((pc_uid, TCmp c1), (pc_uid, TCmp c2)))))
      | Or(Cmp c1, Cmp c2) ->
          (lc_uid, TOr((pc_uid, TCmp c1), (pc_uid, TCmp c2)))
      (* Duration assignment *)
      | Or (And (Cmp c1, Cmp c2), f) ->
          (lc_uid, TOr(classify (And(Cmp c1, Cmp c2)), classify f))
      | And(Cmp c1, Cmp c2) -> (pc_uid, TAnd((pc_uid, TCmp c1), (pc_uid, TCmp c2)))
      | f -> failwith ("unrecognized constraint " ^ (Pretty_print.string_of_formula f)) in
    let rec aux = function
      | Exists (name, ty, f) ->
          let uid = uid_of_var name in
          (* let _ = Printf.printf "Create variable %s in %d\n" name uid in *)
          TExists ({name;ty;uid}, (aux f))
      | QFFormula f ->
          let cons = Rewritting.flatten_conjunction f in
          let tcons = List.map classify cons in
          let tqcons = List.map (fun c -> TQFFormula c) tcons in
          Tast.q_conjunction 0 tqcons in
    aux formula

  let search_strategy tf =
    let rec aux = function
      | TQFFormula _ -> []
      | TExists (tv, tf) when tv.name.[0] = 'd' -> tv.name::(aux tf)
      | TExists (_, tf) -> aux tf in
   Sequence([
    (box_oct_uid, Sequence([(box_uid, VarView (aux tf))]));
    (lc_uid, Simple);
    (box_oct_uid, Sequence([
      (oct_uid, Simple);
      (box_uid, Simple)]))])
end

module LC_PC_BoxOct2(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Octagon = OctagonZ(SPLIT)
  module Box = Box_base(Box_split.First_fail_LB)(Bound_int)
  module BoxOct = Direct_product(Prod_cons(Box)(Prod_atom(Octagon)))
  module PC = Propagator_completion(Box.Vardom)(BoxOct)
  module LC = Logic_completion(PC)
  module LC_box = Logic_completion(Box)
  module E = Event_loop(
    Event_cons(PC)(
    Event_cons(LC_box)(
    Event_atom(LC))))

  module A = Direct_product(
    Prod_cons(BoxOct)(
    Prod_cons(PC)(
    Prod_cons(LC)(
    Prod_cons(LC_box)(
    Prod_atom(E))))))

  let oct_uid = 1
  let box_uid = 2
  let box_oct_uid = 3
  let pc_uid = 4
  let lc_uid = 5
  let lc_box_uid = 6
  let event_uid = 7

  let init () : A.t =
    let octagon = ref (Octagon.empty oct_uid) in
    let box = ref (Box.empty box_uid) in
    let box_oct = ref (BoxOct.init box_oct_uid (Owned box, Owned octagon)) in
    let pc = ref (PC.init {uid=pc_uid; a=box_oct}) in
    let lc = ref (LC.init {uid=lc_uid;a=pc}) in
    let lc_box = ref (LC_box.init {uid=lc_box_uid;a=box}) in
    let event = ref (E.init event_uid (pc, (lc_box, lc))) in
    A.init 0 (Owned box_oct, (Owned pc, (Owned lc, (Owned lc_box, Owned event))))

  (* This function is just for testing purposes, but we should fix the inference engine to support multiple variables, and use that instead. *)
  let type_formula _ formula =
    let uid_of_var name =
      match name.[0] with
      | 's' -> pc_uid
      | 'd' -> box_uid
      | 'm' ->
          if name = "makespan" then box_uid
          else box_uid
      | _ -> failwith ("unknown variable " ^ name) in
    let rec classify c =
      match c with
      (* Domain of variables. *)
      | Cmp (Var n, op, e) -> (uid_of_var n, TCmp(Var n, op, e))
      (* Precedence constraints. *)
      | Cmp (Binary(Var a,b, Var c), op, Cst (d,e)) when a <> "makespan" && c <> "makespan" -> (oct_uid, TCmp(Binary(Var a,b, Var c), op, Cst (d,e)))
      | Cmp (Binary(a,b,c), op, e2) -> (pc_uid, TCmp(Binary(a,b,c), op, e2))
      (* Disjunctive machine *)
      | Imply (Cmp b, Or(Cmp c1, Cmp c2)) ->
          (lc_uid, TImply((pc_uid, TCmp b), (lc_uid, TOr((pc_uid, TCmp c1), (pc_uid, TCmp c2)))))
      | Or(Cmp c1, Cmp c2) ->
          (lc_uid, TOr((pc_uid, TCmp c1), (pc_uid, TCmp c2)))
      (* Duration assignment *)
      | Or (And (Cmp c1, Cmp c2), f) ->
          (lc_box_uid, TOr(classify (And(Cmp c1, Cmp c2)), classify f))
      | And(Cmp c1, Cmp c2) -> (box_uid, TAnd((box_uid, TCmp c1), (box_uid, TCmp c2)))
      | f -> failwith ("unrecognized constraint " ^ (Pretty_print.string_of_formula f)) in
    let rec aux = function
      | Exists (name, ty, f) ->
          let uid = uid_of_var name in
          (* let _ = Printf.printf "Create variable %s in %d\n" name uid in *)
          TExists ({name;ty;uid}, (aux f))
      | QFFormula f ->
          let cons = Rewritting.flatten_conjunction f in
          let tcons = List.map classify cons in
          let tqcons = List.map (fun c -> TQFFormula c) tcons in
          Tast.q_conjunction 0 tqcons in
    aux formula

  let search_strategy tf =
    let rec aux = function
      | TQFFormula _ -> []
      | TExists (tv, tf) when tv.name.[0] = 'd' -> tv.name::(aux tf)
      | TExists (_, tf) -> aux tf in
   Sequence([
    (box_oct_uid, Sequence([(box_uid, VarView (aux tf))]));
    (lc_uid, Simple);
    (box_oct_uid, Sequence([
      (oct_uid, Simple);
      (box_uid, Simple)]))])
(*   Sequence([
    (box_oct_uid, Sequence([
      (box_uid, VarView (aux tf));
      (oct_uid, Simple)]));
    (lc_uid, Simple);
    (lc_box_uid, Simple);
    (box_oct_uid, Sequence([
      (box_uid, Simple)]))]) *)
end

module Cascade_BoxOct(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Octagon = OctagonZ(SPLIT)
  module Box = Box_base(Box_split.First_fail_LB)(Bound_int)
  module BoxOct = Direct_product(Prod_cons(Box)(Prod_atom(Octagon)))
  module PC = Propagator_completion(Box.Vardom)(BoxOct)
  module Cascade = Cascade_product(PC)(Octagon)
  module PC_Cascade = Direct_product(Prod_cons(PC)(Prod_atom(Cascade)))
  module LC = Logic_completion(PC_Cascade)
  module E = Event_loop(
    Event_cons(PC)(
    Event_cons(Cascade)(
    Event_atom(LC))))

  module A = Direct_product(
    Prod_cons(BoxOct)(
    Prod_cons(PC)(
    Prod_cons(Cascade)(
    Prod_cons(LC)(
    Prod_atom(E))))))

  let oct_uid = 1
  let box_uid = 2
  let box_oct_uid = 3
  let pc_uid = 4
  let cascade_uid = 5
  let lc_uid = 6
  let event_uid = 7
  let pc_cascade_uid = 8

  let init () : A.t =
    let octagon = ref (Octagon.empty oct_uid) in
    let box = ref (Box.empty box_uid) in
    let box_oct = ref (BoxOct.init box_oct_uid (Owned box, Owned octagon)) in
    let pc = ref (PC.init {uid=pc_uid; a=box_oct}) in
    let cascade = ref (Cascade.init {uid=cascade_uid; a=pc; b=octagon}) in
    let pc_cascade = ref (PC_Cascade.init pc_cascade_uid (Shared pc, Shared cascade)) in
    let lc = ref (LC.init {uid=lc_uid;a=pc_cascade}) in
    let event = ref (E.init event_uid (pc, (cascade, lc))) in
    A.init 0 (Owned box_oct, (Owned pc, (Owned cascade, (Owned lc, Owned event))))

  (* This function is just for testing purposes, but we should fix the inference engine to support multiple variables, and use that instead. *)
  let type_formula _ formula =
    let uid_of_var name =
      match name.[0] with
      | 's' -> pc_uid
      | 'd' -> box_uid
      | 'm' ->
          if name = "makespan" then pc_uid
          else box_uid
      | _ -> failwith ("unknown variable " ^ name) in
    let rec classify c =
      match c with
      (* Domain of variables. *)
      | Cmp (Var n, op, e) -> (uid_of_var n, TCmp(Var n, op, e))
      (* Precedence constraints. *)
      | Cmp (Binary(Var a, b, Var c), op, Cst (d,e)) -> (oct_uid, TCmp(Binary(Var a, b, Var c), op, Cst (d,e)))
      | Cmp (Binary(a,b,c), op, e2) -> (cascade_uid, TCmp(Binary(a,b,c), op, e2))
      (* Disjunctive machine *)
      | Imply (Cmp b, Or(Cmp c1, Cmp c2)) ->
          (lc_uid, TImply((pc_uid, TCmp b), (lc_uid, TOr((cascade_uid, TCmp c1), (cascade_uid, TCmp c2)))))
      | Or(Cmp c1, Cmp c2) ->
          (lc_uid, TOr((cascade_uid, TCmp c1), (cascade_uid, TCmp c2)))
      (* Duration assignment *)
      | Or (And (Cmp c1, Cmp c2), f) ->
          (lc_uid, TOr(classify (And(Cmp c1, Cmp c2)), classify f))
      | And(Cmp c1, Cmp c2) -> (pc_uid, TAnd((pc_uid, TCmp c1), (pc_uid, TCmp c2)))
      | f -> failwith ("unrecognized constraint " ^ (Pretty_print.string_of_formula f)) in
    let rec aux = function
      | Exists (name, ty, f) ->
          let uid = uid_of_var name in
          (* let _ = Printf.printf "Create variable %s in %d\n" name uid in *)
          TExists ({name;ty;uid}, (aux f))
      | QFFormula f ->
          let cons = Rewritting.flatten_conjunction f in
          let tcons = List.map classify cons in
          let tqcons = List.map (fun c -> TQFFormula c) tcons in
          Tast.q_conjunction 0 tqcons in
    aux formula

  let search_strategy tf =
    let rec aux l = function
      | TQFFormula _ -> []
      | TExists (tv, tf) when tv.name.[0] = l -> tv.name::(aux l tf)
      | TExists (_, tf) -> aux l tf in
   Sequence([
    (box_oct_uid, Sequence([(box_uid, VarView (aux 'd' tf))]));
    (lc_uid, Simple);
    (box_oct_uid, Sequence([(oct_uid, Simple)]));
    (box_oct_uid, Sequence([
      (box_uid, Simple)]))])
end

module Cascade_BoxOct2(SPLIT: Octagon_split.Octagon_split_sig) =
struct
  module Octagon = OctagonZ(SPLIT)
  module Box = Box_base(Box_split.First_fail_LB)(Bound_int)
  module BoxOct = Direct_product(Prod_cons(Box)(Prod_atom(Octagon)))
  module PC = Propagator_completion(Box.Vardom)(BoxOct)
  module Cascade = Cascade_product(PC)(Octagon)
  module PC_Cascade = Direct_product(Prod_cons(PC)(Prod_atom(Cascade)))
  module LC = Logic_completion(PC_Cascade)
  module LC_box = Logic_completion(Box)
  module E = Event_loop(
    Event_cons(PC)(
    Event_cons(Cascade)(
    Event_cons(LC_box)(
    Event_atom(LC)))))

  module A = Direct_product(
    Prod_cons(BoxOct)(
    Prod_cons(PC)(
    Prod_cons(Cascade)(
    Prod_cons(LC)(
    Prod_cons(LC_box)(
    Prod_atom(E)))))))

  let oct_uid = 1
  let box_uid = 2
  let box_oct_uid = 3
  let pc_uid = 4
  let cascade_uid = 5
  let lc_uid = 6
  let lc_box_uid = 7
  let event_uid = 8
  let pc_cascade_uid = 9

  let init () : A.t =
    let octagon = ref (Octagon.empty oct_uid) in
    let box = ref (Box.empty box_uid) in
    let box_oct = ref (BoxOct.init box_oct_uid (Owned box, Owned octagon)) in
    let pc = ref (PC.init {uid=pc_uid; a=box_oct}) in
    let cascade = ref (Cascade.init {uid=cascade_uid; a=pc; b=octagon}) in
    let pc_cascade = ref (PC_Cascade.init pc_cascade_uid (Shared pc, Shared cascade)) in
    let lc = ref (LC.init {uid=lc_uid;a=pc_cascade}) in
    let lc_box = ref (LC_box.init {uid=lc_box_uid;a=box}) in
    let event = ref (E.init event_uid (pc, (cascade, (lc_box, lc)))) in
    A.init 0 (Owned box_oct, (Owned pc, (Owned cascade, (Owned lc, (Owned lc_box, Owned event)))))

  (* This function is just for testing purposes, but we should fix the inference engine to support multiple variables, and use that instead. *)
  let type_formula _ formula =
    let uid_of_var name =
      match name.[0] with
      | 's' -> pc_uid
      | 'd' -> box_uid
      | 'm' ->
          if name = "makespan" then pc_uid
          else box_uid
      | _ -> failwith ("unknown variable " ^ name) in
    let rec classify c =
      match c with
      (* Domain of variables. *)
      | Cmp (Var n, op, e) -> (uid_of_var n, TCmp(Var n, op, e))
      (* Precedence constraints. *)
      | Cmp (Binary(Var a, b, Var c), op, Cst (d,e)) -> (oct_uid, TCmp(Binary(Var a, b, Var c), op, Cst (d,e)))
      | Cmp (Binary(a,b,c), op, e2) -> (cascade_uid, TCmp(Binary(a,b,c), op, e2))
      (* Disjunctive machine *)
      | Imply (Cmp b, Or(Cmp c1, Cmp c2)) ->
          (lc_uid, TImply((pc_uid, TCmp b), (lc_uid, TOr((cascade_uid, TCmp c1), (cascade_uid, TCmp c2)))))
      | Or(Cmp c1, Cmp c2) ->
          (lc_uid, TOr((cascade_uid, TCmp c1), (cascade_uid, TCmp c2)))
      (* Duration assignment *)
      | Or (And (Cmp c1, Cmp c2), f) ->
          (lc_box_uid, TOr(classify (And(Cmp c1, Cmp c2)), classify f))
      | And(Cmp c1, Cmp c2) -> (box_uid, TAnd((box_uid, TCmp c1), (box_uid, TCmp c2)))
      | f -> failwith ("unrecognized constraint " ^ (Pretty_print.string_of_formula f)) in
    let rec aux = function
      | Exists (name, ty, f) ->
          let uid = uid_of_var name in
          (* let _ = Printf.printf "Create variable %s in %d\n" name uid in *)
          TExists ({name;ty;uid}, (aux f))
      | QFFormula f ->
          let cons = Rewritting.flatten_conjunction f in
          let tcons = List.map classify cons in
          let tqcons = List.map (fun c -> TQFFormula c) tcons in
          Tast.q_conjunction 0 tqcons in
    aux formula

  let search_strategy tf =
    let rec aux l = function
      | TQFFormula _ -> []
      | TExists (tv, tf) when tv.name.[0] = l -> tv.name::(aux l tf)
      | TExists (_, tf) -> aux l tf in
   Sequence([
    (box_oct_uid, Sequence([(box_uid, VarView (aux 'd' tf))]));
    (lc_uid, Simple);
    (box_oct_uid, Sequence([(oct_uid, Simple)]));
    (box_oct_uid, Sequence([
      (box_uid, Simple)]));
    (lc_box_uid, Simple)])
  (* Sequence([
    (box_oct_uid, Sequence([
      (box_uid, VarView (aux 'd' tf));
      (oct_uid, Simple)]));
    (lc_uid, Simple);
    (lc_box_uid, Simple);
    (box_oct_uid, Sequence([
      (box_uid, Simple)]))]) *)
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

  (* let print_node _ _ _ = () *)
(*   let print_node status _ node =
    Format.printf "[%s]%a\n" status MA.A.print node;
    flush_all ();
    ignore(Scanf.scanf "%c" (fun x -> x)) *)

  (* let print_sol _ = () *)
(*     Format.printf ">> [Solution]\n\n\n\n" *)

  let bench_instance bench bf problem_path =
    try
      let a = MA.init () in
      let adty = match MA.A.type_of a with
        | Some adty -> adty
        | None -> raise (Wrong_modelling "The given abstract domain does not have a type.") in
      let tf = MA.type_formula adty bf.qf in
      let strategy = MA.search_strategy tf in
      (* Printf.printf "%s\n" (Tast.string_of_tqformula adty tf); *)
      debug (fun () -> "Successfully typed the formula.");
      debug (fun () ->
        let vn, cn = count tf in
        let vn, cn = string_of_int vn, string_of_int cn in
        "There are" ^ vn ^ " variables and " ^ cn ^ " constraints (shared by AND connector).");
      let a, cs = A.interpret a Exact tf in
      (* Format.printf "AD: \n %a\n" A.print a; *)
      debug (fun () -> "Successfully interpreted the formula.");
(*       let a = List.fold_left (fun a c ->
        try A.weak_incremental_closure a c
        with Bot.Bot_found ->
          Format.printf "AD: \n %a\n" A.print a;
          Format.printf "Culprit: %a" Pretty_print.print_qformula (tqformula_to_qformula (A.I.to_qformula (A.interpretation a) [c])); raise Bot.Bot_found) a cs in *)
      let a = List.fold_left A.weak_incremental_closure a cs in
      debug (fun () -> "Successfully added constraints.");
      (* Format.printf "AD: \n %a\n" A.print a; *)
      let solver = make_solver_kind a bf in
      let timeout = T.Timeout(timeout_of_bench bench) in
      (* let printer = T.Printer T.{
        print_node;
        print_sol } in *)
      let transformer = T.init a [timeout; solver(* ; printer *)] in
      let (gs,_) =
        try Solver.solve ~strategy transformer
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
| "MSLF_simple_rotated" -> (module Octagon_split.MSLF_simple_rotated)
| "MSLF_rotated" -> (module Octagon_split.MSLF_rotated)
| "MSLF_UB" -> (module Octagon_split.MSLF_UB)
| "MSLF_UB_all" -> (module Octagon_split.MSLF_UB_all)
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
  | "LC-Oct" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module LC_Oct(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "LC-PC-BoxOct" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module LC_PC_BoxOct(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "LC-PC-BoxOct2" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module LC_PC_BoxOct2(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "Cascade_BoxOct" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module Cascade_BoxOct(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "Cascade_BoxOct2" ->
      let (module S: Octagon_split.Octagon_split_sig) = make_octagon_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module Cascade_BoxOct2(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | "LC-Box" | "BoxIntLogic" ->
      let (module S: Box_split.Box_split_sig) = make_box_strategy solver.strategy in
      let (module M: Make_AD_sig) = (module LC_Box(S)) in
      let (module B: Bencher_sig) = (module Bencher(M)) in
      B.bench bench solver
  | d -> eprintf_and_exit ("The AbSolute domain `" ^ d ^ "` is unknown. Please look into `Absolute_bench.bench_absolute` for a list of the supported domains.")
