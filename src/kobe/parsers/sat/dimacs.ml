(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang
open Lang.Ast
open Minisatml

(** The logical formula is directly created.
    Variables are named from `b0` to `bN`. *)
let var_name i = "b" ^ (string_of_int i)

let make_literal (i, sign) =
  let var = FVar (var_name (i-1)) in
  if sign then Not var else var

let quantify_bool formula v =
  Exists(v, Abstract Bool, formula)

let read_dimacs_file problem_path =
  let (clauses, _, _) = Parser.parse problem_path in
  let formula = Rewritting.conjunction (List.map (fun clause ->
    Rewritting.disjunction (List.map make_literal clause)
  ) clauses) in
  let vars = Rewritting.Variables.elements
    (Rewritting.get_vars_set_formula formula) in
  let qf = List.fold_left quantify_bool
    (QFFormula formula) vars in
  {qf; optimise=Satisfy}
