(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang.Pretty_print
open Lang.Ast
open Core.Types
open Kobecore
open Kobecore.System

let string_of_list l =
  List.fold_left (fun s e -> s ^ e ^ ", ") "" l

let unsupported_type ty =
  eprintf_and_exit ("The type of variable `" ^ (string_of_ty ty) ^ "` is not supported by MiniZinc (or we did not implement the conversion yet.")

let mzn_constraint x = "constraint " ^ x ^ ";\n"

let mzn_of_formula f =
  let rec aux t = function
  | And (f1, f2) when t -> (aux true f1) ^ (aux true f2)
  | x ->
      let x = match x with
        | FVar v -> v
        | Cmp c -> string_of_constraint c
        | And (f1, f2) -> binary_connector "/\\" f1 f2
        | Equiv (f1, f2) -> binary_connector "<->" f1 f2
        | Imply (f1, f2) -> binary_connector "->" f1 f2
        | Or (f1, f2) -> binary_connector "\\/" f1 f2
        | Not f1 -> "(not " ^ (aux false f1) ^ ")"
      in if t then mzn_constraint x else x
  and binary_connector connector f1 f2 =
    "(" ^ (aux false f1) ^ connector ^ (aux false f2) ^ ")"
  in aux true f

let make_search_annot name vars strategy =
  if List.length vars > 0 then
    Printf.sprintf "%s_search([%s], %s)"
      name (string_of_list vars) strategy.Bench_desc_j.plain
  else ""

(* First split on integers, Boolean and finally float variables. *)
let mzn_search_annot vars strategy =
  let split_on_ty (iv,bv,fv) (v,ty) =
    match ty with
    | Concrete Int | Abstract Integer -> (v::iv, bv, fv)
    | Abstract Bool -> (iv, v::bv, fv)
    | Concrete Real | Abstract Float -> (iv, bv, v::fv)
    | _ -> unsupported_type ty
  in
  let iv, bv, fv = List.fold_left split_on_ty ([],[],[]) vars in
  let iv, bv, fv = (List.rev iv, List.rev bv, List.rev fv) in
  let annots = [(make_search_annot "int" iv strategy);
    (make_search_annot "bool" bv strategy);
    (make_search_annot "float" fv strategy)] in
  let annots = List.filter (fun a -> String.length a > 0) annots in
  Printf.sprintf "solve::seq_search([%s])" (string_of_list annots)

let mzn_of_type = function
  | Concrete Int | Abstract Integer -> "int"
  | Abstract Bool -> "bool"
  | Concrete Real | Abstract Float -> "float"
  | ty -> unsupported_type ty

(* Do not close with `;` the `solve` item yet. *)
let mzn_of_qformula' qf strategy =
  let rec aux vars = function
  | QFFormula f ->
      "\n" ^
      (mzn_of_formula f) ^ "\n" ^
      (mzn_search_annot (List.rev vars) strategy)
  | Exists (v, ty, qf) ->
      "var " ^ (mzn_of_type ty) ^ ": " ^ v ^ ";\n" ^ (aux ((v,ty)::vars) qf)
  in aux [] qf

let mzn_of_qformula qf strategy =
  (mzn_of_qformula' qf strategy) ^ ";"

let mzn_bab_output x =
  (Printf.sprintf "output [\"objective = \", show(%s), \"\\n\"];\n" x)

let mzn_of_bab_qformula bab_formula strategy =
  mzn_of_qformula' bab_formula.qf strategy ^
  (match bab_formula.optimise with
  | Minimize x -> "minimize " ^ x ^ ";\n" ^ (mzn_bab_output x)
  | Maximize x -> "maximize " ^ x ^ ";\n" ^ (mzn_bab_output x)
  | Satisfy -> "satisfy;")
