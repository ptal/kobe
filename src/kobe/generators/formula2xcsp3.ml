(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Lang.Ast
open Core.Types
open Kobecore.System

let unsupported_type ty =
  eprintf_and_exit ("The type of variable `" ^ (string_of_ty ty) ^ "` is not supported by our XCSP3 conversion tool.")

let unsupported_funcall f =
  eprintf_and_exit ("The function call `" ^ f ^ "` is not supported by our XCSP3 conversion tool.")


let xcsp3_of_binop = function
  | ADD -> "add"
  | SUB -> "sub"
  | MUL -> "mul"
  | DIV -> "div"
  | POW -> "pow"

let rec xcsp3_of_expr = function
  | Var v -> v
  | Cst (i, ty) ->
      (match ty with
      | Int -> Bound_rat.to_string i
      | _ -> unsupported_type (Concrete ty))
  | Unary (NEG, expr) -> "neg(" ^ (xcsp3_of_expr expr) ^ ")"
  | Binary (e1, op, e2) -> (xcsp3_of_binop op) ^ "(" ^ (xcsp3_of_expr e1)  ^ "," ^ (xcsp3_of_expr e2)  ^ ")"
  | Funcall (f, _) -> unsupported_funcall f

let xcsp3_of_cmpop = function
  | EQ -> "eq"
  | LEQ -> "le"
  | GEQ -> "ge"
  | NEQ -> "ne"
  | GT -> "gt"
  | LT -> "lt"

let xcsp3_of_constraint (e1, op, e2) =
  (xcsp3_of_cmpop op) ^ "(" ^
  (xcsp3_of_expr e1) ^ "," ^ (xcsp3_of_expr e2) ^
  ")"

let xcsp3_constraint c = "<intension>" ^ c ^ "</intension>\n"

let xcsp3_of_formula f =
  let rec aux t = function
  | And (f1, f2) when t -> (aux true f1) ^ (aux true f2)
  | x ->
      let x = match x with
        | FVar v -> v
        | Cmp c -> xcsp3_of_constraint c
        | And (f1, f2) -> binary_connector "and" f1 f2
        | Equiv (f1, f2) -> binary_connector "iff" f1 f2
        | Imply (f1, f2) -> binary_connector "imp" f1 f2
        | Or (f1, f2) -> binary_connector "or" f1 f2
        | Not f1 -> "not(" ^ (aux false f1) ^ ")"
      in
        if t then xcsp3_constraint x else x
  and binary_connector connector f1 f2 =
    connector ^ "(" ^ (aux false f1) ^ "," ^ (aux false f2) ^ ")"
  in aux true f

let xcsp3_of_qformula' qf =
  let rec aux = function
  | Exists (v, ty, qf) ->
      let dom =
        match ty with
        | Concrete Int -> "0..100000000"
        | Abstract Bool -> "0..1"
        | _ -> unsupported_type ty
      in
      "<var id=\"" ^ v ^ "\">" ^ dom ^ "</var>\n" ^
      aux qf
  | QFFormula f ->
      "</variables>\n" ^
      "<constraints>\n" ^
      (xcsp3_of_formula f) ^
      "</constraints>\n"
  in
    "<variables>" ^ aux qf

let instance body =
  {|<?xml version="1.0" encoding="UTF-8" standalone="no"?>
   <instance format="XCSP3" type="CSP"> |} ^
  body ^
  {|</instance>|}

let xcsp3_of_qformula qf = instance (xcsp3_of_qformula' qf)

let optimise kind x =
  "<objectives>" ^
    "<" ^ kind ^ ">" ^
    x ^
    "</" ^ kind ^ ">" ^
  "</objectives>\n"


let xcsp3_of_bab_qformula bab_formula =
  instance (
    (xcsp3_of_qformula' bab_formula.qf) ^
    (match bab_formula.optimise with
    | Minimize x -> optimise "minimize" x
    | Maximize x -> optimise "maximize" x
    | Satisfy -> ""))
