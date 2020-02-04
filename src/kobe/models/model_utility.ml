(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang.Ast

let quantify_vars vars ty formula =
  let exists formula name =
    Exists (name, ty, formula) in
  List.fold_left exists formula (List.rev vars)

let dom_of_var ty l u v =
   And(
    Cmp (Var v, GEQ, Cst (l, ty)),
    Cmp (Var v, LEQ, Cst (u, ty)))

let dom_of_vars vars ty l u =
  List.map (dom_of_var ty l u) vars
