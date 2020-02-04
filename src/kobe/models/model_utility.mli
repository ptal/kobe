(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Bounds
open Core.Types
open Lang.Ast

(** [quantify_vars vars ty qf] adds existential quantifiers in front of `qf` for each variable in `vars`
    The type of the variables is `ty`. *)
val quantify_vars: string list -> var_ty -> qformula -> qformula

(** [dom_of_var ty l u v] Constraints the variable `v` in the interval `(l,u)`.
    Constants are given the type `ty`. *)
val dom_of_var: var_concrete_ty -> Bound_rat.t -> Bound_rat.t -> string -> formula

(** [dom_of_vars vars ty l u] Conjunction of the constraints of the variables in `vars` in the interval `(l,u)`.
    Constants are given the type `ty`. *)
val dom_of_vars: string list -> var_concrete_ty -> Bound_rat.t -> Bound_rat.t -> formula list
