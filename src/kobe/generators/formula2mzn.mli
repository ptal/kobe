(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang.Ast
open Kobecore.Bench_desc_j

(** Generate a MiniZinc model from a logic formula and a search strategy. *)
val mzn_of_bab_qformula: bab_qformula -> search_strategy -> string
val mzn_of_qformula: qformula -> search_strategy -> string
