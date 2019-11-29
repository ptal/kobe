(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** From the RCPSP/max data structure and a decomposition name of the cumulative global constraint,
    generate the logical formula of the RCPSP/max.
    Note that this module could be easily extended to take into account continuous tasks duration. *)

open Lang.Ast
open Parsers_scheduling.Rcpsp_data
open Kobecore.Bench_instance_j

(** Given the name of a decomposition for cumulative (`TaskRD` or `TimeRD`), transform the RCPSP/max into a formula. *)
val formula_of_rcpsp: rcpsp -> decomposition list -> bab_qformula
