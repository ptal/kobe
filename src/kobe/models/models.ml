(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Parsers.Dispatch
open Kobecore.System
open Kobecore.Bench_desc_j

let unsupported_decomposition name decompositions =
  if List.length decompositions > 0 then
     eprintf_and_exit (name ^ " does not support decomposition, but `" ^ (List.hd decompositions).name ^ "` was provided.")

let formula_of_problem decompositions = function
  | JOBSHOP jobshop ->
      unsupported_decomposition "JOBSHOP" decompositions;
      Jobshop.formula_of_jobshop jobshop
  | RCPSP rcpsp -> Rcpsp.formula_of_rcpsp rcpsp decompositions
  | SAT bf -> (unsupported_decomposition "SAT" decompositions; bf)