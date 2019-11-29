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
open Kobecore.System
open Parsers_sat
open Parsers_scheduling
open Parsers_scheduling.Rcpsp_data

type problem =
| RCPSP of rcpsp
| SAT of bab_qformula

type result =
| WARNING of string
| IGNORE
| PROBLEM of problem

(** From the name of a file, create the associated problem. *)
let dispatch problem_path =
  if Sys.is_directory problem_path then begin
    if end_with problem_path "solution" || end_with problem_path "optimum" then IGNORE
    else WARNING ("subdirectory " ^ problem_path ^ " ignored.") end
  else
    let ext = String.lowercase_ascii (Filename.extension problem_path) in
    match ext with
    | ".sm" -> PROBLEM (RCPSP (Sm.read_sm_file problem_path))
    | ".rcp" -> PROBLEM (RCPSP (Patterson.read_patterson_file problem_path))
    | ".sch" -> PROBLEM (RCPSP (Pro_gen_max.read_pro_gen_file problem_path))
    | ".cnf" -> PROBLEM (SAT (Dimacs.read_dimacs_file problem_path))
    | _ ->
        WARNING ("file \"" ^ problem_path ^ "\" ignored (expected extension `" ^ ext ^ "`).")
