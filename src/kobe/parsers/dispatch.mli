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
open Parsers_scheduling.Rcpsp_data
open Parsers_scheduling.Jobshop_data

type problem =
| JOBSHOP of jobshop
| RCPSP of rcpsp
| SAT of bab_qformula

type result =
| WARNING of string (** An unexpected file was encountered. *)
| IGNORE (** An expected file to ignore was encountered. *)
| PROBLEM of problem

(** From the name of a file, creates the associated problem.
    If the file is invalid, `WARNING` is returned.
    Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
    The data files is supposed to be well-formatted otherwise a parsing error will occur (but nothing is made to make this error readable since it ill-formatting is not the responsibility of this library). *)
val dispatch: string -> result
