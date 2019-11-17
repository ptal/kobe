(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements `Solver_sig` for the Choco constraint solver. *)

let has_time_option = true
let parse_output _ = raise (Failure "Choco is not yet supported.")
let make_command exec = Minizinc_generic.make_command (exec ^ " -stat")
