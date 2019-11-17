(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Parsers_scheduling.Rcpsp_data

(** Given a project of a RCPSP specification, generate the MiniZinc data file (DZN) with the data formatted as follows:
      * `n_res = 5;` for 5 resources.
      * `rcap = [3, 4, 5, 5, 5];` for the capacity of each resource.
      * `n_tasks = 12;` for 12 tasks.
      * `dur = [0, 5, 3, 1, 4, 9, 3, 4, 7, 3, 4, 0, ];` for the duration of each task.
      * `rr = [| 0, 0, 0, ...
               |0, 3, 0, ...
               ... |];`
        Each entry corresponds to a resource usage (lines) of a task (columns).

      * `n_dc = 17;` number of precedence constraints.
      * `dcons = [|1, 0, 2 |2, 8, 3 | ...|]` each entry is a precedence constraints of form `x + c <= y`.

    See also the RCPSP MiniZinc models in the repository [kobe-rcpsp](https://github.com/ptal/kobe-rcpsp).
 *)
val make_dzn_data: rcpsp -> project -> string
