(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Core
open Kobecore

(* Note that on recent system, this executable has been renamed to `minizinc` so we add the additional option "-c".  *)
let make_command exec solver_config mzn_file dzn_file fzn_file =
  let no_compile = if (Tools.start_with exec "minizinc") then " -c " else "" in
  exec ^ no_compile ^ " --no-optimize -I " ^ solver_config.Bench_desc_j.globals ^ " -o " ^ fzn_file ^ " " ^ mzn_file ^ " " ^ dzn_file
