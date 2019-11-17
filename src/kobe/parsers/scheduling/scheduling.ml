(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore.System

let read_rcpsp problem_path =
  let ext = String.lowercase_ascii (Filename.extension problem_path) in
  if String.equal ext psplib_ext then
    Sm.read_sm_file problem_path
  else if String.equal ext patterson_ext then
    Patterson.read_patterson_file problem_path
  else if String.equal ext pro_gen_ext then
    Pro_gen_max.read_pro_gen_file problem_path
  else
    eprintf_and_exit ("Unknown file extension `" ^ ext ^ "`.")
