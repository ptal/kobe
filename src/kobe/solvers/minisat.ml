(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements `Solver_sig` for the Minisat SAT solver. *)

open Core.Tools
open Kobecore

let has_time_option = false

(* List of `(minisat-kw,kobe-kw)` keywords where `minisat-kw` is the beginning the output line where we can find the value `kobe-kw`. *)
let minisat_to_kobe_stats = [
  ("restarts", `Restarts);
  ("conflicts", `Fails);
  ("propagations", `Nodes);
  ("Memory used", `Memory `MB);
  ("CPU time", `Time `Sec)];;

let supported_statistics =
  [`Nodes; `Fails; `Solutions; `Restarts; `Memory `MB; `Time `Sec; `Satisfiability]

let is_interesting line =
  minisat_to_kobe_stats |> List.map fst |> List.exists (start_with line)

let parse_last_line lines =
  let last_line =
      (* If the timeout is too tight, Minisat might exit even before the parsing has succeeded, thus there is no status message. *)
      if List.length lines > 0 then
         List.nth lines ((List.length lines) - 1)
      else "INDETERMINATE" in
  let (satisfiability, solutions) =
    if last_line = "INDETERMINATE" then "unknown", "0"
    else if last_line = "SATISFIABLE" then "sat", "1"
    else if last_line = "UNSATISFIABLE" then "unsat", "0"
    else System.eprintf_and_exit
      ("Could not parse the last line of MiniSAT output, which is `" ^ last_line ^ "`")
  in
  [(`Satisfiability, satisfiability); (`Solutions, solutions)]

(* Format: key : value <irrelevant> *)
let extract_key_value_minisat split1 =
  let sep2 = ' ' in
  let key = List.nth split1 0 in
  let entry = String.trim (List.nth split1 1) in
  let split2 = String.split_on_char sep2 entry in
  key, List.hd split2

let parse_output lines =
  let lines = List.filter (fun s -> String.length s > 0) lines in
  let entries = Generic.parse_output lines is_interesting ':'
    minisat_to_kobe_stats extract_key_value_minisat in
  entries@(parse_last_line lines)

let make_command exec _ option input_file =
  exec ^ " -verb=1 " ^ option ^ " " ^ input_file
