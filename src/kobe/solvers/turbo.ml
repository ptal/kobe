(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements `Solver_sig` for the Turbo constraint solver. *)

open Core

let has_time_option = false

let turbo_to_kobe_stats = [
  ("solutions", `Solutions);
  ("fails", `Fails);
  ("nodes", `Nodes);
  ("objective", `Optimum);
  ("peakDepth", `DepthMax)]

let is_interesting line =
  turbo_to_kobe_stats |> List.map fst |> List.exists (Tools.start_with line)

let extract_key_value_turbo key_value =
  List.nth key_value 0, List.nth key_value 1

let parse_output lines =
  let lines = List.filter (fun s -> String.length s > 0) lines in
  let entries = Generic.parse_output lines is_interesting '='
    turbo_to_kobe_stats extract_key_value_turbo in
  entries

let make_command exec _ option input_file = exec ^ " " ^ option ^ " " ^ input_file
