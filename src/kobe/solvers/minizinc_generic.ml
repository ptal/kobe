(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This file provides support function for solver-output compatible with MiniZinc.
    These are entries of the form `%%%mzn-stat: solveTime=0.008161` *)

open Core

let has_time_option = true

let minizinc_to_kobe_stats = [
  ("restarts", `Restarts);
  ("solutions", `Solutions);
  ("failures", `Fails);
  ("nodes", `Nodes);
  ("peakMem", `Memory `MB);
  ("solveTime", `Time `Sec);
  ("objective", `Optimum);
  ("peakDepth", `DepthMax)];;

(* Clean the entry assuming unsatisfiability. *)
let clean_entry (name, value) =
  match name with
  | `Solutions -> [(name, "0")]
  | `Optimum -> []
  | _ -> [(name,value)]

let contains_entry l name = l |> List.map fst |> List.exists (fun x-> x=name)

(* Clean some default values added by MiniZinc solvers.
   For example, Chuffed outputs `objective=-1` in case of unsatisfiability. *)
let clean_up_entries_if_unsat mzn_entries lines =
  let unsat_msg = "=====UNSATISFIABLE=====" in
  let unsat_msg_len = String.length unsat_msg in
  let is_unsat_msg l =
    if String.length l >= unsat_msg_len then
      let unsat_part = String.sub (String.trim l) 0 unsat_msg_len in
      unsat_part = unsat_msg
    else false in
  if List.exists is_unsat_msg lines then
  begin
    let mzn_entries = mzn_entries |> List.map clean_entry |> List.flatten in
    if contains_entry mzn_entries `Solutions then
      mzn_entries
    else
      mzn_entries@[(`Solutions, "0")]
  end
  else mzn_entries

(* Format: <irrelevant>: key=value *)
let extract_key_value_mzn split1 =
  let sep2 = '=' in
  let entry = String.trim (List.nth split1 1) in
  let split2 = String.split_on_char sep2 entry in
  List.hd split2, List.nth split2 1

let parse_output lines =
  let is_mzn_entry l = Tools.start_with l "%" in
  let is_not_mzn_entry l = not (is_mzn_entry l) in
  let mzn_entries = Generic.parse_output lines is_mzn_entry ' '
    minizinc_to_kobe_stats extract_key_value_mzn in
  let mzn_entries =
    if contains_entry mzn_entries `Optimum then
      mzn_entries
    else
      try
        let lines = List.filter is_not_mzn_entry lines in
        let obj = Scanf.sscanf (List.hd lines) "%s = %d;" (fun _ i -> string_of_int i) in
        mzn_entries@[(`Optimum, obj)]
      with _ -> mzn_entries in
  clean_up_entries_if_unsat mzn_entries lines

let make_command exec timeout option input_file =
  exec ^ " -t " ^ (string_of_int timeout) ^ " " ^ option ^ " " ^ input_file
