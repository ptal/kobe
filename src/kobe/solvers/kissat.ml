(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** This module implements `Solver_sig` for the Kissat SAT solver. *)

open Core.Tools
open Kobecore

let has_time_option = false

(* List of `(kissat-kw,kobe-kw)` keywords where `kissat-kw` is the beginning the output line where we can find the value `kobe-kw`. *)
let kissat_to_kobe_stats = [
  ("c restarts", `Restarts);
  ("c conflicts", `Fails);
  ("c propagations", `Nodes);
  ("c maximum-resident-set-size", `Memory `Bytes)];;

let is_interesting line =
  kissat_to_kobe_stats |> List.map fst |> List.exists (start_with line)

let parse_satisfiability_line lines =
  let satisfiability, solutions =
    match List.find_opt (fun l -> start_with l "s ") lines with
    | None -> "unknown", "0"
    | Some "s SATISFIABLE" -> "sat", "1"
    | Some "s UNSATISFIABLE" -> "unsat", "0"
    | Some l -> System.eprintf_and_exit
        ("Could not parse the satisfiability line of Kissat output, which is `" ^ l ^ "`")
  in
  [(`Satisfiability, satisfiability); (`Solutions, solutions)]

let parse_time_line lines =
  match List.find_opt (fun l -> start_with l "c process-time") lines with
  | Some l ->
      let l = String.split_on_char ' ' l in
      let l = List.filter (fun x -> not (String.equal x "")) l in
      let t = List.nth l 3 in
      [(`Time `Sec, t)]
  | _ -> []

let parse_output lines =
  let lines = List.filter (fun s -> String.length s > 0) lines in
  let entries = Generic.parse_output lines is_interesting ':'
    kissat_to_kobe_stats Minisat.extract_key_value_minisat in
  entries@(parse_satisfiability_line lines)@(parse_time_line lines)

let make_command exec _ option input_file =
  exec ^ option ^ " " ^ input_file
