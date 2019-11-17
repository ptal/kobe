(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

let root_node_unsat =
  [(`Time `Sec, "0.01"); (`Restarts, "0");
   (`Solutions, "0"); (`Fails, "1"); (`Nodes, "1");
   (`Satisfiability, "unsat"); (`Optimum, "unsat")]

let parse_output lines is_interesting sep solver_to_kobe extract_key_value =
     lines
  |> List.map String.trim
  |> List.filter is_interesting
  |> List.map (String.split_on_char sep)
  |> List.filter (fun l -> (List.length l) = 2)
  |> List.map (fun l ->
      try
        let key, value = extract_key_value l in
        let key, value = String.trim key, String.trim value in
        let key = List.assoc key solver_to_kobe in
        [(key, value)]
      with Not_found -> []
     )
  |> List.flatten
