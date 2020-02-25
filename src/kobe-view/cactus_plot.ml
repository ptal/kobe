(* Copyright 2019 Pierre Talbot, Tom Perroux

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Data_analyzer
open Analyzer_all

let hash_to_json_cactus (strat1 : strategy) (strat2 : strategy) =
  let rec hash_to_json_cactus_rec (strat1 : strategy) (strat2 : strategy) keys times =
    match keys with
    | [] -> "["^(remove_last_char times)^"]"
    | k::keys ->
      let time =
        try
          let instance = Hashtbl.find strat1.all k in
          let instance' = Hashtbl.find strat2.all k in
          let (t,t') = ((float_option_to_string instance.time),(float_option_to_string instance'.time)) in
          let time = "{"^"\"x\":"^t^",\"y\":"^t'^"}," in
          time
        with Not_found -> ""
      in hash_to_json_cactus_rec strat1 strat2 keys (times^time)
  in hash_to_json_cactus_rec strat1 strat2 (get_keys strat1.all) ""
