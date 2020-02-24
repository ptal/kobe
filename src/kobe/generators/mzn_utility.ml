(* Copyright 2020 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

let string_of_list to_string l =
  List.fold_left (fun s e -> s ^ (to_string e) ^ ", ") "" l

let list_template name content =
  name ^ " = [" ^ content ^ "];\n"

let string_of_set_list name l =
  let content = string_of_list (fun (l,u) ->
    (string_of_int l) ^ ".." ^ (string_of_int u)) l in
  list_template name content

let list_to_mzn name l = list_template name (string_of_list string_of_int l)

let string_of_2D_list name l =
  name ^ " = [|\n" ^
  (List.fold_left (fun a r -> a ^ (string_of_list string_of_int r) ^ "\n  |") "" l) ^
  "];\n"
