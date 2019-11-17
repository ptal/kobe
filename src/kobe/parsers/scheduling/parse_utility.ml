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
open Scanf

let ignore_lines file n =
  for _=1 to n do
    ignore(bscanf file "%[^\n]\n" (fun _ -> ()))
  done

let read_trailing_int_list file n =
  let numbers = List.map (fun _ -> bscanf file " %d " (fun x->x)) (Tools.range 1 n) in
  numbers
