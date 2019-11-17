(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Parsing/utility functions that are common across formats (Patterson, SM, ProGen/max).  *)

open Scanf

(** Discards `n` lines from the input. *)
val ignore_lines: Scanning.in_channel -> int -> unit

(** Read a list of `n` space separated integers. *)
val read_trailing_int_list: Scanning.in_channel -> int -> int list