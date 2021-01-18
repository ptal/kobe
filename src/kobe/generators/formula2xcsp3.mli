(* Copyright 2021 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Lang.Ast

(** Generate a XCSP3 model from a logic formula. *)
val xcsp3_of_bab_qformula: bab_qformula -> string
val xcsp3_of_qformula: qformula -> string
