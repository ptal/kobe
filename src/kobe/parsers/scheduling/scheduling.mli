(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
    The data files is supposed to be well-formatted otherwise a parsing error will occur (but nothing is made to make this error readable since it ill-formatting is not the responsibility of this library). *)
val read_rcpsp: string -> Rcpsp_data.rcpsp
