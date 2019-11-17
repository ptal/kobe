(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Given a filename, parse its data into a RCPSP structure.
    The data is supposed to be formatted according to the SM format (PSPLIB).
    See also the [documentation](https://ptal.github.io/scheduling-data.html). *)
val read_sm_file: string -> Rcpsp_data.rcpsp
