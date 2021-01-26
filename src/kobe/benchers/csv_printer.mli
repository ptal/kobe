(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

(** Print a benchmarking measure to the CSV format. *)

open Kobecore.Bench_instance_t
open Measure

(** Print the header of the CSV file. It contains the name of the column. *)
val print_csv_header: bench_instance -> unit
val print_as_csv: bench_instance -> measure -> unit
val print_exception: string -> string -> unit
val print_error_csv: string -> string -> unit
