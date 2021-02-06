(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore.Bench_desc_t

(** Interface of the command of a constraint solver and to transform its output into Kobe statistics. *)
module type S =
sig
  (** `false` if the solver does not provide a timeout option.
       In this case, we wrap the call to the solver in a `timeout` command. *)
  val has_time_option: bool

  (** [make_command exec timeout option input_file].
      Create the command line, as a string, for calling the solver.
      The command should output its result on the standard output. *)
  val make_command: string -> int -> string -> string -> string

  (** From the solver output, retrieve the list of corresponding CSV fields with their associated values.
      If the solver does not provide an information, do not fill a default value, this is automatically handled later. *)
  val parse_output: string list -> (csv_field * string) list
end

(** Create a solver module from its name.
    Every new solver should be registered here for genericity. *)
let make_solver name : (module S) =
  match name with
  | "choco" -> (module Fzn_choco)
  | "chuffed" -> (module Fzn_chuffed)
  | "gecode" -> (module Fzn_gecode)
  | "minisat" -> (module Minisat)
  | "kissat" -> (module Kissat)
  | "turbo" -> (module Turbo)
  | "turbo_seq" -> (module Turbo)
  | _ -> raise (Failure ("The solver `" ^ name ^ "` is not supported."))
