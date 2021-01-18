(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore
open Bench_desc_j
open Bench_instance_j

val find_solver: benchmark -> string -> solver_config
val is_fzn_solver: string -> bool
val is_standalone_solver: string -> bool
val is_xcsp3_solver: string -> bool

val map_solvers: (solver -> 'a list) -> (string -> bool) -> solver list -> 'a list
val map_fzn_solvers: (solver -> 'a list) -> solver list -> 'a list
val map_standalone_solvers: (solver -> 'a list) -> solver list -> 'a list
val map_xcsp3_solvers: (solver -> 'a list) -> solver list -> 'a list
val map_solver_option: (solver_option option -> 'a) -> solver_option list -> 'a list

val retrieve_solvers: benchmark -> string list -> solver_config list
val check_pipeline_sink: string -> solver_instance list -> solver_instance list

val model_to_absolute_sink: benchmark -> make_model -> model_to_absolute -> solver_instance list
val fzn_solver_sink: benchmark -> make_model -> model_to_fzn -> solver list -> solver_instance list
val xcsp3_solver_sink: benchmark -> make_model -> solver list -> solver_instance list
val mzn2fzn_solve_sink: benchmark -> mzn_to_fzn -> solver list -> solver_instance list
val solve_sink: benchmark -> solver list -> solver_instance list

val model_to_fzn_state: benchmark -> make_model -> model_to_fzn -> pipeline list -> solver_instance list
val make_model_state: benchmark -> make_model -> pipeline list -> solver_instance list
val mzn_to_fzn_state: benchmark -> mzn_to_fzn -> pipeline list -> solver_instance list
val model_to_xcsp3_state: benchmark -> make_model -> pipeline list -> solver_instance list

val initial_state: benchmark -> pipeline list -> solver_instance list
val error_no_pipeline: unit -> unit
val make_pipeline: benchmark -> pipeline list -> solver_instance list
val make_solver_instances: benchmark -> solver_instance list

val make_instances: benchmark -> problem_set -> bench_instance list
val gen_benches: benchmark -> bench_instance list

val finalize_bench: benchmark -> bench_instance -> bench_instance

val option_name: solver_option option -> string
val solver_uid: solver_config -> string
val decompositions_name: decomposition list -> string

val create_solver_dir: bench_instance -> string
val create_result_filename: bench_instance -> string

val copy_dir: benchmark -> bench_instance -> string -> unit
val copy_optimum_dir: benchmark -> bench_instance -> unit
val copy_solution_dir: benchmark -> bench_instance -> unit

val register_bench: benchmark -> bench_instance -> unit

val config_from_json: string -> benchmark
