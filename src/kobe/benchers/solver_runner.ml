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
open Solvers
open Bounds
open Measure
open Kobecore.Bench_instance_j
open Kobecore.Bench_desc_j
open Kobecore.System

module type S =
sig
  val run: bench_instance -> solver_config -> solver_option option -> string -> string -> unit
end

module Make(Solver: Solver_sig.S) =
struct
  let ios = int_of_string
  let kleene_of_string = function
  | "sat" -> Kleene.True
  | "unsat" -> Kleene.False
  | "unknown" -> Kleene.Unknown
  | s -> failwith ("kleene_of_string called with `" ^ s ^ "`")

  let time_of_string u t =
    let t = float_of_string t in
    let t_int = Int64.of_float t in
    match u with
    | `NSec -> time_of_ns t_int
    | `MSec -> time_of_ms t_int
    (* Usually the time of the solver is written as 8.2389s, we want to retreive ".2389" as well. *)
    | `Sec -> time_of_ms (Int64.of_float (t *. 1000.))

  let memory_of_string u m =
    let m = float_of_string m in
    let m = match u with
      | `Bytes -> m
      | `KB -> m *. 1000.
      | `MB -> m *. 1000000.
      | `GB -> m *. 1000000000. in
    Int64.of_float m

  let fill_measure bench problem_path entries =
    let rec aux2 m s l = aux { m with stats = s } l
    and aux m = function
      | [] -> m
      | (`ProblemPath, _)::l -> aux m l
      | (`ProblemName, _)::l -> aux m l
      | (`Memory(u), mem)::l ->
          let mem = memory_of_string u mem in
          aux { m with memory=Some(mem) } l
      | (`Optimum, "unsat")::l -> aux { m with optimum=None } l
      | (`Optimum, obj)::l -> aux { m with optimum=Some (Bound_rat.of_string obj) } l
      | (`Solutions, x)::l -> aux2 m {m.stats with sols=(ios x)} l
      | (`NodesBeforeLastSol, x)::l -> aux2 m {m.stats with nodes_before_last_sol=(ios x)} l
      | (`Fails, x)::l -> aux2 m {m.stats with fails=(ios x)} l
      | (`Nodes, x)::l -> aux2 m {m.stats with nodes=(ios x)} l
      | (`Satisfiability, x)::l -> aux { m with satisfiability=Some (kleene_of_string x) } l
      | (`Restarts, x)::l -> aux2 m {m.stats with restarts=(ios x)} l
      | (`DepthMax, x)::l -> aux2 m {m.stats with depth_max=(ios x)} l
      | (`Time(_), _)::l -> aux m l
    in
    (* We fill the time afterwards only if it did not timeout (satisfiability <> unknown). *)
    let rec fill_time m = function
      | [] -> m
      | (`Time(u), time)::_ ->
          let time = time_of_string u time in
          Measure.update_time bench { m.stats with elapsed=time } m
      | _::l -> fill_time m l in
    let m = aux (Measure.default problem_path) entries in
    fill_time m entries

  let create_measure bench problem_path output =
    let data = file_to_string output in
    let lines = String.split_on_char '\n' data in
    let entries = Solver.parse_output lines in
    let measure = fill_measure bench problem_path entries in
    guess_missing_measures measure

  let wrap_timeout (bench:bench_instance) command =
    if Solver.has_time_option then command
    else "timeout " ^ (string_of_int bench.timeout) ^ "s " ^ command

  (* Decompress a file with `xz`. If the decompressed file already exist in /tmp, avoid decompressing it a second time. *)
  let decompress input_file =
    let decompressed_file = "/tmp/" ^ (Filename.remove_extension (Filename.basename input_file)) in
    if Sys.file_exists decompressed_file then
      decompressed_file
    else
      match Filename.extension input_file with
      | ".xz" ->
          begin
            let decompress_xz = "xz -k -c -d " ^ input_file ^ " > " ^ decompressed_file in
            (* let _ = Printf.printf "%s\n" decompress_xz ; flush_all () in *)
            match call_command decompress_xz with
            | 0 -> decompressed_file
            | _ -> eprintf_and_exit ("Could not decompress .xz file `" ^ input_file ^ "`. Do you have `xz` installed?")
          end
      | _ -> input_file

  let run (bench:bench_instance) solver solver_option problem_path input_file =
    let input_file = decompress input_file in
    let solver_option = match solver_option with Some x -> x.option | None -> "" in
    let output_file = make_unique_file_name output_file in
    let error_file = make_unique_file_name error_file in
    let command = Solver.make_command solver.exec bench.timeout solver_option input_file in
    let command = wrap_timeout bench command in
    let command = command ^ " > " ^ output_file ^ " 2> " ^ error_file in
    (* let _ = Printf.printf "%s\n" command; flush_all () in *)
    let _ = call_command command in
    let measure = create_measure bench problem_path output_file in
    Csv_printer.print_as_csv bench measure
end
