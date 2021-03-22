(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Kobecore.Bench_instance_t
open Bounds
open Measure

let csv_line items = String.concat ", " items

let string_of_time_unit = function
  | `NSec -> "ns"
  | `MSec -> "ms"
  | `Sec -> "s"

let string_of_memory_unit = function
  | `Bytes -> "B"
  | `KB -> "KB"
  | `MB -> "MB"
  | `GB -> "GB"

let csv_field_name = function
  | `ProblemPath -> "path"
  | `ProblemName -> "problem"
  | `Time(_) -> "time"
  | `Memory(_) -> "memory"
  | `Solutions -> "solutions"
  | `Fails -> "fails"
  | `Nodes -> "nodes"
  | `NodesBeforeLastSol -> "nodes_before_last_sol"
  | `Optimum -> "optimum"
  | `Satisfiability -> "satisfiability"
  | `Exhaustivity -> "exhaustivity"
  | `Restarts -> "restarts"
  | `DepthMax -> "depthmax"
  | `Variables -> "variables"
  | `Constraints -> "constraints"

let csv_header output_statistics =
  let names = List.map csv_field_name output_statistics in
  csv_line names

let print_csv_line line =
  Printf.printf "%s\n" line;
  flush_all ()

let print_csv_header _bench output_statistics = print_csv_line (csv_header output_statistics)

let csv_time_field bench measure u =
  let aux time =
    let time = Mtime.Span.of_uint64_ns time in
      let time = match u with
        | `NSec -> Mtime.Span.to_ns time
        | `MSec -> Mtime.Span.to_ms time
        | `Sec -> Mtime.Span.to_s time in
      Printf.sprintf "%.2f%s" time (string_of_time_unit u) in
  match measure.time with
  | None -> aux (Int64.mul (Int64.of_int 1000000) (Int64.of_int bench.timeout))
  | Some(time) -> aux time


let csv_memory_field measure u =
  let format bytes n =
    (Int64.to_string (Int64.div bytes n)) ^ "." ^
    (Int64.to_string (Int64.rem (Int64.rem bytes n) (Int64.of_int 100))) in
  match measure.memory with
  | None -> "none"
  | Some(bytes) ->
    let mem = match u with
      | `Bytes -> Int64.to_string bytes
      | `KB -> format bytes 1000L
      | `MB -> format bytes (Int64.mul 1000L 1000L)
      | `GB -> format bytes (Int64.mul (Int64.mul 1000L 1000L) 1000L) in
    Printf.sprintf "%s%s" mem (string_of_memory_unit u)

let csv_field_value bench measure = function
  | `ProblemPath -> measure.problem_path
  | `ProblemName -> Filename.basename measure.problem_path
  | `Time(u) -> csv_time_field bench measure u
  | `Memory(u) -> csv_memory_field measure u
  | `Solutions -> (string_of_int measure.stats.sols)
  | `Fails -> (string_of_int measure.stats.fails)
  | `Nodes -> (string_of_int measure.stats.nodes)
  | `NodesBeforeLastSol -> (string_of_int measure.stats.nodes_before_last_sol)
  | `Optimum -> begin
      match measure.optimum, measure.time with
      | Some(o), _ -> Bound_rat.to_string o
      | None, Some _ -> "unsat"
      | None, None -> "none" end
  | `Satisfiability -> begin
      match measure.satisfiability with
      | Some True -> "sat"
      | Some False -> "unsat"
      | _ -> "unknown" end
  | `Exhaustivity -> (string_of_bool measure.exhaustivity)
  | `Restarts -> (string_of_int measure.stats.restarts)
  | `DepthMax -> (string_of_int measure.stats.depth_max)
  | `Variables -> (string_of_int measure.variables)
  | `Constraints -> (string_of_int measure.constraints)

let bench_to_csv bench measure output_statistics =
  let values = List.map (csv_field_value bench measure) (`ProblemName::output_statistics) in
  csv_line values

let print_as_csv bench measure output_statistics = print_csv_line (bench_to_csv bench measure output_statistics)

let print_exception problem_path msg = print_csv_line (Format.sprintf "%s: %s" problem_path msg)

let print_error_csv problem_path err_file =
  print_csv_line (Format.sprintf "%s, failure (see file `%s`)" (Filename.basename problem_path) err_file)
