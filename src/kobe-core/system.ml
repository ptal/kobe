(* Copyright 2019 Pierre Talbot

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 3 of the License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details. *)

open Printf
open Bench_instance_t

let json_ext = ".json"
let usage = "Usage: kobe_gen <configuration file>\n"

let output_file = "out.txt"
let error_file = "err.txt"

let print_warning msg =
  eprintf "[Warning] %s\n%!" msg

let eprintf_and_exit msg =
  eprintf "[Error] %s\n%!" msg;
  exit 1

let file_argument fname =
  if Filename.check_suffix fname json_ext
  then
    fname
  else
    eprintf_and_exit (sprintf
      "Unknown file extension: %s. We expect `%s`.\n" fname json_ext)

let find_file fname =
  if Sys.file_exists fname then
    fname
  else begin
    Printf.printf "%s" usage;
    eprintf_and_exit (sprintf
      "Cannot find the file %s\n%!" fname)
  end

let string_to_file fpath data =
  let oc = open_out fpath in
  Printf.fprintf oc "%s" data;
  close_out oc

let file_to_string fname =
  let fpath = find_file fname in
  begin
    let ic = open_in fpath in
    try
      let content = Std.input_all ic in
      close_in ic;
      content
    with e ->
      close_in ic;
      eprintf_and_exit (sprintf
        "Reading file %s failed.\nError: %s." fname (Printexc.to_string e))
  end

let get_bench_desc () =
  if Array.length Sys.argv < 2 then
  begin
    eprintf_and_exit "Benchmarks description file missing (see the manual for the specification)."
  end
  else file_to_string (Array.get Sys.argv 1)

let get_config_desc () =
  if Array.length Sys.argv > 2 then
    Some (file_to_string (Array.get Sys.argv 2))
  else
    None

let contain_string s sub extract =
  let s = String.trim s in
  let sub = String.trim sub in
  let sub_len = String.length sub in
  if String.length s >= sub_len then
    (extract s sub_len) = sub
  else
    false

let end_with s tail =
  contain_string s tail (fun s tail_len ->
    String.sub s ((String.length s) - tail_len) tail_len)

let is_digit c = c >= '0' && c <= '9'

(* Extract the longest number in `s` starting at position `i`. *)
let extract_number s i =
  let open String in
  let rec aux l =
    let j = i + l in
    if length s = j then l
    else if is_digit (get s j) then aux (l+1)
    else l
  in
    let l = aux 0 in
    let s' = sub s i l in
    (int_of_string s', i + l)

(* It compares two strings taking into account the number parts.
   For example ["a1"; "a10"; "a2"] is sorted as ["a1"; "a2"; "a10"].  *)
let natural_comparison x y =
  let open String in
  let rec aux i j =
    if (length x) = i then -1
    else if (length y) = j then 1
    else
      let a = get x i in
      let b = get y j in
      match Stdlib.compare a b with
      | _ when is_digit a && is_digit b -> compare_number_sequence i j
      | 0 -> aux (i+1) (j+1)
      | r -> r
  and compare_number_sequence i j =
    let (n, i') = extract_number x i in
    let (m, j') = extract_number y j in
    (* let _ = Printf.printf "%d , %d\n" n m in *)
    match Stdlib.compare n m with
    | 0 -> aux i' j'
    | r -> r
  in
    aux 0 0

let remove_trailing_slash dir1 =
  if String.length dir1 > 1 then
    let l = (String.length dir1) - 1 in
    if dir1.[l] = Filename.dir_sep.[0] then String.sub dir1 0 l else dir1
  else
    dir1

let concat_dir dir1 dir2 =
  let dir1 = remove_trailing_slash dir1 in
  if dir1 = "" then dir2
  else if dir2 = "" then dir1
  else dir1 ^ Filename.dir_sep ^ dir2

let list_of_problems bench =
  let path = bench.problem_set_path in
  if Sys.is_directory path then
    let files = Sys.readdir path in
    Array.sort natural_comparison files;
    Array.to_list files |>
    List.map (fun x -> concat_dir path x)
  else
    eprintf_and_exit ("The problem set path `" ^ path ^ "` must be a directory. The structure of the input database must follow some conventions described in the benchmarking manual.")

let call_command command =
  (* Printf.printf "%s\n" command; *)
  flush_all ();
  let status = Sys.command command in
  status

let time_of coeff time =
  Mtime.Span.of_uint64_ns (Int64.mul (Int64.of_int coeff) time)

let time_of_ns = time_of 1
let time_of_ms = time_of 1000000
let time_of_sec = time_of 1000000000

let timeout_of_bench bench = time_of_sec (Int64.of_int bench.timeout)

let make_unique_file_name name =
  let rec aux name i =
    let base = Filename.remove_extension name in
    let ext = Filename.extension name in
    let path = "/tmp/" ^ base ^ (string_of_int i) ^ ext in
    if Sys.file_exists path then
      aux name (i+1)
    else
     path
  in
  aux name 0

let create_file data name =
  let oc = open_out name in
  fprintf oc "%s\n" data;
  close_out oc

let make_file data file =
  let filename = make_unique_file_name file in
  create_file data filename;
  filename
