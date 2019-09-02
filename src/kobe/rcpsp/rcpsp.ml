open Kobecore.System

module Rcpsp_data=Rcpsp_data
module Rcpsp_model=Rcpsp_model

(* Precondition: Sanity checks on the file path are supposed to be already done, otherwise it can throw I/O related exceptions.
The files from PSPlib are also supposed to be well-formatted. *)
let read_rcpsp problem_path =
  let ext = String.lowercase_ascii (Filename.extension problem_path) in
  if String.equal ext psplib_ext then
    Sm_format.read_sm_file problem_path
  else if String.equal ext patterson_ext then
    Patterson.read_patterson_file problem_path
  else if String.equal ext pro_gen_ext then
    Pro_gen_max.read_pro_gen_file problem_path
  else
    eprintf_and_exit ("Unknown file extension `" ^ ext ^ "`.")
