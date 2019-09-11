open Kobecore.Bench_desc_t

module type Solver_sig =
sig
  (** `false` if the solver does not provide a timeout option.
       In this case, we wrap the call to the solver in a `timeout` command. *)
  val has_time_option: bool

  (** Make the command for calling the solver.
      `make_command exec timeout strategy input_file`.
      The command should output its result on the standard output. *)
  val make_command: string -> int -> string -> string -> string

  (** From the solver output, retrieve the list of corresponding CSV fields with their associated values.
      If the solver does not provide an information, do not fill a default value, this is automatically handled later. *)
  val parse_output: string list -> (csv_field * string) list
end
