open Kobecore
open Bench_desc_j
open Bench_instance_j
open System

let find_solver benchmark name =
  try
    List.find (fun (c: solver_config) -> String.equal c.name name) benchmark.solvers_config
  with Not_found ->
    System.eprintf_and_exit ("Solver named `" ^ name ^ "` is not described in the configuration file. Please see `benchmark/data/benchmarks.json` for example of solver configuration.")

let is_fzn_solver = function
  | "gecode" | "chuffed" | "choco" -> true
  | _ -> false

let is_standalone_solver = function
  | "minisat" -> true
  | _ -> false

let map_solvers f filter (solvers: solver list) =
     solvers
  |> List.filter (fun (s: solver) -> filter s.name)
  |> List.map f
  |> List.flatten

let map_fzn_solvers f (solvers: solver list) = map_solvers f is_fzn_solver solvers
let map_standalone_solvers f (solvers: solver list) = map_solvers f is_standalone_solver solvers

let map_solver_option f options =
  if List.length options = 0 then [f None]
  else
    options |> List.map (fun x -> Some x) |> List.map f

let retrieve_solvers benchmark solvers =
  List.map (fun name -> find_solver benchmark name) solvers

let check_pipeline_sink state_name (instances: solver_instance list) =
  if List.length instances = 0 then
    eprintf_and_exit ("The pipeline `" ^ state_name ^ "` is not a sink pipeline and is not followed by any sink (such as `Solve`).\n\
    Please consult the manual for more information.")
  else
    instances

let model_to_absolute_sink benchmark (model: make_model) abs =
  let absolute_solver = find_solver benchmark "absolute" in
  List.flatten (
  List.map (fun (domain: domain) ->
  List.map (fun strategy ->
    `AbsoluteSolver {
      version=absolute_solver.version;
      decompositions=model.decompositions;
      domain=domain.name;
      strategy }
  ) domain.strategies
  ) abs.domains)

let fzn_solver_sink benchmark (model: make_model) (fzn: model_to_fzn) (solvers: solver list) =
  map_fzn_solvers (fun (solver: solver) ->
    let solver_config = find_solver benchmark solver.name in
    List.flatten (
    List.map (fun strategy ->
    map_solver_option (fun option ->
      `FznSolver {
        solver=solver_config;
        option;
        decompositions=model.decompositions;
        strategy }
    ) solver.options
    ) fzn.strategies)
  ) solvers

let model_to_fzn_state benchmark (model: make_model) (fzn: model_to_fzn) (pipelines: pipeline list) =
  let rec aux : pipeline list -> solver_instance list = function
    | (`Solve s)::l ->
        (fzn_solver_sink benchmark model fzn s)@(aux l)
    | _::l -> aux l
    | [] -> [] in
  check_pipeline_sink "ModelToFzn" (aux pipelines)

let make_model_state benchmark (model: make_model) (pipelines: pipeline list) =
  let rec aux : pipeline list -> solver_instance list = function
    | (`ModelToFzn fzn)::l ->
        (model_to_fzn_state benchmark model fzn l)@(aux l)
    | (`ModelToAbsolute abs)::l ->
        (model_to_absolute_sink benchmark model abs)@(aux l)
    | _::l -> aux l
    | [] -> [] in
  check_pipeline_sink "MakeModel" (aux pipelines)

let mzn2fzn_solve_sink benchmark (mzn: mzn_to_fzn) solvers =
  map_fzn_solvers (fun solver ->
    let solver_config = find_solver benchmark solver.name in
    List.flatten (List.flatten (
    List.map (fun model ->
    List.map (fun strategy ->
    map_solver_option (fun option ->
      `MznSolver {
        solver=solver_config;
        option;
        model;
        strategy }
    ) solver.options
    ) mzn.strategies
    ) mzn.models))
  ) solvers

let mzn_to_fzn_state benchmark mzn pipelines =
  let rec aux = function
    | (`Solve s)::l ->
        (mzn2fzn_solve_sink benchmark mzn s)@(aux l)
    | _::l -> aux l
    | [] -> [] in
  check_pipeline_sink "MznToFzn" (aux pipelines)

let solve_sink benchmark solvers =
  map_standalone_solvers (fun (solver: solver) ->
    let solver_config = find_solver benchmark solver.name in
    map_solver_option (fun option ->
      `StandaloneSolver {
        solver=solver_config;
        option; }
    ) solver.options
  ) solvers

let initial_state benchmark (pipelines: pipeline list) =
  let rec aux = function
    | (`MakeModel x)::l ->
        (make_model_state benchmark x l)@(aux l)
    | (`MznToFzn x)::l ->
        (mzn_to_fzn_state benchmark x l)@(aux l)
    | (`Solve x)::l ->
        (solve_sink benchmark x)@(aux l)
    | _::l -> aux l
    | _ -> [] in
  aux pipelines

let error_no_pipeline () =
  eprintf_and_exit "The list of pipelines must terminate by a `Solve item.\n\
    Please see the manual for more information."

let make_pipeline benchmark pipelines =
  if List.length pipelines = 0 then
    error_no_pipeline ()
  else
    let instances = initial_state benchmark pipelines in
    if List.length instances = 0 then
      error_no_pipeline ()
    else
      instances

let make_solver_instances benchmark =
  make_pipeline benchmark benchmark.pipelines

let make_instances benchmark problem_set =
  let solver_instances = make_solver_instances benchmark in
  List.map (fun s ->
    { problem_set_path=problem_set.path;
      timeout=problem_set.timeout;
      csv=benchmark.csv;
      solver_instance=s; }) solver_instances

let gen_benches benchmark =
  List.flatten (List.map (make_instances benchmark) benchmark.problem_sets)

let finalize_bench benchmark bench =
  let path = System.concat_dir benchmark.input_dir bench.problem_set_path in
  {bench with problem_set_path=path}

let option_name: solver_option option -> string = function
  | Some x -> "-" ^ x.name
  | None -> ""

let solver_uid (solver: solver_config) = solver.name ^ "-" ^ solver.version

let decompositions_name decompositions =
  match decompositions with
  | [] -> ""
  | x::l -> List.fold_left (fun x y -> x ^ "_" ^ y.name) ("-" ^ x.name) l

let create_solver_dir bench =
  match bench.solver_instance with
  | `AbsoluteSolver abs -> "absolute-" ^ abs.version
  | `FznSolver fzn -> solver_uid fzn.solver
  | `MznSolver mzn -> solver_uid mzn.solver
  | `StandaloneSolver standalone -> solver_uid standalone.solver

let create_result_filename bench =
  match bench.solver_instance with
    | `AbsoluteSolver abs ->
        abs.domain ^ "-" ^ abs.strategy ^ (decompositions_name abs.decompositions)
    | `FznSolver fzn -> "box-" ^ fzn.strategy.short ^ (decompositions_name fzn.decompositions) ^ (option_name fzn.option)
    | `MznSolver mzn ->
        (Filename.remove_extension (Filename.basename mzn.model)) ^ "-" ^ mzn.strategy.short
        ^ (option_name mzn.option)
    | `StandaloneSolver standalone -> "standalone" ^ (option_name standalone.option)

let copy_optimum_files benchmark bench =
  let optimum_path = System.concat_dir bench.problem_set_path "optimum" in
  let source = System.concat_dir benchmark.input_dir optimum_path in
  let target = System.concat_dir benchmark.output_dir bench.problem_set_path in
  let _ = System.call_command ("cp -r " ^ source ^ " " ^ target) in
  ()

let register_bench benchmark bench =
  let solver_dir = create_solver_dir bench in
  let path = List.fold_left System.concat_dir benchmark.output_dir
    [bench.problem_set_path; solver_dir] in
  let _ = System.call_command ("mkdir -p " ^ path) in
  let _ = copy_optimum_files benchmark bench in
  let result_filename = create_result_filename bench in
  let bench_instance_file = System.concat_dir path (result_filename ^ ".json") in
  let bench = finalize_bench benchmark bench in
  let data = Yojson.Safe.prettify (string_of_bench_instance bench) in
  System.string_to_file bench_instance_file data;
  let bench_instance_output = System.concat_dir path (result_filename ^ ".csv") in
  Printf.printf "%s %s %s\n" benchmark.bench_exec bench_instance_file bench_instance_output

let config_from_json json_data =
  try
    benchmark_of_string json_data
  with
  | Atdgen_runtime__Oj_run.Error(msg)
  | Yojson.Json_error(msg) ->
      System.eprintf_and_exit (Printf.sprintf
        "The benchmarks description file contains an error:\n\n\
         %s\n\n\
        [help] Be careful to the case: \"int\" is not the same as \"Int\".\n\
        [help] You can find a full example of the JSON format in benchmark/data/benchmarks.json." msg)

let () =
  (* Printexc.record_backtrace true; *)
  let benchmark = config_from_json (System.get_bench_desc ()) in
  let benches = gen_benches benchmark in
  Printf.printf "%d bench files generated.\n" (List.length benches);
  List.iter (register_bench benchmark) benches
