open Core
open Why3

type check_result = Valid | Invalid of string | Error of string

let model_filename = "model.smv"
let trace_filename = "trace.xml"
let commands_filename = "commands.txt"

let commands =
  [
    "set on_failure_script_quits 1";
    "set counter_examples 1";
    "go";
    "check_ltlspec";
    "show_traces -p 4 -o \"" ^ trace_filename ^ "\"";
    "quit";
  ]

let get_tmp_dir () =
  let base = Stdlib.Filename.get_temp_dir_name () in
  let dir =
    Filename.concat base
      (sprintf "gr1_%d_%d"
         (Pid.to_int (Core_unix.getpid ()))
         (Random.int 1000000))
  in
  Core_unix.mkdir dir ~perm:0o700;
  dir

let collect_vars term =
  let rec aux acc t =
    match t.Term.t_node with
    | Tvar vs -> Set.add acc vs.vs_name.Ident.id_string
    | Tconst _ -> acc
    | Tapp (fs, args) ->
        let acc' =
          if List.is_empty args then Set.add acc fs.ls_name.Ident.id_string
          else acc
        in
        List.fold args ~init:acc' ~f:aux
    | Tif (c, a, b) -> aux (aux (aux acc c) a) b
    | Tlet (t1, tb) ->
        let _, body = Term.t_open_bound tb in
        aux (aux acc t1) body
    | Tcase (scrut, branches) ->
        let acc' = aux acc scrut in
        List.fold branches ~init:acc' ~f:(fun s br ->
            let _, body = Term.t_open_branch br in
            aux s body)
    | Tquant (_, tq) ->
        let _, _, body = Term.t_open_quant tq in
        aux acc body
    | Tbinop (_, a, b) -> aux (aux acc a) b
    | Tnot t -> aux acc t
    | Ttrue | Tfalse | Teps _ -> acc
  in
  aux (Set.empty (module String)) term

let write_commands ~tmp_dir =
  let buf = Buffer.create 512 in
  List.iter commands ~f:(fun cmd ->
      Buffer.(
        add_string buf cmd;
        add_string buf "\n"));
  Out_channel.write_all
    (Filename.concat tmp_dir commands_filename)
    ~data:(Buffer.contents buf)

let write_model ~tmp_dir (spec : Gr1.t) =
  let all_terms =
    (Gr1.asm_init spec :: Gr1.gnt_init spec :: Gr1.asm_safety spec)
    @ Gr1.asm_liveness spec @ Gr1.gnt_safety spec @ Gr1.gnt_liveness spec
  in
  let vars =
    List.fold all_terms
      ~init:(Set.empty (module String))
      ~f:(fun acc term -> collect_vars term |> Set.union acc)
    |> Set.to_list
  in
  let buf = Buffer.create 512 in
  Buffer.add_string buf "MODULE main\n";
  Buffer.add_string buf "VAR\n";
  List.iter vars ~f:(fun v ->
      Buffer.add_string buf (sprintf "  %s : boolean;\n" v));
  Buffer.add_string buf "\nLTLSPEC ";
  Buffer.add_string buf (Gr1.to_smv_ltl spec);
  Buffer.add_char buf '\n';
  Out_channel.write_all
    (Filename.concat tmp_dir model_filename)
    ~data:(Buffer.contents buf)

let read_file_if_exists path =
  try Some (In_channel.read_all path) with Sys_error _ -> None

let environment = Core_unix.environment ()

let run_nuxmv ~tmp_dir =
  let cmd =
    sprintf "cd %s && nuXmv -source %s %s" tmp_dir commands_filename
      model_filename
  in
  try
    let process = Core_unix.open_process_full cmd ~env:environment in
    let stdout = In_channel.input_all process.stdout in
    let stderr = In_channel.input_all process.stderr in
    let status = Core_unix.close_process_full process in
    match status with
    | Ok () -> Ok (stdout, stderr)
    | Error (`Exit_non_zero n) -> Error (n, stdout, stderr)
    | Error (`Signal _) -> raise (Failure "nuXmv terminated by signal")
  with e ->
    Error (127, "", sprintf "Failed to run nuXmv: %s" (Exn.to_string e))

let check spec =
  let tmp_dir = get_tmp_dir () in
  write_commands ~tmp_dir;
  write_model ~tmp_dir spec;
  match run_nuxmv ~tmp_dir with
  | Ok (out, _err) -> (
      if
        String.is_substring out ~substring:"-- specification"
        && String.is_substring out ~substring:"is true"
      then Valid
      else
        match read_file_if_exists (Filename.concat tmp_dir trace_filename) with
        | Some xml -> Invalid xml
        | None -> Invalid out)
  | Error (status, out, err) ->
      Error
        (sprintf "nuXmv failed (exit %d). stdout:\n%s\nstderr:\n%s" status out
           err)
