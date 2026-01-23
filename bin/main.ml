open Core
open Ltl_weaken

let smv_file =
  let doc = "Path to SMV model file" in
  Cmdliner.Arg.(required & opt (some non_dir_file) None & info [ "smv" ] ~doc)

let check_smv smv_path =
  let spec = Parser.Parse_smv.parse_smv_file smv_path in
  match Nuxmv.check smv_path spec with
  | Nuxmv.Valid -> printf "Specification is VALID\n"
  | Nuxmv.Invalid cexs ->
      printf "Specification is INVALID\n";
      List.iter ~f:(fun p -> Eval.eval (List.hd_exn cexs) 0 p |> ignore) spec;
      Lasso.print (List.hd_exn cexs)
  | Nuxmv.Error msg -> printf "Error during verification:\n%s\n" msg

let check_cmd =
  let doc = "Weaken an LTL specification to hold on a given SMV model" in
  let man = [ `S Cmdliner.Manpage.s_description; `P (doc ^ ".") ] in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "ltl-weaken" ~doc ~man)
    Cmdliner.Term.(const check_smv $ smv_file)

let () =
  let cmd = check_cmd in
  Cmdliner.Cmd.eval cmd |> exit
