open Core
open Gr1_weaken

(* Command-line interface using cmdliner *)
let tlsf_file =
  let doc = "Path to TLSF GR(1) specification file" in
  Cmdliner.Arg.(required & opt (some non_dir_file) None & info [ "tlsf" ] ~doc)

let smv_file =
  let doc = "Path to SMV model file" in
  Cmdliner.Arg.(required & opt (some non_dir_file) None & info [ "smv" ] ~doc)

let check_tlsf tlsf_path smv_path =
  let content = In_channel.read_all tlsf_path in
  let spec = Parser.Tlsf.parse_gr1 content in
  match Nuxmv.check smv_path spec with
  | Nuxmv.Valid -> printf "Specification is VALID\n"
  | Nuxmv.Invalid xml ->
      printf "Specification is INVALID\n";
      let lasso = Cex.parse xml in
      Gr1.eval spec lasso;
      Lasso.print lasso
  | Nuxmv.Error msg -> printf "Error during verification:\n%s\n" msg

let check_cmd =
  let doc = "Weaken a GR(1) specification to hold on a given SMV model" in
  let man = [ `S Cmdliner.Manpage.s_description; `P (doc ^ ".") ] in
  Cmdliner.Cmd.v
    (Cmdliner.Cmd.info "gr1-weaken" ~doc ~man)
    Cmdliner.Term.(const check_tlsf $ tlsf_file $ smv_file)

let () =
  let cmd = check_cmd in
  Cmdliner.Cmd.eval cmd |> exit
