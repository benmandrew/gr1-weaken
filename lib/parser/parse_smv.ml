open Core

(** Parse SMV files and extract LTL specifications *)

(** Extract LTLSPEC formulae from an SMV file *)
let extract_ltlspecs (content : string) : string list =
  let lines = String.split_lines content in
  List.filter_map lines ~f:(fun line ->
      let trimmed = String.strip line in
      (* Match lines that start with LTLSPEC (case-insensitive) *)
      if String.is_prefix trimmed ~prefix:"LTLSPEC" then
        (* Extract the formula part after "LTLSPEC" *)
        let after_ltlspec =
          String.drop_prefix trimmed (String.length "LTLSPEC") |> String.strip
        in
        (* Remove trailing semicolon if present *)
        let formula =
          if String.is_suffix after_ltlspec ~suffix:";" then
            String.drop_suffix after_ltlspec 1
          else after_ltlspec
        in
        if String.is_empty formula then None else Some formula
      else None)

(** Read an SMV file and parse all LTLSPEC lines as LTL formulae *)
let parse_smv_file (filename : string) : Ltl.any_formula list =
  let content = In_channel.read_all filename in
  let spec_strings = extract_ltlspecs content in
  List.map spec_strings ~f:Parse_ltl.parse_formula

(** Parse LTLSPEC lines from SMV content (as a string) *)
let parse_smv_string (content : string) : Ltl.any_formula list =
  let spec_strings = extract_ltlspecs content in
  List.map spec_strings ~f:Parse_ltl.parse_formula
