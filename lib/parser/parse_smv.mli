(** Parse SMV files and extract LTL specifications *)

val extract_ltlspecs : string -> string list
(** Extract LTLSPEC formulae strings from an SMV file content *)

val parse_smv_file : string -> Ltl.any_formula list
(** Read an SMV file and parse all LTLSPEC lines as LTL formulae *)

val parse_smv_string : string -> Ltl.any_formula list
(** Parse LTLSPEC lines from SMV content (as a string) *)
