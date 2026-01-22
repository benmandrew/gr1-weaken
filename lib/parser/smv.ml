open Core

(** Parse SMV propositional formulae into Why3 terms *)

(** Parse a formula string in SMV syntax *)
let parse_formula (input : string) : Ltl.any_formula =
  let lexbuf = Lexing.from_string input in
  try Smv_parser.formula Smv_lexer.token lexbuf with
  | Smv_parser.Error ->
      let pos = lexbuf.lex_curr_pos in
      let msg =
        sprintf "Parse error at position %d: '%s'" pos
          (String.sub input ~pos:0 ~len:(min (String.length input) (pos + 10)))
      in
      invalid_arg msg
  | e -> invalid_arg (sprintf "Parsing exception: %s" (Exn.to_string e))

(** Parse multiple formulae from a single string (comma or semicolon separated)
*)
let parse_formulae (input : string) : Ltl.any_formula list =
  String.split_on_chars input ~on:[ ','; ';' ]
  |> List.map ~f:String.strip
  |> List.filter ~f:(fun s -> not (String.is_empty s))
  |> List.map ~f:parse_formula

(** Parse an identifier from a variable reference *)
let parse_ident (input : string) : string =
  let lexbuf = Lexing.from_string (String.strip input) in
  match Smv_lexer.token lexbuf with
  | Smv_parser.IDENT id -> id
  | _ -> invalid_arg (sprintf "Expected identifier, got: %s" input)
