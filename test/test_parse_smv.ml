open Core
open Ltl_weaken

let%expect_test "parse LTLSPEC formulae" =
  let content =
    {|LTLSPEC !been_picking & !been_putting & !ended_f;
      LTLSPEC G (X been_picking <-> (success | (been_picking & !reset)));
      LTLSPEC G (X been_putting <-> (putsuccess | (been_putting & !reset)));
      LTLSPEC G (X ended_f <-> (ended | (ended_f & !reset)));
      LTLSPEC G (ended_f -> F (been_picking & been_putting));|}
  in
  let formulas = Parser.Parse_smv.parse_smv_string content in
  printf "Parsed %d formulas\n" (List.length formulas);
  List.iter formulas ~f:(fun formula ->
      printf "- %s\n" (Ltl.to_string_any formula));
  [%expect
    {|
    Parsed 5 formulas
    - (((!been_picking) & (!been_putting)) & (!ended_f))
    - (G ((X been_picking) <-> (success | (been_picking & (!reset)))))
    - (G ((X been_putting) <-> (putsuccess | (been_putting & (!reset)))))
    - (G ((X ended_f) <-> (ended | (ended_f & (!reset)))))
    - (G (ended_f -> (F (been_picking & been_putting))))
    |}]

let%expect_test "parse SMV content as string" =
  let content =
    "LTLSPEC G p;\nLTLSPEC F q\nLTLSPEC p U q;\n-- comment\nLTLSPEC X r"
  in
  let formulas = Parser.Parse_smv.parse_smv_string content in
  printf "Parsed %d formulas from string\n" (List.length formulas);
  List.iter formulas ~f:(fun formula ->
      printf "- %s\n" (Ltl.to_string_any formula));
  [%expect
    {|
    Parsed 4 formulas from string
    - (G p)
    - (F q)
    - (p U q)
    - (X r)
    |}]

let%expect_test "extract LTLSPEC with semicolons" =
  let content = "LTLSPEC G p;\nLTLSPEC F q\nLTLSPEC p U q;" in
  let specs = Parser.Parse_smv.extract_ltlspecs content in
  List.iter specs ~f:(fun spec -> printf "%s\n" spec);
  [%expect {|
    G p
    F q
    p U q
    |}]

let%expect_test "ignore non-LTLSPEC lines" =
  let content =
    "VAR x : boolean;\nASSIGN init(x) := TRUE;\nLTLSPEC G x;\nDEFINE y := x;"
  in
  let specs = Parser.Parse_smv.extract_ltlspecs content in
  printf "Found %d specs\n" (List.length specs);
  List.iter specs ~f:(fun spec -> printf "%s\n" spec);
  [%expect {|
    Found 1 specs
    G x
    |}]

let%expect_test "empty file has no LTLSPEC" =
  let content = "" in
  let specs = Parser.Parse_smv.extract_ltlspecs content in
  printf "Found %d specs\n" (List.length specs);
  [%expect {| Found 0 specs |}]
