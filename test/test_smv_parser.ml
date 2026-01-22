open Core
open Ltl_weaken

let%expect_test "SMV parse simple proposition" =
  let term = Parser.Smv.parse_formula "p" in
  print_endline (Ltl.to_string_any term);
  [%expect {| p |}]

let%expect_test "SMV parse negation" =
  let term = Parser.Smv.parse_formula "!p" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (!p) |}]

let%expect_test "SMV parse conjunction" =
  let term = Parser.Smv.parse_formula "p & q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p & q) |}]

let%expect_test "SMV parse conjunction with &&" =
  let term = Parser.Smv.parse_formula "p && q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p & q) |}]

let%expect_test "SMV parse disjunction" =
  let term = Parser.Smv.parse_formula "p | q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p | q) |}]

let%expect_test "SMV parse disjunction with ||" =
  let term = Parser.Smv.parse_formula "p || q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p | q) |}]

let%expect_test "SMV parse implication" =
  let term = Parser.Smv.parse_formula "p -> q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p -> q) |}]

let%expect_test "SMV parse biconditional" =
  let term = Parser.Smv.parse_formula "p <-> q" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (p <-> q) |}]

let%expect_test "SMV parse complex formula" =
  let term = Parser.Smv.parse_formula "(p & q) | (!r)" in
  print_endline (Ltl.to_string_any term);
  [%expect {| ((p & q) | (!r)) |}]

let%expect_test "SMV parse nested parentheses" =
  let term = Parser.Smv.parse_formula "((p & q) | r) & s" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (((p & q) | r) & s) |}]

let%expect_test "SMV parse multiple formulae" =
  let terms = Parser.Smv.parse_formulae "p & q, !r | s" in
  printf "%d formulae:\n" (List.length terms);
  List.iter terms ~f:(fun t -> printf "  %s\n" (Ltl.to_string_any t));
  [%expect {|
    2 formulae:
      (p & q)
      ((!r) | s)
    |}]

let%expect_test "SMV parse next operator" =
  let term = Parser.Smv.parse_formula "X (p & q)" in
  print_endline (Ltl.to_string_any term);
  [%expect {| (X (p & q)) |}]

let%expect_test "SMV parse identifier" =
  let id = Parser.Smv.parse_ident "my_variable" in
  printf "%s\n" id;
  [%expect {| my_variable |}]

let%expect_test "SMV parse constants" =
  let t_true = Parser.Smv.parse_formula "TRUE" in
  let t_false = Parser.Smv.parse_formula "FALSE" in
  printf "TRUE => %s\n" (Ltl.to_string_any t_true);
  printf "FALSE => %s\n" (Ltl.to_string_any t_false);
  [%expect {|
    TRUE => TRUE
    FALSE => FALSE
    |}]
