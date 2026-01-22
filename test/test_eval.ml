open Core
open Ltl_weaken

let%expect_test "eval true on any state" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let result = Eval.eval lasso 0 (Ltl.Any Ltl.PTrue) in
  printf "eval(T) = %b\n" result;
  [%expect {| eval(T) = true |}]

let%expect_test "eval false on any state" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let result = Eval.eval lasso 0 (Ltl.Any Ltl.PFalse) in
  printf "eval(F) = %b\n" result;
  [%expect {| eval(F) = false |}]

let%expect_test "eval conjunction (true && true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let conj = Parser.Smv.parse_formula "p & q" in
  let result = Eval.eval lasso 0 conj in
  printf "eval(p & q) = %b\n" result;
  [%expect {| eval(p & q) = true |}]

let%expect_test "eval conjunction (true && false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let conj = Parser.Smv.parse_formula "p & q" in
  let result = Eval.eval lasso 0 conj in
  printf "eval(p & q) = %b\n" result;
  [%expect {| eval(p & q) = false |}]

let%expect_test "eval disjunction (true || false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let disj = Parser.Smv.parse_formula "p | q" in
  let result = Eval.eval lasso 0 disj in
  printf "eval(p | q) = %b\n" result;
  [%expect {| eval(p | q) = true |}]

let%expect_test "eval negation (not true)" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let neg = Parser.Smv.parse_formula "!p" in
  let result = Eval.eval lasso 0 neg in
  printf "eval(!p) = %b\n" result;
  [%expect {| eval(!p) = false |}]

let%expect_test "eval negation (not false)" =
  let states = [ [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let neg = Parser.Smv.parse_formula "!p" in
  let result = Eval.eval lasso 0 neg in
  printf "eval(!p) = %b\n" result;
  [%expect {| eval(!p) = true |}]

let%expect_test "eval implication (true -> true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let impl = Parser.Smv.parse_formula "p -> q" in
  let result = Eval.eval lasso 0 impl in
  printf "eval(p -> q) = %b\n" result;
  [%expect {| eval(p -> q) = true |}]

let%expect_test "eval implication (true -> false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let impl = Parser.Smv.parse_formula "p -> q" in
  let result = Eval.eval lasso 0 impl in
  printf "eval(p -> q) = %b\n" result;
  [%expect {| eval(p -> q) = false |}]

let%expect_test "eval iff (true <-> true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let iff = Parser.Smv.parse_formula "p <-> q" in
  let result = Eval.eval lasso 0 iff in
  printf "eval(p <-> q) = %b\n" result;
  [%expect {| eval(p <-> q) = true |}]

let%expect_test "eval iff (true <-> false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let iff = Parser.Smv.parse_formula "p <-> q" in
  let result = Eval.eval lasso 0 iff in
  printf "eval(p <-> q) = %b\n" result;
  [%expect {| eval(p <-> q) = false |}]

let%expect_test "eval X" =
  let states = [ [ ("p", true) ]; [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let term = Parser.Smv.parse_formula "X p" in
  let result = Eval.eval lasso 0 term in
  printf "eval(X (p)) = %b\n" result;
  [%expect {| eval(X (p)) = false |}];
  let result = Eval.eval lasso 1 term in
  printf "eval(X (p)) = %b\n" result;
  [%expect {| eval(X (p)) = true |}];
  let term = Parser.Smv.parse_formula "X (!p)" in
  let result = Eval.eval lasso 0 term in
  printf "eval(X (!p)) = %b\n" result;
  [%expect {| eval(X (!p)) = true |}];
  let term = Parser.Smv.parse_formula "!(X p)" in
  let result = Eval.eval lasso 0 term in
  printf "eval(!(X (p))) = %b\n" result;
  [%expect {| eval(!(X (p))) = true |}]

let%expect_test "eval G" =
  let states = [ [ ("p", true) ]; [ ("p", true) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "G p" in
  let result = Eval.eval lasso 0 term in
  printf "eval(G p) from index 0 = %b\n" result;
  [%expect {| eval(G p) from index 0 = true |}]

let%expect_test "eval G false" =
  let states = [ [ ("p", true) ]; [ ("p", false) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "G p" in
  let result = Eval.eval lasso 0 term in
  printf "eval(G p) from index 0 = %b\n" result;
  [%expect {| eval(G p) from index 0 = false |}]

let%expect_test "eval G check from middle of lasso" =
  let states = [ [ ("p", false) ]; [ ("p", true) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "G p" in
  let result = Eval.eval lasso 1 term in
  printf "eval(G p) from index 1 = %b\n" result;
  [%expect {| eval(G p) from index 1 = true |}]

let%expect_test "eval G with conjunction" =
  let states =
    [
      [ ("p", true); ("q", true) ];
      [ ("p", true); ("q", false) ];
      [ ("p", true); ("q", true) ];
    ]
  in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "G (p && q)" in
  let result = Eval.eval lasso 0 term in
  printf "eval(G (p && q)) = %b\n" result;
  [%expect {| eval(G (p && q)) = false |}]

let%expect_test "eval F" =
  let states = [ [ ("p", false) ]; [ ("p", false) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "F p" in
  let result = Eval.eval lasso 0 term in
  printf "eval(F p) from index 0 = %b\n" result;
  [%expect {| eval(F p) from index 0 = true |}]

let%expect_test "eval F false" =
  let states = [ [ ("p", true) ]; [ ("p", false) ]; [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 1 in
  let term = Parser.Smv.parse_formula "F p" in
  let result = Eval.eval lasso 0 term in
  printf "eval(F p) from index 0 = %b\n" result;
  [%expect {| eval(F p) from index 0 = false |}];
  let result = Eval.eval lasso 1 term in
  printf "eval(F p) from index 1 = %b\n" result;
  [%expect {| eval(F p) from index 1 = false |}];
  let result = Eval.eval lasso 2 term in
  printf "eval(F p) from index 2 = %b\n" result;
  [%expect {| eval(F p) from index 2 = false |}]
