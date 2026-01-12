open Core
open Gr1_weaken
open Why3

let create_prop name =
  let ps =
    match Hashtbl.find Lasso.lsymbol_cache name with
    | Some ps -> ps
    | None ->
        let ps = Term.create_psymbol (Ident.id_fresh name) [] in
        Hashtbl.set Lasso.lsymbol_cache ~key:name ~data:ps;
        ps
  in
  Term.ps_app ps []

let%expect_test "eval true on any state" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let result = Eval.eval lasso 0 Term.t_true in
  printf "eval(T) = %b\n" result;
  [%expect {| eval(T) = true |}]

let%expect_test "eval false on any state" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let result = Eval.eval lasso 0 Term.t_false in
  printf "eval(F) = %b\n" result;
  [%expect {| eval(F) = false |}]

let%expect_test "eval conjunction (true && true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let conj = Term.t_and p q in
  let result = Eval.eval lasso 0 conj in
  printf "eval(p && q) = %b\n" result;
  [%expect {| eval(p && q) = true |}]

let%expect_test "eval conjunction (true && false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let conj = Term.t_and p q in
  let result = Eval.eval lasso 0 conj in
  printf "eval(p && q) = %b\n" result;
  [%expect {| eval(p && q) = false |}]

let%expect_test "eval disjunction (true || false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let disj = Term.t_or p q in
  let result = Eval.eval lasso 0 disj in
  printf "eval(p || q) = %b\n" result;
  [%expect {| eval(p || q) = true |}]

let%expect_test "eval negation (not true)" =
  let states = [ [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let neg = Term.t_not p in
  let result = Eval.eval lasso 0 neg in
  printf "eval(not p) = %b\n" result;
  [%expect {| eval(not p) = false |}]

let%expect_test "eval negation (not false)" =
  let states = [ [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let neg = Term.t_not p in
  let result = Eval.eval lasso 0 neg in
  printf "eval(not p) = %b\n" result;
  [%expect {| eval(not p) = true |}]

let%expect_test "eval implication (true -> true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let impl = Term.t_implies p q in
  let result = Eval.eval lasso 0 impl in
  printf "eval(p -> q) = %b\n" result;
  [%expect {| eval(p -> q) = true |}]

let%expect_test "eval implication (true -> false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let impl = Term.t_implies p q in
  let result = Eval.eval lasso 0 impl in
  printf "eval(p -> q) = %b\n" result;
  [%expect {| eval(p -> q) = false |}]

let%expect_test "eval iff (true <-> true)" =
  let states = [ [ ("p", true); ("q", true) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let iff = Term.t_iff p q in
  let result = Eval.eval lasso 0 iff in
  printf "eval(p <-> q) = %b\n" result;
  [%expect {| eval(p <-> q) = true |}]

let%expect_test "eval iff (true <-> false)" =
  let states = [ [ ("p", true); ("q", false) ] ] in
  let lasso = Lasso.of_states states 0 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let iff = Term.t_iff p q in
  let result = Eval.eval lasso 0 iff in
  printf "eval(p <-> q) = %b\n" result;
  [%expect {| eval(p <-> q) = false |}]

let%expect_test "eval_safety: all states satisfy property" =
  let states = [ [ ("p", true) ]; [ ("p", true) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let result = Eval.eval_safety lasso 0 p in
  printf "eval_safety(p) from index 0 = %b\n" result;
  [%expect {| eval_safety(p) from index 0 = true |}]

let%expect_test "eval_safety: one state violates property" =
  let states = [ [ ("p", true) ]; [ ("p", false) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let result = Eval.eval_safety lasso 0 p in
  printf "eval_safety(p) from index 0 = %b\n" result;
  [%expect {| eval_safety(p) from index 0 = false |}]

let%expect_test "eval_safety: check from middle of lasso" =
  let states = [ [ ("p", false) ]; [ ("p", true) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let result = Eval.eval_safety lasso 1 p in
  printf "eval_safety(p) from index 1 = %b\n" result;
  [%expect {| eval_safety(p) from index 1 = true |}]

let%expect_test "eval_safety with conjunction" =
  let states =
    [
      [ ("p", true); ("q", true) ];
      [ ("p", true); ("q", false) ];
      [ ("p", true); ("q", true) ];
    ]
  in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let q = create_prop "q" in
  let conj = Term.t_and p q in
  let result = Eval.eval_safety lasso 0 conj in
  printf "eval_safety(p && q) = %b\n" result;
  [%expect {| eval_safety(p && q) = false |}]

let%expect_test "eval_liveness: some state satisfies property" =
  let states = [ [ ("p", false) ]; [ ("p", false) ]; [ ("p", true) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let result = Eval.eval_liveness lasso 0 p in
  printf "eval_liveness(p) from index 0 = %b\n" result;
  [%expect {| eval_liveness(p) from index 0 = true |}]

let%expect_test "eval_liveness: no loop state satisfies property" =
  let states = [ [ ("p", true) ]; [ ("p", false) ]; [ ("p", false) ] ] in
  let lasso = Lasso.of_states states 1 in
  let p = create_prop "p" in
  let result = Eval.eval_liveness lasso 0 p in
  printf "eval_liveness(p) from index 0 = %b\n" result;
  [%expect {| eval_liveness(p) from index 0 = false |}];
  let result = Eval.eval_liveness lasso 1 p in
  printf "eval_liveness(p) from index 1 = %b\n" result;
  [%expect {| eval_liveness(p) from index 1 = false |}];
  let result = Eval.eval_liveness lasso 2 p in
  printf "eval_liveness(p) from index 2 = %b\n" result;
  [%expect {| eval_liveness(p) from index 2 = false |}]
