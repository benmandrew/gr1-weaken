open Core
open Why3
open Ltl_weaken

(* Helper to create a simple variable *)
let make_var name =
  let id = Ident.id_fresh name in
  let ty = Ty.ty_bool in
  Term.create_vsymbol id ty

(* Helper to create an atomic proposition (0-arity predicate) *)
let make_prop name =
  let id = Ident.id_fresh name in
  Term.create_psymbol id []

(* Helper to create a proposition term from a predicate symbol *)
let prop_term ps = Term.ps_app ps []

let%expect_test "boolean constants" =
  print_endline (Print.term_to_smv Term.t_true);
  [%expect {| TRUE |}];
  print_endline (Print.term_to_smv Term.t_false);
  [%expect {| FALSE |}]

let%expect_test "variables" =
  let var_x = make_var "x" in
  let term_x = Term.t_var var_x in
  print_endline (Print.term_to_smv term_x);
  [%expect {| x |}];
  let var_state = make_var "state" in
  let term_state = Term.t_var var_state in
  print_endline (Print.term_to_smv term_state);
  [%expect {| state |}]

(* Tests with atomic propositions *)

let%expect_test "atomic propositions" =
  let p = make_prop "p" in
  let q = make_prop "q" in
  let p_term = prop_term p in
  let q_term = prop_term q in
  print_endline (Print.term_to_smv p_term);
  [%expect {| p |}];
  print_endline (Print.term_to_smv q_term);
  [%expect {| q |}]

let%expect_test "propositions with AND" =
  let p = make_prop "ready" in
  let q = make_prop "enabled" in
  let formula = Term.t_and (prop_term p) (prop_term q) in
  print_endline (Print.term_to_smv formula);
  [%expect {| (ready & enabled) |}]

let%expect_test "propositions with OR" =
  let p = make_prop "req" in
  let q = make_prop "ack" in
  let formula = Term.t_or (prop_term p) (prop_term q) in
  print_endline (Print.term_to_smv formula);
  [%expect {| (req | ack) |}]

let%expect_test "propositions with NOT" =
  let p = make_prop "busy" in
  let formula = Term.t_not (prop_term p) in
  print_endline (Print.term_to_smv formula);
  [%expect {| !busy |}]

let%expect_test "propositions with IMPLIES" =
  let p = make_prop "request" in
  let q = make_prop "grant" in
  let formula = Term.t_implies (prop_term p) (prop_term q) in
  print_endline (Print.term_to_smv formula);
  [%expect {| (!request | grant) |}]

let%expect_test "propositions with IFF" =
  let p = make_prop "ready" in
  let q = make_prop "done" in
  let formula = Term.t_iff (prop_term p) (prop_term q) in
  print_endline (Print.term_to_smv formula);
  [%expect {| (ready <-> done) |}]

let%expect_test "complex proposition formula" =
  let p = make_prop "p" in
  let q = make_prop "q" in
  let r = make_prop "r" in
  (* (p && q) || (!p && r) *)
  let formula =
    Term.t_or
      (Term.t_and (prop_term p) (prop_term q))
      (Term.t_and (Term.t_not (prop_term p)) (prop_term r))
  in
  print_endline (Print.term_to_smv formula);
  [%expect {| ((p & q) | (!p & r)) |}]
