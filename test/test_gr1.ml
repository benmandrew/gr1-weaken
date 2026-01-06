open Core
open Why3
open Gr1_weaken

(* Helper to create a simple variable *)
let make_var name =
  let id = Ident.id_fresh name in
  let ty = Ty.ty_bool in
  Term.create_vsymbol id ty

let%expect_test "boolean constants" =
  print_endline (Gr1.term_to_promela Term.t_true);
  [%expect {| true |}];
  print_endline (Gr1.term_to_promela Term.t_false);
  [%expect {| false |}]

let%expect_test "variables" =
  let var_x = make_var "x" in
  let term_x = Term.t_var var_x in
  print_endline (Gr1.term_to_promela term_x);
  [%expect {| x |}];
  let var_state = make_var "state" in
  let term_state = Term.t_var var_state in
  print_endline (Gr1.term_to_promela term_state);
  [%expect {| state |}]

let%expect_test "logical operators - AND" =
  let and_term = Term.t_and Term.t_true Term.t_false in
  print_endline (Gr1.term_to_promela and_term);
  [%expect {| (true && false) |}]

let%expect_test "logical operators - OR" =
  let or_term = Term.t_or Term.t_true Term.t_false in
  print_endline (Gr1.term_to_promela or_term);
  [%expect {| (true || false) |}]

let%expect_test "logical operators - NOT" =
  let not_term = Term.t_not Term.t_true in
  print_endline (Gr1.term_to_promela not_term);
  [%expect {| !(true) |}]

let%expect_test "logical operators - IMPLIES" =
  let implies_term = Term.t_implies Term.t_true Term.t_false in
  print_endline (Gr1.term_to_promela implies_term);
  [%expect {| (!(true) || false) |}]

let%expect_test "logical operators - IFF" =
  let iff_term = Term.t_iff Term.t_true Term.t_false in
  print_endline (Gr1.term_to_promela iff_term);
  [%expect {| ((true) == (false)) |}]

let%expect_test "nested expressions" =
  (* (true && false) || true *)
  let nested = Term.t_or (Term.t_and Term.t_true Term.t_false) Term.t_true in
  print_endline (Gr1.term_to_promela nested);
  [%expect {| ((true && false) || true) |}];
  (* !(true && false) *)
  let nested2 = Term.t_not (Term.t_and Term.t_true Term.t_false) in
  print_endline (Gr1.term_to_promela nested2);
  [%expect {| !((true && false)) |}]

let%expect_test "GR(1) to Promela conversion" =
  let spec =
    Gr1.make ~asm_init:Term.t_true
      ~asm_safety:[ Term.t_true; Term.t_false ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_false
      ~gnt_safety:[ Term.t_true ]
      ~gnt_liveness:[ Term.t_false; Term.t_true ]
  in
  print_endline (Gr1.to_promela spec);
  [%expect
    {|
    /* GR(1) Specification in Promela LTL */

    /* Assumptions */
    asm_init: true
    asm_safety: true && false
    asm_liveness: true

    /* Guarantees */
    gnt_init: false
    gnt_safety: true
    gnt_liveness: false && true |}]

let%expect_test "GR(1) to LTL formula" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in
  print_endline (Gr1.to_promela_ltl spec);
  [%expect
    {| (true && [] (true) && ([] <> (true))) -> (true && [] (true) && ([] <> (true))) |}]

let%expect_test "empty lists in GR(1) spec" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[] ~asm_liveness:[]
      ~gnt_init:Term.t_true ~gnt_safety:[] ~gnt_liveness:[]
  in
  print_endline (Gr1.to_promela spec);
  [%expect
    {|
    /* GR(1) Specification in Promela LTL */

    /* Assumptions */
    asm_init: true

    /* Guarantees */
    gnt_init: true |}];
  print_endline (Gr1.to_promela_ltl spec);
  [%expect {| (true) -> (true) |}]

let%expect_test "single element lists" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ] ~asm_liveness:[]
      ~gnt_init:Term.t_false ~gnt_safety:[] ~gnt_liveness:[ Term.t_true ]
  in
  print_endline (Gr1.to_promela spec);
  [%expect
    {|
    /* GR(1) Specification in Promela LTL */

    /* Assumptions */
    asm_init: true
    asm_safety: true

    /* Guarantees */
    gnt_init: false
    gnt_liveness: true |}]

let%expect_test "complex GR(1) formula" =
  (* (true && false) || (false && true) *)
  let safety1 =
    Term.t_or
      (Term.t_and Term.t_true Term.t_false)
      (Term.t_and Term.t_false Term.t_true)
  in
  (* true -> false *)
  let liveness1 = Term.t_implies Term.t_true Term.t_false in
  (* Create a deeply nested formula *)
  let safety2 =
    Term.t_not (Term.t_or (Term.t_and Term.t_true Term.t_false) Term.t_true)
  in
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ safety1; safety2 ]
      ~asm_liveness:[ liveness1 ] ~gnt_init:Term.t_false
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in
  print_endline (Gr1.to_promela spec);
  [%expect
    {|
    /* GR(1) Specification in Promela LTL */

    /* Assumptions */
    asm_init: true
    asm_safety: ((true && false) || (false && true)) && !(((true && false) || true))
    asm_liveness: (!(true) || false)

    /* Guarantees */
    gnt_init: false
    gnt_safety: true
    gnt_liveness: true |}]

let%expect_test "temporal operator structure" =
  let spec =
    Gr1.make ~asm_init:Term.t_true
      ~asm_safety:[ Term.t_true; Term.t_false ]
      ~asm_liveness:[ Term.t_true; Term.t_false ]
      ~gnt_init:Term.t_true ~gnt_safety:[ Term.t_true ]
      ~gnt_liveness:[ Term.t_true; Term.t_false ]
  in
  print_endline (Gr1.to_promela_ltl spec);
  [%expect
    {| (true && [] ((true && false)) && ([] <> (true) && [] <> (false))) -> (true && [] (true) && ([] <> (true) && [] <> (false))) |}]

let%expect_test "output format consistency" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in
  (* Test that output is not empty and has proper structure *)
  let promela_result = Gr1.to_promela spec in
  print_s [%sexp (String.length promela_result > 0 : bool)];
  [%expect {| true |}];
  let ltl_result = Gr1.to_promela_ltl spec in
  print_s [%sexp (String.length ltl_result > 0 : bool)];
  [%expect {| true |}];
  (* Test that ltl result has implication arrow *)
  print_s [%sexp (String.is_substring ltl_result ~substring:"->" : bool)];
  [%expect {| true |}]
