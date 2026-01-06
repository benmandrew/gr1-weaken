open Core
open Why3
open Gr1_weaken

(* Helper to create a simple variable *)
let make_var name =
  let id = Ident.id_fresh name in
  let ty = Ty.ty_bool in
  Term.create_vsymbol id ty

(* Test basic boolean constants *)
let test_boolean_constants () =
  print_endline "Testing boolean constants...";
  assert (String.equal (Gr1.term_to_promela Term.t_true) "true");
  assert (String.equal (Gr1.term_to_promela Term.t_false) "false");
  print_endline "  ✓ Boolean constants work correctly"

(* Test variables *)
let test_variables () =
  print_endline "Testing variables...";
  let var_x = make_var "x" in
  let term_x = Term.t_var var_x in
  assert (String.equal (Gr1.term_to_promela term_x) "x");

  let var_state = make_var "state" in
  let term_state = Term.t_var var_state in
  assert (String.equal (Gr1.term_to_promela term_state) "state");
  print_endline "  ✓ Variables work correctly"

(* Test logical operators *)
let test_logical_operators () =
  print_endline "Testing logical operators...";

  (* AND *)
  let and_term = Term.t_and Term.t_true Term.t_false in
  let result = Gr1.term_to_promela and_term in
  assert (String.is_substring result ~substring:"&&");
  assert (String.is_substring result ~substring:"true");
  assert (String.is_substring result ~substring:"false");

  (* OR *)
  let or_term = Term.t_or Term.t_true Term.t_false in
  let result = Gr1.term_to_promela or_term in
  assert (String.is_substring result ~substring:"||");

  (* NOT *)
  let not_term = Term.t_not Term.t_true in
  let result = Gr1.term_to_promela not_term in
  assert (String.is_substring result ~substring:"!");
  assert (String.is_substring result ~substring:"true");

  (* IMPLIES *)
  let implies_term = Term.t_implies Term.t_true Term.t_false in
  let result = Gr1.term_to_promela implies_term in
  assert (String.is_substring result ~substring:"||");
  assert (String.is_substring result ~substring:"!");

  (* IFF *)
  let iff_term = Term.t_iff Term.t_true Term.t_false in
  let result = Gr1.term_to_promela iff_term in
  assert (String.is_substring result ~substring:"==");

  print_endline "  ✓ Logical operators work correctly"

(* Test nested expressions *)
let test_nested_expressions () =
  print_endline "Testing nested expressions...";

  (* (true && false) || true *)
  let nested = Term.t_or (Term.t_and Term.t_true Term.t_false) Term.t_true in
  let result = Gr1.term_to_promela nested in
  assert (String.is_substring result ~substring:"&&");
  assert (String.is_substring result ~substring:"||");
  assert (String.is_substring result ~substring:"(");

  (* !(true && false) *)
  let nested2 = Term.t_not (Term.t_and Term.t_true Term.t_false) in
  let result = Gr1.term_to_promela nested2 in
  assert (String.is_substring result ~substring:"!");
  assert (String.is_substring result ~substring:"&&");

  print_endline "  ✓ Nested expressions work correctly"

(* Test GR(1) specification conversion *)
let test_gr1_to_promela () =
  print_endline "Testing GR(1) to Promela conversion...";

  let spec =
    Gr1.make ~asm_init:Term.t_true
      ~asm_safety:[ Term.t_true; Term.t_false ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_false
      ~gnt_safety:[ Term.t_true ]
      ~gnt_liveness:[ Term.t_false; Term.t_true ]
  in

  let result = Gr1.to_promela spec in

  (* Check that all sections are present *)
  assert (String.is_substring result ~substring:"GR(1) Specification");
  assert (String.is_substring result ~substring:"Assumptions");
  assert (String.is_substring result ~substring:"Guarantees");
  assert (String.is_substring result ~substring:"asm_init:");
  assert (String.is_substring result ~substring:"asm_safety:");
  assert (String.is_substring result ~substring:"asm_liveness:");
  assert (String.is_substring result ~substring:"gnt_init:");
  assert (String.is_substring result ~substring:"gnt_safety:");
  assert (String.is_substring result ~substring:"gnt_liveness:");

  print_endline "  ✓ GR(1) to Promela conversion works correctly"

(* Test GR(1) to LTL formula *)
let test_gr1_to_ltl () =
  print_endline "Testing GR(1) to LTL formula conversion...";

  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in

  let result = Gr1.to_promela_ltl spec in

  (* Check for implication *)
  assert (String.is_substring result ~substring:"->");

  (* Check for temporal operators *)
  assert (String.is_substring result ~substring:"[]");
  assert (String.is_substring result ~substring:"<>");

  (* Check for boolean operators *)
  assert (String.is_substring result ~substring:"&&");

  print_endline "  ✓ GR(1) to LTL formula works correctly"

(* Test empty lists in GR(1) spec *)
let test_empty_lists () =
  print_endline "Testing empty lists in GR(1) spec...";

  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[] ~asm_liveness:[]
      ~gnt_init:Term.t_true ~gnt_safety:[] ~gnt_liveness:[]
  in

  let result = Gr1.to_promela spec in
  (* Should still have init sections *)
  assert (String.is_substring result ~substring:"asm_init:");
  assert (String.is_substring result ~substring:"gnt_init:");

  (* Should not crash on empty lists *)
  let ltl_result = Gr1.to_promela_ltl spec in
  assert (String.is_substring ltl_result ~substring:"->");

  print_endline "  ✓ Empty lists handled correctly"

(* Test single element lists *)
let test_single_element_lists () =
  print_endline "Testing single element lists...";

  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ] ~asm_liveness:[]
      ~gnt_init:Term.t_false ~gnt_safety:[] ~gnt_liveness:[ Term.t_true ]
  in

  let result = Gr1.to_promela spec in
  assert (String.is_substring result ~substring:"asm_safety:");
  assert (String.is_substring result ~substring:"gnt_liveness:");

  print_endline "  ✓ Single element lists handled correctly"

(* Test complex nested GR(1) formula *)
let test_complex_gr1 () =
  print_endline "Testing complex GR(1) formula...";

  (* Create complex formulas using boolean constants and operators *)
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

  let result = Gr1.to_promela spec in
  assert (String.is_substring result ~substring:"true");
  assert (String.is_substring result ~substring:"false");
  assert (String.is_substring result ~substring:"&&");
  assert (String.is_substring result ~substring:"||");

  let ltl_result = Gr1.to_promela_ltl spec in
  assert (String.is_substring ltl_result ~substring:"true");
  assert (String.is_substring ltl_result ~substring:"false");
  assert (String.is_substring ltl_result ~substring:"->");

  print_endline "  ✓ Complex GR(1) formula works correctly"

(* Test temporal operators structure in output *)
let test_temporal_structure () =
  print_endline "Testing temporal operator structure...";

  let spec =
    Gr1.make ~asm_init:Term.t_true
      ~asm_safety:[ Term.t_true; Term.t_false ]
      ~asm_liveness:[ Term.t_true; Term.t_false ]
      ~gnt_init:Term.t_true ~gnt_safety:[ Term.t_true ]
      ~gnt_liveness:[ Term.t_true; Term.t_false ]
  in

  let ltl_result = Gr1.to_promela_ltl spec in

  (* Safety properties should be wrapped in [] *)
  (* Liveness properties should be wrapped in [] <> *)
  (* Multiple liveness properties should be combined with && *)
  assert (String.is_substring ltl_result ~substring:"[]");
  assert (String.is_substring ltl_result ~substring:"<>");

  print_endline "  ✓ Temporal operator structure is correct"

(* Test output format consistency *)
let test_output_format () =
  print_endline "Testing output format consistency...";

  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in

  (* Test that output is not empty *)
  let promela_result = Gr1.to_promela spec in
  assert (String.length promela_result > 0);

  let ltl_result = Gr1.to_promela_ltl spec in
  assert (String.length ltl_result > 0);

  (* Test that ltl result has proper structure: (assumptions) -> (guarantees) *)
  let arrow_count = String.count ltl_result ~f:(fun c -> Char.equal c '>') in
  assert (arrow_count >= 1);

  (* At least one -> *)
  print_endline "  ✓ Output format is consistent"

(* Main test runner *)
let () =
  print_endline "╔════════════════════════════════════════════════════╗";
  print_endline "║  Running Promela Pretty-Printer Tests             ║";
  print_endline "╚════════════════════════════════════════════════════╝\n";

  test_boolean_constants ();
  test_variables ();
  test_logical_operators ();
  test_nested_expressions ();
  test_gr1_to_promela ();
  test_gr1_to_ltl ();
  test_empty_lists ();
  test_single_element_lists ();
  test_complex_gr1 ();
  test_temporal_structure ();
  test_output_format ();

  print_endline "\n╔════════════════════════════════════════════════════╗";
  print_endline "║  All tests passed! ✓                              ║";
  print_endline "╚════════════════════════════════════════════════════╝"
