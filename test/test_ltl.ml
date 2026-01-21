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

let%expect_test "GR(1) to SMV conversion" =
  let req = make_prop "request" in
  let gnt = make_prop "grant" in
  let busy = make_prop "busy" in
  let asm_safety =
    Term.t_implies (prop_term req) (Term.t_not (prop_term busy))
  in
  let gnt_safety =
    Term.t_implies (prop_term gnt) (Term.t_not (prop_term busy))
  in
  let spec =
    Gr1.make
      ~asm_init:(Term.t_not (prop_term busy))
      ~asm_safety:[ asm_safety ]
      ~asm_liveness:[ prop_term req ]
      ~gnt_init:(Term.t_not (prop_term gnt))
      ~gnt_safety:[ gnt_safety ]
      ~gnt_liveness:[ prop_term gnt ]
  in
  print_endline (Gr1.to_smv spec);
  [%expect
    {|
    -- GR(1) Specification in SMV LTL

    -- Assumptions
    asm_init: !busy
    asm_safety: (!request | !busy)
    asm_liveness: request

    -- Guarantees
    gnt_init: !grant
    gnt_safety: (!grant | !busy)
    gnt_liveness: grant
    |}]

let%expect_test "GR(1) to SMV LTL formula" =
  let req = make_prop "request" in
  let gnt = make_prop "grant" in
  let spec =
    Gr1.make ~asm_init:(prop_term req)
      ~asm_safety:[ Term.t_not (prop_term req) ]
      ~asm_liveness:[ prop_term req ]
      ~gnt_init:(Term.t_not (prop_term gnt))
      ~gnt_safety:[ Term.t_implies (prop_term req) (prop_term gnt) ]
      ~gnt_liveness:[ prop_term gnt ]
  in
  print_endline (Gr1.to_smv_ltl spec);
  [%expect
    {| (request & G (!request) & ((G F request)) -> !grant & G ((!request | grant)) & ((G F grant))) |}]

let%expect_test "empty lists in GR(1) spec" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[] ~asm_liveness:[]
      ~gnt_init:Term.t_true ~gnt_safety:[] ~gnt_liveness:[]
  in
  print_endline (Gr1.to_smv spec);
  [%expect
    {|
    -- GR(1) Specification in SMV LTL

    -- Assumptions
    asm_init: TRUE

    -- Guarantees
    gnt_init: TRUE |}];
  print_endline (Gr1.to_smv_ltl spec);
  [%expect {| (TRUE -> TRUE) |}]

let%expect_test "single element lists" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ] ~asm_liveness:[]
      ~gnt_init:Term.t_false ~gnt_safety:[] ~gnt_liveness:[ Term.t_true ]
  in
  print_endline (Gr1.to_smv spec);
  [%expect
    {|
    -- GR(1) Specification in SMV LTL

    -- Assumptions
    asm_init: TRUE
    asm_safety: TRUE

    -- Guarantees
    gnt_init: FALSE
    gnt_liveness: TRUE |}]

let%expect_test "temporal operator structure" =
  let p = make_prop "p" in
  let q = make_prop "q" in
  let spec =
    Gr1.make ~asm_init:(prop_term p)
      ~asm_safety:[ prop_term p; prop_term q ]
      ~asm_liveness:[ prop_term p; prop_term q ]
      ~gnt_init:(prop_term p)
      ~gnt_safety:[ prop_term p ]
      ~gnt_liveness:[ prop_term p; prop_term q ]
  in
  print_endline (Gr1.to_smv_ltl spec);
  [%expect
    {| (p & G ((p & q)) & ((G F p) & (G F q)) -> p & G (p) & ((G F p) & (G F q))) |}]

let%expect_test "output format consistency" =
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[ Term.t_true ]
      ~asm_liveness:[ Term.t_true ] ~gnt_init:Term.t_true
      ~gnt_safety:[ Term.t_true ] ~gnt_liveness:[ Term.t_true ]
  in
  (* Test that output is not empty and has proper structure *)
  let smv_result = Gr1.to_smv spec in
  print_s [%sexp (String.length smv_result > 0 : bool)];
  [%expect {| true |}];
  let ltl_result = Gr1.to_smv_ltl spec in
  print_s [%sexp (String.length ltl_result > 0 : bool)];
  [%expect {| true |}];
  (* Test that ltl result has implication arrow *)
  print_s [%sexp (String.is_substring ltl_result ~substring:"->" : bool)];
  [%expect {| true |}]

let%expect_test "mutual exclusion example" =
  let in_cs1 = make_prop "in_critical_section_1" in
  let in_cs2 = make_prop "in_critical_section_2" in
  let req1 = make_prop "request_1" in
  let req2 = make_prop "request_2" in
  (* Mutual exclusion: not both in critical section *)
  let mutex = Term.t_not (Term.t_and (prop_term in_cs1) (prop_term in_cs2)) in
  (* If request, eventually in critical section *)
  let liveness1 = Term.t_implies (prop_term req1) (prop_term in_cs1) in
  let liveness2 = Term.t_implies (prop_term req2) (prop_term in_cs2) in
  let spec =
    Gr1.make ~asm_init:Term.t_true ~asm_safety:[]
      ~asm_liveness:[ prop_term req1; prop_term req2 ]
      ~gnt_init:
        (Term.t_and
           (Term.t_not (prop_term in_cs1))
           (Term.t_not (prop_term in_cs2)))
      ~gnt_safety:[ mutex ] ~gnt_liveness:[ liveness1; liveness2 ]
  in
  print_endline (Gr1.to_smv spec);
  [%expect
    {|
    -- GR(1) Specification in SMV LTL

    -- Assumptions
    asm_init: TRUE
    asm_liveness: request_1 & request_2

    -- Guarantees
    gnt_init: (!in_critical_section_1 & !in_critical_section_2)
    gnt_safety: !(in_critical_section_1 & in_critical_section_2)
    gnt_liveness: (!request_1 | in_critical_section_1) & (!request_2 | in_critical_section_2)
    |}]
