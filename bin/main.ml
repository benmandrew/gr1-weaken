open Core
open Why3
open Gr1_weaken

(* Example: Create a simple GR(1) specification and convert to Promela *)
let example_promela () =
  (* Create example terms (these would normally come from parsing/analysis) *)
  (* For demonstration, we'll create simple boolean terms *)
  let true_term = Term.t_true in

  (* Create a GR(1) spec *)
  let spec =
    Gr1.make ~asm_init:true_term ~asm_safety:[ true_term ]
      ~asm_liveness:[ true_term ] ~gnt_init:true_term ~gnt_safety:[ true_term ]
      ~gnt_liveness:[ true_term ]
  in

  (* Convert to Promela *)
  print_endline "=== GR(1) Specification in Promela ===";
  print_endline (Gr1.to_promela spec);
  print_endline "\n=== As Single LTL Formula ===";
  print_endline (Gr1.to_promela_ltl spec)

let () =
  print_endline "GR(1) Weakening Tool\n";
  example_promela ()
