open Core
open Ltl_weaken

let%expect_test "TLSF parse GR(1) specification" =
  let tlsf_spec =
    {|MAIN {
INPUTS {
  request : boolean;
  busy : boolean;
}

OUTPUTS {
  ack : boolean;
  idle : boolean;
}

INITIALLY {
  TRUE;
}

PRESET {
}

REQUIRE {
  (request -> !busy);
}

ASSERT {
  ack;
}

ASSUME {
  request;
}

GUARANTEE {
  idle;
}
}|}
  in
  let spec = Parser.Tlsf.parse_gr1 tlsf_spec in
  printf "asm_init: %s\n" (Print.term_to_smv (Gr1.asm_init spec));
  printf "asm_safety: %s\n"
    (Gr1.asm_safety spec
    |> List.map ~f:Print.term_to_smv
    |> String.concat ~sep:", ");
  printf "asm_liveness: %s\n"
    (Gr1.asm_liveness spec
    |> List.map ~f:Print.term_to_smv
    |> String.concat ~sep:", ");
  printf "gnt_init: %s\n" (Print.term_to_smv (Gr1.gnt_init spec));
  printf "gnt_safety: %s\n"
    (Gr1.gnt_safety spec
    |> List.map ~f:Print.term_to_smv
    |> String.concat ~sep:", ");
  printf "gnt_liveness: %s\n"
    (Gr1.gnt_liveness spec
    |> List.map ~f:Print.term_to_smv
    |> String.concat ~sep:", ");
  [%expect
    {|
    asm_init: TRUE
    asm_safety: (!request | !busy)
    asm_liveness: request
    gnt_init: TRUE
    gnt_safety: ack
    gnt_liveness: idle
    |}]

let%expect_test "TLSF parse with multiple safety properties" =
  let tlsf_spec =
    {|MAIN {
INPUTS {
  a : boolean;
}

OUTPUTS {
  b : boolean;
}

INITIALLY {
  a & b;
}

PRESET {
}

REQUIRE {
  (a -> b);
  (!a | b);
}

ASSERT {
}

ASSUME {
}

GUARANTEE {
  b;
  (a | b);
}
}|}
  in
  let spec = Parser.Tlsf.parse_gr1 tlsf_spec in
  printf "asm_init: %s\n" (Print.term_to_smv (Gr1.asm_init spec));
  printf "asm_safety count: %d\n" (List.length (Gr1.asm_safety spec));
  List.iter (Gr1.asm_safety spec) ~f:(fun term ->
      printf "  - %s\n" (Print.term_to_smv term));
  printf "gnt_liveness count: %d\n" (List.length (Gr1.gnt_liveness spec));
  List.iter (Gr1.gnt_liveness spec) ~f:(fun term ->
      printf "  - %s\n" (Print.term_to_smv term));
  [%expect
    {|
    asm_init: (a & b)
    asm_safety count: 2
      - (!a | b)
      - (!a | b)
    gnt_liveness count: 2
      - (a | b)
      - b
    |}]
