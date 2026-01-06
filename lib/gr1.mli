open Why3

type t

val make :
  asm_init:Term.term ->
  asm_safety:Term.term list ->
  asm_liveness:Term.term list ->
  gnt_init:Term.term ->
  gnt_safety:Term.term list ->
  gnt_liveness:Term.term list ->
  t

val term_to_promela : ?indent:int -> Term.term -> string
(** [term_to_promela term] converts a Why3 term to Promela/SPIN syntax.

    Supports:
    - Boolean operators: &&, ||, !, ==>, <=>
    - Arithmetic operators: +, -, *, /, %
    - Comparison operators: ==, !=, <, <=, >, >=
    - LTL operators: [] (always), <> (eventually), X (next), U (until), R
      (release)
    - Variables and constants *)

val to_promela : t -> string
(** [to_promela t] converts a GR(1) specification to a readable Promela format
    with separate sections for assumptions and guarantees. *)

val to_promela_ltl : t -> string
(** [to_promela_ltl t] converts a GR(1) specification to a single Promela LTL
    formula in the form: (assumptions) -> (guarantees)

    The GR(1) formula is structured as:
    - Assumptions: init ∧ \[\]safety ∧ ∧ᵢ \[\]<>liveness_i
    - Guarantees: init ∧ \[\]safety ∧ ∧ᵢ \[\]<>liveness_i *)
