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

val asm_init : t -> Term.term
val asm_safety : t -> Term.term list
val asm_liveness : t -> Term.term list
val gnt_init : t -> Term.term
val gnt_safety : t -> Term.term list
val gnt_liveness : t -> Term.term list

val term_to_smv : ?indent:int -> Term.term -> string
(** [term_to_smv term] converts a Why3 term to SMV LTL syntax.

    Supports:
    - Boolean operators: &, |, !, ->, <->
    - Arithmetic operators: +, -, *, /, mod
    - Comparison operators: =, !=, <, <=, >, >=
    - LTL operators: G (always), F (eventually), X (next), U (until), R
      (release)
    - Variables and constants *)

val to_smv : t -> string
(** [to_smv t] converts a GR(1) specification to a readable SMV format with
    separate sections for assumptions and guarantees. *)

val to_smv_ltl : t -> string
(** [to_smv_ltl t] converts a GR(1) specification to a single SMV LTL formula in
    the form: (assumptions) -> (guarantees)

    The GR(1) formula is structured as:
    - Assumptions: init ∧ G safety ∧ ∧ᵢ G F liveness_i
    - Guarantees: init ∧ G safety ∧ ∧ᵢ G F liveness_i *)

val eval : t -> Lasso.t -> unit
(** [eval spec lasso] evaluates all assumptions and guarantees of the GR(1)
    specification [spec] against the provided lasso trace [lasso] to fill the
    lasso's cache. *)
