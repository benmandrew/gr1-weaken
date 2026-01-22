(** Parse SMV propositional formulae into Why3 terms *)

val parse_formula : string -> Ltl.any_formula
(** [parse_formula input] parses a single SMV formula and returns an LTL
    formula.
    @raise Invalid_argument if parsing fails *)

val parse_formulae : string -> Ltl.any_formula list
(** [parse_formulae input] parses multiple comma/semicolon-separated formulae.
    @return a list of LTL formulas *)

val parse_ident : string -> string
(** [parse_ident input] parses a variable identifier *)
