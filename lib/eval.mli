open Why3

val eval : Lasso.t -> int -> Term.term -> bool
(** [eval lasso i term] evaluates the given term [term] in the context of the
    lasso [lasso] at index [i]. If the term does not already exist in the cache,
    it is computed and stored for future use. *)

val eval_safety : Lasso.t -> int -> Term.term -> bool
(** [eval_safety lasso i term] evaluates a safety property [G term] at index [i]
    in the lasso [lasso]. *)
