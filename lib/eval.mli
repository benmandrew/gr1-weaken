val eval : Lasso.t -> int -> Ltl.any_formula -> bool
(** [eval lasso i term] evaluates the given term [term] in the context of the
    lasso [lasso] at index [i]. If the term does not already exist in the cache,
    it is computed and stored for future use. *)
