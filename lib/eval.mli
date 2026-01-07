open Core
module Ls : Comparator.S with type t = Why3.Term.lsymbol

val eval : bool Map.M(Ls).t -> Why3.Term.term -> bool
