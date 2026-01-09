open Core
open Why3

type property =
  | Prop of Term.term
  | Safety of Term.term
  | Liveness of Term.term

type state = (property, bool) Hashtbl.t
type t

val prefix_length : t -> int
(** [prefix_length lasso] returns the number of states in the prefix of the
    lasso trace. *)

val loop_length : t -> int
(** [loop_length lasso] returns the number of states in the loop of the lasso
    trace. *)

val length : t -> int
(** [length lasso] returns the total number of states in the lasso trace,
    combining both the prefix and loop. *)

val lsymbol_cache : (string, Term.lsymbol) Hashtbl.t
(** Global cache for Why3 logical symbols. *)

val of_states : (string * bool) list list -> int -> t
(** [of_states states loop_point] constructs a lasso trace from a list of states
    and the index of the loop point. Each state is represented as a list of
    (variable name, value) pairs. The [loop_point] indicates the start index of
    the loop within the states list. *)

val get_state : t -> int -> state
(** [get lasso idx] retrieves the variable assignments at the specified index
    [idx] in the lasso trace. The index counts through the prefix states first,
    followed by the loop states. *)

val get_future_states : t -> int -> state list
(** [get_future_states lasso idx] returns the list of future states starting
    from index [idx] in the lasso trace. *)

val get_assignments : t -> (string * bool) list list
(** [get_assignments lasso] returns the list of state assignments in the lasso
    trace, combining both the prefix and loop states. *)

val print : t -> unit
(** [print lasso] prints the lasso trace in a human-readable format. *)
