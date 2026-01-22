open Core

type prop_kind
type temporal_kind

type _ formula =
  (* Propositional operators *)
  | PTrue : prop_kind formula
  | PFalse : prop_kind formula
  | PAtom : string -> prop_kind formula
  | PNot : 'k formula -> 'k formula
  | PAnd : 'k and_list -> 'k formula
  | POr : 'k and_list -> 'k formula
  | PImply : 'k bin_args -> 'k formula
  | PIff : 'k bin_args -> 'k formula
  (* Temporal operators - result is temporal *)
  | Next : _ formula -> temporal_kind formula
  | Until : _ formula * _ formula -> temporal_kind formula
  | Release : _ formula * _ formula -> temporal_kind formula
  | Globally : _ formula -> temporal_kind formula
  | Finally : _ formula -> temporal_kind formula

and _ and_list =
  | ANil : prop_kind and_list
  | AProp : prop_kind formula * 'k and_list -> 'k and_list
  | ATemp : temporal_kind formula * _ and_list -> temporal_kind and_list

and _ bin_args =
  | Bin_PP : prop_kind formula * prop_kind formula -> prop_kind bin_args
  | Bin_PT : prop_kind formula * temporal_kind formula -> temporal_kind bin_args
  | Bin_TP : temporal_kind formula * prop_kind formula -> temporal_kind bin_args
  | Bin_TT :
      temporal_kind formula * temporal_kind formula
      -> temporal_kind bin_args

and any_formula = Any : 'k formula -> any_formula

type prop = prop_kind formula
type t = temporal_kind formula

(* Smart constructors *)
val p_not : any_formula -> any_formula
val p_and : any_formula list -> any_formula
val p_or : any_formula list -> any_formula
val p_imply : any_formula -> any_formula -> any_formula
val p_iff : any_formula -> any_formula -> any_formula
val p_next : any_formula -> any_formula
val p_until : any_formula -> any_formula -> any_formula
val p_release : any_formula -> any_formula -> any_formula
val p_globally : any_formula -> any_formula
val p_finally : any_formula -> any_formula
val to_string : 'k formula -> string
val to_string_any : any_formula -> string
val index : 'k formula -> int list -> any_formula

module Any_formula : sig
  val sexp_of_t : any_formula -> Sexplib.Sexp.t

  include Hashable.S_plain with type t = any_formula
end
