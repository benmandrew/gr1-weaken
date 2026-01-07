open Core
open Why3
module Sexp = Sexplib.Sexp

module Ls = struct
  type t = Why3.Term.lsymbol

  include Comparator.Make (struct
    type nonrec t = t

    let compare = Why3.Term.ls_compare
    let sexp_of_t _ = Sexp.Atom "lsymbol"
  end)
end

let rec eval a f =
  match f.Term.t_node with
  | Tbinop (Tand, p, q) -> eval a p && eval a q
  | Tbinop (Tor, p, q) -> eval a p || eval a q
  | Tbinop (Timplies, p, q) -> (not @@ eval a p) || eval a q
  | Tbinop (Tiff, p, q) -> Bool.equal (eval a p) (eval a q)
  | Tnot p -> not @@ eval a p
  | Ttrue -> true
  | Tfalse -> false
  | Tapp (p, []) -> Map.find_exn a p
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise (Not_found_s (Sexp.Atom "Unsupported term in eval"))
