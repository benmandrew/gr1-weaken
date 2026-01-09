open Core
open Why3
module Sexp = Sexplib.Sexp

module Ts = struct
  include Why3.Term

  type t = term

  let compare = Term.t_compare
  let sexp_of_t t = Sexp.Atom (Format.asprintf "%a" Pretty.print_term t)
  let hash = Hashtbl.hash
end

let rec eval_aux cache f =
  if Hashtbl.mem cache f then Hashtbl.find_exn cache f
  else
    match f.Term.t_node with
    | Tbinop (Tand, p, q) -> eval_aux cache p && eval_aux cache q
    | Tbinop (Tor, p, q) -> eval_aux cache p || eval_aux cache q
    | Tbinop (Timplies, p, q) -> (not @@ eval_aux cache p) || eval_aux cache q
    | Tbinop (Tiff, p, q) -> Bool.equal (eval_aux cache p) (eval_aux cache q)
    | Tnot p -> not @@ eval_aux cache p
    | Ttrue -> true
    | Tfalse -> false
    | Tapp (p, []) ->
        Format.printf "Proposition '%s' not found\n" p.ls_name.Ident.id_string;
        raise (Not_found_s (Sexp.Atom "Proposition not found"))
    | _ ->
        Format.printf "'%a' not supported\n" Pretty.print_term f;
        raise (Not_found_s (Sexp.Atom "Unsupported term in eval"))

let eval f =
  let cache = Hashtbl.create (module Ts) in
  eval_aux cache f
