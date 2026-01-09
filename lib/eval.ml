open Core
open Why3

let rec eval_aux lasso i f =
  match f.Term.t_node with
  | Tbinop (Tand, p, q) -> eval lasso i p && eval lasso i q
  | Tbinop (Tor, p, q) -> eval lasso i p || eval lasso i q
  | Tbinop (Timplies, p, q) -> (not @@ eval lasso i p) || eval lasso i q
  | Tbinop (Tiff, p, q) -> Bool.equal (eval lasso i p) (eval lasso i q)
  | Tnot p -> not @@ eval lasso i p
  | Ttrue -> true
  | Tfalse -> false
  | Tapp (p, []) ->
      Format.printf "Proposition '%s' not found\n" p.ls_name.Ident.id_string;
      raise (Not_found_s (Core.Sexp.Atom "Proposition not found"))
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise (Not_found_s (Core.Sexp.Atom "Unsupported term in eval"))

and eval lasso i f =
  let state = Lasso.get lasso i in
  match Hashtbl.find state f with
  | Some v -> v
  | None ->
      let res = eval_aux lasso i f in
      Hashtbl.set state ~key:f ~data:res;
      res
