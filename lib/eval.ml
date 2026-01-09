open Core
open Why3

let rec eval_on_state_aux state f =
  match f.Term.t_node with
  | Tbinop (Tand, p, q) -> eval_on_state state p && eval_on_state state q
  | Tbinop (Tor, p, q) -> eval_on_state state p || eval_on_state state q
  | Tbinop (Timplies, p, q) ->
      (not @@ eval_on_state state p) || eval_on_state state q
  | Tbinop (Tiff, p, q) ->
      Bool.equal (eval_on_state state p) (eval_on_state state q)
  | Tnot p -> not @@ eval_on_state state p
  | Ttrue -> true
  | Tfalse -> false
  | Tapp (p, []) ->
      Format.printf "Proposition '%s' not found\n" p.ls_name.Ident.id_string;
      raise (Not_found_s (Core.Sexp.Atom "Proposition not found"))
  | _ ->
      Format.printf "'%a' not supported\n" Pretty.print_term f;
      raise (Not_found_s (Core.Sexp.Atom "Unsupported term in eval"))

and eval_on_state state f =
  match Hashtbl.find state (Lasso.Prop f) with
  | Some v -> v
  | None ->
      let res = eval_on_state_aux state f in
      Hashtbl.set state ~key:(Lasso.Prop f) ~data:res;
      res

let eval lasso i f =
  let state = Lasso.get_state lasso i in
  eval_on_state state f

let eval_safety lasso i f =
  let initial_state = Lasso.get_state lasso i in
  match Hashtbl.find initial_state (Lasso.Safety f) with
  | Some v -> v
  | None ->
      let states = Lasso.get_future_states lasso i in
      let res = List.for_all states ~f:(fun state -> eval_on_state state f) in
      Hashtbl.set initial_state ~key:(Lasso.Safety f) ~data:res;
      res

let eval_liveness lasso i f =
  let initial_state = Lasso.get_state lasso i in
  match Hashtbl.find initial_state (Lasso.Liveness f) with
  | Some v -> v
  | None ->
      let prefix_length = Lasso.prefix_length lasso in
      let loop_states =
        let states = Lasso.get_future_states lasso i in
        if i < prefix_length then List.drop states (prefix_length - i)
        else states
      in
      let res =
        List.exists loop_states ~f:(fun state -> eval_on_state state f)
      in
      Hashtbl.set initial_state ~key:(Lasso.Liveness f) ~data:res;
      res
