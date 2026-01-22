open Core

let in_loop lasso i = i >= Lasso.prefix_length lasso

let get_next_index lasso i =
  let next_i = i + 1 in
  let prefix_len = Lasso.prefix_length lasso in
  let loop_len = Lasso.loop_length lasso in
  if next_i >= Lasso.length lasso then
    prefix_len + ((next_i - prefix_len) % loop_len)
  else next_i

(* Main evaluation function *)
let rec eval (lasso : Lasso.t) (i : int) (Ltl.Any formula : Ltl.any_formula) :
    bool =
  match formula with
  | Ltl.PTrue -> true
  | Ltl.PFalse -> false
  | Ltl.PAtom name -> (
      let state = Lasso.get_state lasso i in
      match Hashtbl.find state (Ltl.Any (Ltl.PAtom name)) with
      | Some v -> v
      | None -> false)
  | Ltl.PNot f -> not (eval lasso i (Ltl.Any f))
  | Ltl.PAnd fs -> eval_and_list lasso i fs ~combine:( && ) ~identity:true
  | Ltl.POr fs -> eval_and_list lasso i fs ~combine:( || ) ~identity:false
  | Ltl.PImply args -> eval_imply lasso i args
  | Ltl.PIff args -> eval_iff lasso i args
  | Ltl.Next f -> eval lasso (get_next_index lasso i) (Ltl.Any f)
  | Ltl.Globally f -> eval_globally lasso i (Ltl.Any f)
  | Ltl.Finally f -> eval_finally lasso i (Ltl.Any f)
  | Ltl.Until (f1, f2) -> eval_until lasso i (Ltl.Any f1) (Ltl.Any f2)
  | Ltl.Release (f1, f2) -> eval_release lasso i (Ltl.Any f1) (Ltl.Any f2)

and eval_and_list : type k.
    Lasso.t ->
    int ->
    k Ltl.and_list ->
    combine:(bool -> bool -> bool) ->
    identity:bool ->
    bool =
 fun lasso i lst ~combine ~identity ->
  match lst with
  | Ltl.ANil -> identity
  | Ltl.AProp (f, rest) ->
      combine (eval lasso i (Ltl.Any f))
        (eval_and_list lasso i rest ~combine ~identity)
  | Ltl.ATemp (f, rest) ->
      combine (eval lasso i (Ltl.Any f))
        (eval_and_list lasso i rest ~combine ~identity)

and eval_imply : type k. Lasso.t -> int -> k Ltl.bin_args -> bool =
 fun lasso i -> function
  | Ltl.Bin_PP (l, r) ->
      (not (eval lasso i (Ltl.Any l))) || eval lasso i (Ltl.Any r)
  | Ltl.Bin_PT (l, r) ->
      (not (eval lasso i (Ltl.Any l))) || eval lasso i (Ltl.Any r)
  | Ltl.Bin_TP (l, r) ->
      (not (eval lasso i (Ltl.Any l))) || eval lasso i (Ltl.Any r)
  | Ltl.Bin_TT (l, r) ->
      (not (eval lasso i (Ltl.Any l))) || eval lasso i (Ltl.Any r)

and eval_iff : type k. Lasso.t -> int -> k Ltl.bin_args -> bool =
 fun lasso i -> function
  | Ltl.Bin_PP (l, r) ->
      Bool.equal (eval lasso i (Ltl.Any l)) (eval lasso i (Ltl.Any r))
  | Ltl.Bin_PT (l, r) ->
      Bool.equal (eval lasso i (Ltl.Any l)) (eval lasso i (Ltl.Any r))
  | Ltl.Bin_TP (l, r) ->
      Bool.equal (eval lasso i (Ltl.Any l)) (eval lasso i (Ltl.Any r))
  | Ltl.Bin_TT (l, r) ->
      Bool.equal (eval lasso i (Ltl.Any l)) (eval lasso i (Ltl.Any r))

(* Evaluate Globally: G(f) = f ∧ X(G(f)) *)
and eval_globally lasso i (Ltl.Any f) =
  if not (eval lasso i (Ltl.Any f)) then false
  else if not (in_loop lasso i) then
    eval lasso (get_next_index lasso i) (Ltl.Any (Ltl.Globally f))
  else
    let loop_len = Lasso.loop_length lasso in
    let rec check_loop idx count =
      if count > loop_len then true
      else if eval lasso idx (Ltl.Any f) then
        check_loop (get_next_index lasso idx) (count + 1)
      else false
    in
    check_loop (get_next_index lasso i) 1

(* Evaluate Finally: F(f) = f ∨ X(F(f)) *)
and eval_finally lasso i (Ltl.Any f) =
  if eval lasso i (Ltl.Any f) then true
  else if not (in_loop lasso i) then
    eval lasso (get_next_index lasso i) (Ltl.Any (Ltl.Finally f))
  else
    let loop_len = Lasso.loop_length lasso in
    let rec check_loop idx count =
      if count > loop_len then false
      else if eval lasso idx (Ltl.Any f) then true
      else check_loop (get_next_index lasso idx) (count + 1)
    in
    check_loop (get_next_index lasso i) 1

(* Evaluate Until: f1 U f2 = f2 ∨ (f1 ∧ X(f1 U f2)) *)
and eval_until lasso i (Ltl.Any f1) (Ltl.Any f2) =
  if eval lasso i (Ltl.Any f2) then true
  else if not (eval lasso i (Ltl.Any f1)) then false
  else if not (in_loop lasso i) then
    eval lasso (get_next_index lasso i) (Ltl.Any (Ltl.Until (f1, f2)))
  else
    let loop_len = Lasso.loop_length lasso in
    let rec check_loop idx count =
      if count > loop_len then false
      else if eval lasso idx (Ltl.Any f2) then true
      else if eval lasso idx (Ltl.Any f1) then
        check_loop (get_next_index lasso idx) (count + 1)
      else false
    in
    check_loop (get_next_index lasso i) 1

(* Evaluate Release: f1 R f2 = f2 ∧ (f1 ∨ X(f1 R f2)) *)
and eval_release lasso i (Ltl.Any f1) (Ltl.Any f2) =
  if not (eval lasso i (Ltl.Any f2)) then false
  else if eval lasso i (Ltl.Any f1) then
    eval lasso (get_next_index lasso i) (Ltl.Any (Ltl.Release (f1, f2)))
  else true
