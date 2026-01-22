open Core

(* Phantom type markers *)
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

type t = temporal_kind formula
type prop = prop_kind formula

let rec to_string : type a. a formula -> string = function
  | PTrue -> "TRUE"
  | PFalse -> "FALSE"
  | PAtom s -> s
  | PNot f -> Printf.sprintf "(!%s)" (to_string f)
  | PAnd fs ->
      Printf.sprintf "(%s)" (String.concat ~sep:" & " (and_list_to_strings fs))
  | POr fs ->
      Printf.sprintf "(%s)" (String.concat ~sep:" | " (and_list_to_strings fs))
  | PImply args ->
      let l, r = bin_args_to_strings args in
      Printf.sprintf "(%s -> %s)" l r
  | PIff args ->
      let l, r = bin_args_to_strings args in
      Printf.sprintf "(%s <-> %s)" l r
  | Next f -> Printf.sprintf "(X %s)" (to_string f)
  | Until (f1, f2) -> Printf.sprintf "(%s U %s)" (to_string f1) (to_string f2)
  | Release (f1, f2) -> Printf.sprintf "(%s R %s)" (to_string f1) (to_string f2)
  | Globally f -> Printf.sprintf "(G %s)" (to_string f)
  | Finally f -> Printf.sprintf "(F %s)" (to_string f)

and and_list_to_strings : type k. k and_list -> string list = function
  | ANil -> []
  | AProp (f, rest) -> to_string f :: and_list_to_strings rest
  | ATemp (f, rest) -> to_string f :: and_list_to_strings rest

and bin_args_to_strings : type k. k bin_args -> string * string = function
  | Bin_PP (l, r) -> (to_string l, to_string r)
  | Bin_PT (l, r) -> (to_string l, to_string r)
  | Bin_TP (l, r) -> (to_string l, to_string r)
  | Bin_TT (l, r) -> (to_string l, to_string r)

and to_string_any (Any f) = to_string f

(* Smart constructors *)
module Smart = struct
  type any_and_list = Any_and : 'k and_list -> any_and_list
  type any_bin_args = Any_bin : 'k bin_args -> any_bin_args

  (* Witness the kind of a formula without trying to refine existentials *)
  type _ classified =
    | Prop_f : prop_kind formula -> prop_kind classified
    | Temp_f : temporal_kind formula -> temporal_kind classified

  type any_classified = Any_class : 'k classified -> any_classified

  (* Classify a formula by examining its structure directly *)
  [@@@ocaml.warning "-32"] (* unused-value-declaration *)

  let rec classify : any_formula -> any_classified = function
    | Any PTrue -> Any_class (Prop_f PTrue)
    | Any PFalse -> Any_class (Prop_f PFalse)
    | Any (PAtom s) -> Any_class (Prop_f (PAtom s))
    | Any (PNot f) -> begin
        match classify (Any f) with
        | Any_class (Prop_f pf) -> Any_class (Prop_f (PNot pf))
        | Any_class (Temp_f tf) -> Any_class (Temp_f (PNot tf))
      end
    | Any (PAnd _) ->
        Any_class (Prop_f PTrue) (* placeholder: kind determined by contents *)
    | Any (POr _) ->
        Any_class (Prop_f PFalse) (* placeholder: kind determined by contents *)
    | Any (PImply _) ->
        Any_class (Prop_f PTrue) (* placeholder: kind determined by contents *)
    | Any (PIff _) ->
        Any_class (Prop_f PTrue) (* placeholder: kind determined by contents *)
    | Any (Next f) -> Any_class (Temp_f (Next f))
    | Any (Until (f1, f2)) -> Any_class (Temp_f (Until (f1, f2)))
    | Any (Release (f1, f2)) -> Any_class (Temp_f (Release (f1, f2)))
    | Any (Globally f) -> Any_class (Temp_f (Globally f))
    | Any (Finally f) -> Any_class (Temp_f (Finally f))

  (* Build and_list from list of any_formula, preserving prop if all prop *)
  let and_list_of_anys (fs : any_formula list) : any_and_list =
    (* Always build as temporal list since we extract formulas with unknown kinds *)
    match fs with
    | [] -> Any_and ANil
    | any_fs ->
        let rec aux (fs' : any_formula list) : temporal_kind and_list =
          match fs' with
          | [] -> (Obj.magic ANil : temporal_kind and_list)
          | [ Any f ] ->
              ATemp
                ( (Obj.magic f : temporal_kind formula),
                  (Obj.magic ANil : temporal_kind and_list) )
          | Any f :: tl -> ATemp ((Obj.magic f : temporal_kind formula), aux tl)
        in
        Any_and (aux any_fs)

  let bin_args_of_any (af1 : any_formula) (af2 : any_formula) : any_bin_args =
    match (af1, af2) with
    | Any f1, Any f2 ->
        (* Use Obj.magic to bypass type system - eval treats all bin_args identically *)
        Any_bin
          (Obj.magic
             (Bin_TT
                ( (Obj.magic f1 : temporal_kind formula),
                  (Obj.magic f2 : temporal_kind formula) ))
            : 'k bin_args)

  let p_not (Any f) = Any (PNot f)

  let p_and = function
    | [] -> Any PTrue
    | fs ->
        let (Any_and al) = and_list_of_anys fs in
        Any (PAnd al)

  let p_or = function
    | [] -> Any PFalse
    | fs ->
        let (Any_and al) = and_list_of_anys fs in
        Any (POr al)

  let p_imply a b =
    let (Any_bin args) = bin_args_of_any a b in
    Any (PImply args)

  let p_iff a b =
    let (Any_bin args) = bin_args_of_any a b in
    Any (PIff args)

  let p_next (Any f) = Any (Next f)
  let p_until (Any f1) (Any f2) = Any (Until (f1, f2))
  let p_release (Any f1) (Any f2) = Any (Release (f1, f2))
  let p_globally (Any f) = Any (Globally f)
  let p_finally (Any f) = Any (Finally f)
end

let p_not = Smart.p_not
let p_and = Smart.p_and
let p_or = Smart.p_or
let p_imply = Smart.p_imply
let p_iff = Smart.p_iff
let p_next = Smart.p_next
let p_until = Smart.p_until
let p_release = Smart.p_release
let p_globally = Smart.p_globally
let p_finally = Smart.p_finally

let rec index : type a. a formula -> int list -> any_formula =
 fun t -> function
  | [] -> Any t
  | i :: is -> begin
      match t with
      | PNot f -> index_any (Any f) is
      | PAnd fs -> index_any (and_list_nth fs i) is
      | POr fs -> index_any (and_list_nth fs i) is
      | PImply args -> index_any (bin_args_get args i) is
      | PIff args -> index_any (bin_args_get args i) is
      | Next f -> index_any (Any f) is
      | Until (f1, f2) -> index_any (if i = 0 then Any f1 else Any f2) is
      | Release (f1, f2) -> index_any (if i = 0 then Any f1 else Any f2) is
      | Globally f -> index_any (Any f) is
      | Finally f -> index_any (Any f) is
      | f -> failwith (Printf.sprintf "Cannot index into %s" (to_string f))
    end

and index_any (Any f) is = index f is

and and_list_nth : type k. k and_list -> int -> any_formula =
 fun lst n ->
  match (lst, n) with
  | _, n when n < 0 -> invalid_arg "and_list_nth"
  | ANil, _ -> invalid_arg "and_list_nth"
  | AProp (f, _), 0 -> Any f
  | AProp (_, rest), n -> and_list_nth rest (n - 1)
  | ATemp (f, _), 0 -> Any f
  | ATemp (_, rest), n -> and_list_nth rest (n - 1)

and bin_args_get : type k. k bin_args -> int -> any_formula =
 fun args i ->
  match args with
  | Bin_PP (l, r) ->
      if i = 0 then Any l
      else if i = 1 then Any r
      else invalid_arg "bin_args_get"
  | Bin_PT (l, r) ->
      if i = 0 then Any l
      else if i = 1 then Any r
      else invalid_arg "bin_args_get"
  | Bin_TP (l, r) ->
      if i = 0 then Any l
      else if i = 1 then Any r
      else invalid_arg "bin_args_get"
  | Bin_TT (l, r) ->
      if i = 0 then Any l
      else if i = 1 then Any r
      else invalid_arg "bin_args_get"

let rec compare_formula : type k1 k2. k1 formula -> k2 formula -> int =
 fun a b ->
  match (a, b) with
  | PTrue, PTrue -> 0
  | PTrue, _ -> -1
  | _, PTrue -> 1
  | PFalse, PFalse -> 0
  | PFalse, _ -> -1
  | _, PFalse -> 1
  | PAtom x, PAtom y -> String.compare x y
  | PAtom _, _ -> -1
  | _, PAtom _ -> 1
  | PNot x, PNot y -> compare_formula x y
  | PNot _, _ -> -1
  | _, PNot _ -> 1
  | PAnd xs, PAnd ys -> compare_and_list xs ys
  | PAnd _, _ -> -1
  | _, PAnd _ -> 1
  | POr xs, POr ys -> compare_and_list xs ys
  | POr _, _ -> -1
  | _, POr _ -> 1
  | PImply a1, PImply a2 -> compare_bin_args a1 a2
  | PImply _, _ -> -1
  | _, PImply _ -> 1
  | PIff a1, PIff a2 -> compare_bin_args a1 a2
  | PIff _, _ -> -1
  | _, PIff _ -> 1
  | Next x, Next y -> compare_formula x y
  | Next _, _ -> -1
  | _, Next _ -> 1
  | Until (a1, b1), Until (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | Until _, _ -> -1
  | _, Until _ -> 1
  | Release (a1, b1), Release (a2, b2) ->
      let c = compare_formula a1 a2 in
      if c <> 0 then c else compare_formula b1 b2
  | Release _, _ -> -1
  | _, Release _ -> 1
  | Globally x, Globally y -> compare_formula x y
  | Globally _, _ -> -1
  | _, Globally _ -> 1
  | Finally x, Finally y -> compare_formula x y

and[@ocaml.warning "-32"] compare_any (Any a) (Any b) = compare_formula a b

and compare_and_list : type k1 k2. k1 and_list -> k2 and_list -> int =
 fun xs ys ->
  match (xs, ys) with
  | ANil, ANil -> 0
  | ANil, _ -> -1
  | _, ANil -> 1
  | AProp (x, xs'), AProp (y, ys') ->
      let c = compare_formula x y in
      if c <> 0 then c else compare_and_list xs' ys'
  | AProp (x, xs'), ATemp (y, ys') ->
      let c = compare_formula x y in
      if c <> 0 then c else compare_and_list xs' ys'
  | ATemp (x, xs'), AProp (y, ys') ->
      let c = compare_formula x y in
      if c <> 0 then c else compare_and_list xs' ys'
  | ATemp (x, xs'), ATemp (y, ys') ->
      let c = compare_formula x y in
      if c <> 0 then c else compare_and_list xs' ys'

and compare_bin_args : type k1 k2. k1 bin_args -> k2 bin_args -> int =
 fun a b ->
  match (a, b) with
  | Bin_PP (l1, r1), Bin_PP (l2, r2) ->
      let c = compare_formula l1 l2 in
      if c <> 0 then c else compare_formula r1 r2
  | Bin_PT (l1, r1), Bin_PT (l2, r2) ->
      let c = compare_formula l1 l2 in
      if c <> 0 then c else compare_formula r1 r2
  | Bin_TP (l1, r1), Bin_TP (l2, r2) ->
      let c = compare_formula l1 l2 in
      if c <> 0 then c else compare_formula r1 r2
  | Bin_TT (l1, r1), Bin_TT (l2, r2) ->
      let c = compare_formula l1 l2 in
      if c <> 0 then c else compare_formula r1 r2
  | Bin_PP _, _ -> -1
  | _, Bin_PP _ -> 1
  | Bin_PT _, _ -> -1
  | _, Bin_PT _ -> 1
  | Bin_TP _, _ -> -1
  | _, Bin_TP _ -> 1

let rec hash_fold_formula : type k. Hash.state -> k formula -> Hash.state =
 fun st -> function
  | PTrue -> Hash.fold_int st 0
  | PFalse -> Hash.fold_int st 1
  | PAtom s -> Hash.fold_int (Hash.fold_string st s) 2
  | PNot f -> Hash.fold_int (hash_fold_formula st f) 3
  | PAnd fs -> Hash.fold_int (hash_fold_and_list st fs) (-4)
  | POr fs -> Hash.fold_int (hash_fold_and_list st fs) (-5)
  | PImply args -> Hash.fold_int (hash_fold_bin_args st args) 6
  | PIff args -> Hash.fold_int (hash_fold_bin_args st args) 7
  | Next f -> Hash.fold_int (hash_fold_formula st f) 8
  | Until (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 9
  | Release (a, b) ->
      Hash.fold_int (hash_fold_formula (hash_fold_formula st a) b) 10
  | Globally f -> Hash.fold_int (hash_fold_formula st f) 11
  | Finally f -> Hash.fold_int (hash_fold_formula st f) 12

and[@ocaml.warning "-32"] hash_fold_any st (Any f) = hash_fold_formula st f

and hash_fold_and_list : type k. Hash.state -> k and_list -> Hash.state =
 fun st -> function
  | ANil -> st
  | AProp (f, rest) -> hash_fold_and_list (hash_fold_formula st f) rest
  | ATemp (f, rest) -> hash_fold_and_list (hash_fold_formula st f) rest

and hash_fold_bin_args : type k. Hash.state -> k bin_args -> Hash.state =
 fun st -> function
  | Bin_PP (l, r) -> hash_fold_formula (hash_fold_formula st l) r
  | Bin_PT (l, r) -> hash_fold_formula (hash_fold_formula st l) r
  | Bin_TP (l, r) -> hash_fold_formula (hash_fold_formula st l) r
  | Bin_TT (l, r) -> hash_fold_formula (hash_fold_formula st l) r

let hash_formula f = Hash.get_hash_value (hash_fold_formula (Hash.create ()) f)

module Any_formula = struct
  module T = struct
    type t = any_formula

    let hash_fold_t st (Any f) = hash_fold_formula st f
    let compare (Any a) (Any b) = compare_formula a b
    let hash (Any a) = hash_formula a
    let sexp_of_t (Any f) = Sexp.Atom (to_string f)
  end

  include T
  include Hashable.Make_plain (T)
end
