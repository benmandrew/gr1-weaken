open Core
open Why3

type property =
  | Prop of Term.term
  | Safety of Term.term
  | Liveness of Term.term

module Ts = struct
  include Term

  type t = property

  let compare t t' =
    match (t, t') with
    | Prop t1, Prop t2 -> Term.t_compare t1 t2
    | Safety t1, Safety t2 -> Term.t_compare t1 t2
    | Liveness t1, Liveness t2 -> Term.t_compare t1 t2
    | Prop _, _ -> -1
    | _, Prop _ -> 1
    | Safety _, _ -> -1
    | _, Safety _ -> 1

  let sexp_of_t = function
    | Prop t | Safety t | Liveness t ->
        Core.Sexp.Atom (Format.asprintf "%a" Pretty.print_term t)

  let hash = function Prop t | Safety t | Liveness t -> Term.t_hash t
end

type state = (property, bool) Hashtbl.t
type t = { prefix : state list; loop : state list }

let lsymbol_cache = Hashtbl.create (module String)

let prop_of name =
  match Hashtbl.find lsymbol_cache name with
  | Some ps -> ps
  | None ->
      let ps = Term.create_psymbol (Ident.id_fresh name) [] in
      Hashtbl.set lsymbol_cache ~key:name ~data:ps;
      ps

let lit_of (name, value) = (Prop (Term.ps_app (prop_of name) []), value)

let of_states states loop_idx =
  (* Split states into prefix and loop *)
  let state_prefix = List.take states loop_idx in
  let state_loop = List.drop states loop_idx in
  let state_to_term assigns =
    assigns |> List.map ~f:lit_of |> Hashtbl.of_alist (module Ts) |> function
    | `Duplicate_key _ -> raise (Invalid_argument "Duplicate variable in state")
    | `Ok pair -> pair
  in
  {
    prefix = List.map state_prefix ~f:state_to_term;
    loop = List.map state_loop ~f:state_to_term;
  }

let get_state t i =
  if i < List.length t.prefix then List.nth_exn t.prefix i
  else List.nth_exn t.loop ((i - List.length t.prefix) mod List.length t.loop)

let get_future_states t i =
  if i < List.length t.prefix then
    let future_prefix = List.drop t.prefix i in
    future_prefix @ t.loop
  else
    let loop_start_idx = (i - List.length t.prefix) mod List.length t.loop in
    let future_loop = List.drop t.loop loop_start_idx in
    let wrap_around = List.take t.loop loop_start_idx in
    future_loop @ wrap_around

let get_assignments t =
  t.prefix @ t.loop
  |> List.map ~f:(fun t ->
      Hashtbl.to_alist t
      |> List.map ~f:(fun (k, v) ->
          let var_name =
            match k with
            | Prop term -> Format.asprintf "%a" Pretty.print_term term
            | Safety term -> Format.asprintf "G %a" Pretty.print_term term
            | Liveness term -> Format.asprintf "G F %a" Pretty.print_term term
          in
          (var_name, v)))

let collect_variable_names states =
  states
  |> List.concat_map ~f:(List.map ~f:fst)
  |> List.dedup_and_sort ~compare:String.compare

let header width num_states =
  if num_states >= 10 then begin
    printf "%*s" width "";
    for i = 0 to num_states - 1 do
      if i < 10 then printf "  " else printf " %d" (i / 10)
    done;
    printf "\n"
  end;
  printf "%*s" width "";
  for i = 0 to num_states - 1 do
    printf " %d" (i mod 10)
  done;
  printf "\n"

let print_rows width vars num_states get_value =
  List.iter vars ~f:(fun var_name ->
      printf "%*s" (-width) var_name;
      for i = 0 to num_states - 1 do
        let char =
          match get_value i var_name with
          | Some true -> "●"
          | Some false -> " "
          | None -> "?"
        in
        printf "│%s" char
      done;
      printf "│\n")

let print_indicator width prefix_len num_states =
  printf "%*s" (-(width + 1)) "=Lasso=";
  for i = 0 to num_states - 1 do
    if i = prefix_len && i = num_states - 1 then printf "⊔"
    else if i = prefix_len then printf "└─"
    else if i = num_states - 1 then printf "┘ "
    else if i > prefix_len then printf "──"
    else printf "  "
  done;
  printf "\n"

let lasso_str_len = String.length "=Lasso="

let print t =
  let state_assigns = get_assignments t in
  let num_states = List.length state_assigns in
  let prefix_len = List.length t.prefix in
  let vars = collect_variable_names state_assigns in
  let width =
    let var_widths = List.map vars ~f:String.length in
    let all_widths = lasso_str_len :: var_widths in
    List.max_elt all_widths ~compare:Int.compare
    |> Option.value ~default:lasso_str_len
    |> Int.succ
  in
  let state_map = Array.of_list state_assigns in
  let get_value state_idx var_name =
    if state_idx < Array.length state_map then
      List.Assoc.find state_map.(state_idx) ~equal:String.equal var_name
    else None
  in
  header width num_states;
  print_rows width vars num_states get_value;
  print_indicator width prefix_len num_states
