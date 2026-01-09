open Core
open Why3

type t = {
  asm_init : Term.term;
  asm_safety : Term.term list;
  asm_liveness : Term.term list;
  gnt_init : Term.term;
  gnt_safety : Term.term list;
  gnt_liveness : Term.term list;
}

let make ~asm_init ~asm_safety ~asm_liveness ~gnt_init ~gnt_safety ~gnt_liveness
    =
  { asm_init; asm_safety; asm_liveness; gnt_init; gnt_safety; gnt_liveness }

let asm_init t = t.asm_init
let asm_safety t = t.asm_safety
let asm_liveness t = t.asm_liveness
let gnt_init t = t.gnt_init
let gnt_safety t = t.gnt_safety
let gnt_liveness t = t.gnt_liveness

let rec term_to_smv ?(indent = 0) (term : Term.term) : string =
  let ind = String.make (indent * 2) ' ' in
  match term.t_node with
  | Tvar vs ->
      (* Variables *)
      vs.vs_name.Ident.id_string
  | Tconst _ ->
      (* Constants - use generic string representation *)
      Format.asprintf "%a" Pretty.print_term term
  | Tapp (fs, args) -> (
      let fname = fs.ls_name.Ident.id_string in
      let args_str = List.map args ~f:(term_to_smv ~indent) in
      match (fname, args) with
      | "True", [] -> "TRUE"
      | "False", [] -> "FALSE"
      (* Logical operators *)
      | "and", [ a; b ] ->
          sprintf "(%s & %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "or", [ a; b ] ->
          sprintf "(%s | %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "not", [ a ] -> sprintf "!(%s)" (term_to_smv ~indent a)
      | "->", [ a; b ] | "implies", [ a; b ] ->
          sprintf "(!(%s) | %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<->", [ a; b ] | "iff", [ a; b ] ->
          sprintf "((%s) <-> (%s))" (term_to_smv ~indent a)
            (term_to_smv ~indent b)
      (* Arithmetic operators *)
      | "+", [ a; b ] ->
          sprintf "(%s + %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "-", [ a; b ] ->
          sprintf "(%s - %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "*", [ a; b ] ->
          sprintf "(%s * %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "/", [ a; b ] ->
          sprintf "(%s / %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "mod", [ a; b ] ->
          sprintf "(%s mod %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Comparison operators *)
      | "=", [ a; b ] | "==", [ a; b ] ->
          sprintf "(%s == %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "!=", [ a; b ] | "<>", [ a; b ] ->
          sprintf "(%s != %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<", [ a; b ] ->
          sprintf "(%s < %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "<=", [ a; b ] ->
          sprintf "(%s <= %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | ">", [ a; b ] ->
          sprintf "(%s > %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | ">=", [ a; b ] ->
          sprintf "(%s >= %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Temporal operators (LTL) *)
      | "G", [ a ] | "[]", [ a ] -> sprintf "G (%s)" (term_to_smv ~indent a)
      | "F", [ a ] | "<>", [ a ] -> sprintf "F (%s)" (term_to_smv ~indent a)
      | "X", [ a ] | "next", [ a ] -> sprintf "X (%s)" (term_to_smv ~indent a)
      | "U", [ a; b ] ->
          sprintf "(%s U %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      | "R", [ a; b ] ->
          sprintf "(%s R %s)" (term_to_smv ~indent a) (term_to_smv ~indent b)
      (* Default: function application *)
      | _ ->
          if List.is_empty args then fname
          else sprintf "%s(%s)" fname (String.concat ~sep:", " args_str))
  | Tif (cond, then_t, else_t) ->
      sprintf "(%s ? %s : %s)" (term_to_smv ~indent cond)
        (term_to_smv ~indent then_t)
        (term_to_smv ~indent else_t)
  | Tlet (t, tb) ->
      let vs, body = Term.t_open_bound tb in
      sprintf "/* let %s = %s in */\n%s%s" vs.vs_name.Ident.id_string
        (term_to_smv ~indent t) ind (term_to_smv ~indent body)
  | Tcase (scrutinee, branches) ->
      let scrutinee_str = term_to_smv ~indent scrutinee in
      let branches_str =
        List.mapi branches ~f:(fun i br ->
            let _pat, body = Term.t_open_branch br in
            sprintf "%s/* case %d: */ %s" ind i
              (term_to_smv ~indent:(indent + 1) body))
        |> String.concat ~sep:"\n"
      in
      sprintf "/* match %s */\n%s" scrutinee_str branches_str
  | Tquant (quant, tq) ->
      let vsl, _, body = Term.t_open_quant tq in
      let quant_str =
        match quant with Tforall -> "forall" | Texists -> "exists"
      in
      let vars =
        List.map vsl ~f:(fun vs -> vs.vs_name.Ident.id_string)
        |> String.concat ~sep:", "
      in
      sprintf "/* %s %s */ %s" quant_str vars (term_to_smv ~indent body)
  | Tbinop (op, t1, t2) -> (
      match op with
      | Tand ->
          sprintf "(%s & %s)" (term_to_smv ~indent t1) (term_to_smv ~indent t2)
      | Tor ->
          sprintf "(%s | %s)" (term_to_smv ~indent t1) (term_to_smv ~indent t2)
      | Timplies ->
          sprintf "(!(%s) | %s)" (term_to_smv ~indent t1)
            (term_to_smv ~indent t2)
      | Tiff ->
          sprintf "((%s) <-> (%s))" (term_to_smv ~indent t1)
            (term_to_smv ~indent t2))
  | Tnot t -> sprintf "!(%s)" (term_to_smv ~indent t)
  | Ttrue -> "TRUE"
  | Tfalse -> "FALSE"
  | Teps _ -> "/* epsilon term */"

(* Convert GR(1) specification to SMV LTL formula *)
let to_smv t =
  let format_list name terms =
    match terms with
    | [] -> None
    | _ ->
        let formulas = List.map terms ~f:term_to_smv in
        let combined = String.concat ~sep:" & " formulas in
        Some (sprintf "%s: %s" name combined)
  in
  let parts =
    [
      Some (sprintf "-- GR(1) Specification in SMV LTL\n");
      Some (sprintf "-- Assumptions");
      Some (sprintf "asm_init: %s" (term_to_smv t.asm_init));
      format_list "asm_safety" t.asm_safety;
      format_list "asm_liveness" t.asm_liveness;
      Some (sprintf "\n-- Guarantees");
      Some (sprintf "gnt_init: %s" (term_to_smv t.gnt_init));
      format_list "gnt_safety" t.gnt_safety;
      format_list "gnt_liveness" t.gnt_liveness;
    ]
  in
  List.filter_map parts ~f:Fn.id |> String.concat ~sep:"\n"

(* Convert GR(1) to a single SMV LTL formula *)
let to_smv_ltl t =
  let combine_with_and terms =
    match terms with
    | [] -> None
    | [ x ] -> Some (term_to_smv x)
    | _ ->
        let formulas = List.map terms ~f:term_to_smv in
        Some (sprintf "(%s)" (String.concat ~sep:" & " formulas))
  in
  let always_terms terms =
    match combine_with_and terms with
    | None -> None
    | Some formula -> Some (sprintf "G (%s)" formula)
  in
  let always_eventually_terms terms =
    match terms with
    | [] -> None
    | _ ->
        let formulas =
          List.map terms ~f:(fun t -> sprintf "G F (%s)" (term_to_smv t))
        in
        Some (sprintf "(%s)" (String.concat ~sep:" & " formulas))
  in
  let assumptions =
    [
      Some (term_to_smv t.asm_init);
      always_terms t.asm_safety;
      always_eventually_terms t.asm_liveness;
    ]
    |> List.filter_map ~f:Fn.id
  in
  let guarantees =
    [
      Some (term_to_smv t.gnt_init);
      always_terms t.gnt_safety;
      always_eventually_terms t.gnt_liveness;
    ]
    |> List.filter_map ~f:Fn.id
  in
  let asm_formula = String.concat ~sep:" & " assumptions in
  let gnt_formula = String.concat ~sep:" & " guarantees in
  sprintf "(%s) -> (%s)" asm_formula gnt_formula

let eval t lasso =
  let length = Lasso.length lasso in
  for i = 0 to length - 1 do
    Eval.eval lasso i t.asm_init |> ignore;
    Eval.eval lasso i t.gnt_init |> ignore;
    List.map t.asm_safety ~f:(Eval.eval_safety lasso i) |> ignore;
    List.map t.gnt_safety ~f:(Eval.eval_safety lasso i) |> ignore;
    List.map t.asm_liveness ~f:(Eval.eval_liveness lasso i) |> ignore;
    List.map t.gnt_liveness ~f:(Eval.eval_liveness lasso i) |> ignore
  done
