%{
  (* SMV Parser for propositional formulae *)
  open Core
  open Why3

  (* Helper to create or retrieve a propositional symbol *)
  let var_term name =
    match Hashtbl.find Lasso.lsymbol_cache name with
    | Some ps -> Term.ps_app ps []
    | None ->
        let ps = Term.create_psymbol (Ident.id_fresh name) [] in
        Hashtbl.set Lasso.lsymbol_cache ~key:name ~data:ps;
        Term.ps_app ps []

  (* Helper to apply unary operations *)
  let unary_op op arg =
    match op with
    | "!" -> Term.t_not arg
    | "G" ->
        (* For temporal operators, just return the argument for now *)
        arg
    | "F" -> arg
    | "X" -> arg
    | _ -> arg

  (* Helper to apply binary operators *)
  let binary_op op left right =
    match op with
    | "&" -> Term.t_and left right
    | "|" -> Term.t_or left right
    | "->" -> Term.t_implies left right
    | "<->" -> Term.t_iff left right
    | "=" | "==" -> Term.t_equ left right
    | "!=" | "<>" -> Term.t_neq left right
    | "+" | "-" | "*" | "/" | "mod" | "<" | "<=" | ">" | ">=" | "U" | "R" | "W" ->
        (* Unsupported operators: return left for now *)
        left
    | _ -> left
%}

/* Terminal symbols */
%token TRUE FALSE
%token AND OR NOT
%token IMPLIES IFF
%token G F X U R W
%token EQ NEQ LT LE GT GE
%token PLUS MINUS MULT DIV MOD
%token LPAREN RPAREN
%token <string> IDENT
%token EOF

/* Start symbol */
%start formula
%type <Why3.Term.term> formula

/* Operator precedence and associativity (low to high) */
%left IFF
%left IMPLIES
%left OR
%left AND
%left EQ NEQ LT LE GT GE
%left PLUS MINUS
%left MULT DIV MOD
%right NOT
%right G F X
%left U R W

%%

formula:
  | expr EOF { $1 }

expr:
  | expr AND expr       { binary_op "&" $1 $3 }
  | expr OR expr        { binary_op "|" $1 $3 }
  | expr IMPLIES expr   { binary_op "->" $1 $3 }
  | expr IFF expr       { binary_op "<->" $1 $3 }
  | expr U expr         { binary_op "U" $1 $3 }
  | expr R expr         { binary_op "R" $1 $3 }
  | expr W expr         { binary_op "W" $1 $3 }
  | NOT expr            { unary_op "!" $2 }
  | G expr              { unary_op "G" $2 }
  | F expr              { unary_op "F" $2 }
  | X expr              { unary_op "X" $2 }
  | expr EQ expr        { binary_op "=" $1 $3 }
  | expr NEQ expr       { binary_op "!=" $1 $3 }
  | expr LT expr        { binary_op "<" $1 $3 }
  | expr LE expr        { binary_op "<=" $1 $3 }
  | expr GT expr        { binary_op ">" $1 $3 }
  | expr GE expr        { binary_op ">=" $1 $3 }
  | expr PLUS expr      { binary_op "+" $1 $3 }
  | expr MINUS expr     { binary_op "-" $1 $3 }
  | expr MULT expr      { binary_op "*" $1 $3 }
  | expr DIV expr       { binary_op "/" $1 $3 }
  | expr MOD expr       { binary_op "mod" $1 $3 }
  | primary             { $1 }

primary:
  | TRUE                { Term.t_true }
  | FALSE               { Term.t_false }
  | IDENT               { var_term $1 }
  | LPAREN expr RPAREN  { $2 }
