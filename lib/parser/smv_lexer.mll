{
  (* SMV Lexer for parsing propositional formulae *)
}

let whitespace = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z' '_']
let ident = letter (letter | digit)*

rule token = parse
  | whitespace     { token lexbuf }
  | "(*"           { comment lexbuf }
  | "TRUE"         { Smv_parser.TRUE }
  | "FALSE"        { Smv_parser.FALSE }
  | "true"         { Smv_parser.TRUE }
  | "false"        { Smv_parser.FALSE }
  (* Logical operators *)
  | "&&"           { Smv_parser.AND }
  | '&'            { Smv_parser.AND }
  | "||"           { Smv_parser.OR }
  | '|'            { Smv_parser.OR }
  | '!'            { Smv_parser.NOT }
  | "->"           { Smv_parser.IMPLIES }
  | "<->"          { Smv_parser.IFF }
  (* Temporal operators *)
  | "next"         { Smv_parser.X }
  | 'G'            { Smv_parser.G }
  | 'F'            { Smv_parser.F }
  | 'X'            { Smv_parser.X }
  | 'W'            { Smv_parser.W }
  | "[]"           { Smv_parser.G }  (* Alias for G *)
  | "<>"           { Smv_parser.F }  (* Alias for F *)
  | 'U'            { Smv_parser.U }
  | 'R'            { Smv_parser.R }
  (* Comparisons *)
  | "=="           { Smv_parser.EQ }
  | "!="           { Smv_parser.NEQ }
  | '='            { Smv_parser.EQ }
  | "<="           { Smv_parser.LE }
  | ">="           { Smv_parser.GE }
  | '<'            { Smv_parser.LT }
  | '>'            { Smv_parser.GT }
  (* Arithmetic *)
  | '+'            { Smv_parser.PLUS }
  | '-'            { Smv_parser.MINUS }
  | '*'            { Smv_parser.MULT }
  | '/'            { Smv_parser.DIV }
  | "mod"          { Smv_parser.MOD }
  (* Delimiters *)
  | '('            { Smv_parser.LPAREN }
  | ')'            { Smv_parser.RPAREN }
  | ':'            { token lexbuf }  (* Skip colon *)
  | '.'            { token lexbuf }  (* Skip dot *)
  | ','            { token lexbuf }  (* Skip comma *)
  | ';'            { token lexbuf }  (* Skip semicolon *)
  (* Identifiers and numbers *)
  | ident as id    { Smv_parser.IDENT id }
  | eof            { Smv_parser.EOF }
  | _              { token lexbuf }  (* Skip unexpected characters *)

and comment = parse
  | "*)"           { token lexbuf }
  | _              { comment lexbuf }
