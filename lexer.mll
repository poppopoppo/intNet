{
    open Parser
    exception Error of string
}

let space = [' ' '\t' '\n' '\r']
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ascii = ['a'-'z' 'A'-'Z' '0'-'9' '(' ')' '!' '"' '#'
  '$' '%' '&' '\'' '=' '~' '~' '|' '{' '}' '`' '@' '[' ']'
  '*' '+' ';' ':' '<' '>' ',' '.' '?' '/' '\\' '_' ]
let alnum = digit | alpha

rule token = parse
    | "\194\167"  (* § *) { LCE }
    | "CMD"    { CMD }
    | "\194\187" (* » *)  { OPR                            }
    | "\226\138\162" (* ⊢ *) { SRC             }
    | '(' { L_PRN }
    | ')' { R_PRN }
    | "+" { PLS }
    | '*' { MLT }
    | (('-' digit+)|digit+) as lxm  { INT (int_of_string lxm) }
    | alpha+ as lxm { NAM (lxm) }
    | '$' ((('-' digit+)|digit+) as lxm) { VAL (int_of_string lxm) }
    | space+        { token lexbuf                         }
    | eof           { EOF                                  }
    | _             { raise (Error (Printf.sprintf
                      "At offset %d: unexpected character.\n"
                      (Lexing.lexeme_start lexbuf))) }
