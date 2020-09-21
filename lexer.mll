(* File lexer.mll *)
{
open Parser
}

let characters = ['a'-'z' 'A'-'Z' '0'-'9' '_' ''']*

rule token = parse
  [' ' '\t' '\n'] {token lexbuf}   (*skip blanks and newlines*)
| '.' {EOL}
| ',' {COMMA}
| '(' {LPAREN}
| ')' {RPAREN}
| ';' {SEMI}
| ":-" {IF}
| "true" {BOOL(true)}
| "false" {BOOL(false)}
| ['A'-'Z'](characters) as str {VAR(str)}
| ['a'-'z'](characters) as str {ID(str)}
| eof {EOF}

