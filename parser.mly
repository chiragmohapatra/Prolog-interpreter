%{
  open Assign5
%}

%token LPAREN RPAREN COMMA IF SEMI EOL EOF
%token <bool> BOOL
%token <string> VAR ID

%start main
%type <Assign5.clause> main

%%

main:
    clausee EOL {$1}
  | EOL {(Node("end",[]),[])}
  | SEMI {(Node("next",[]),[])}
  | EOF {(Node("end_of_file",[]),[])}
  ;

clausee:
    head { ($1,[]) }
  | head IF body { ($1,$3) }
  | body  {(Node("goal",[])),$1}
  ;

head:
    ID { Node($1,[]) }
  | VAR { V($1) }
  | LPAREN head RPAREN { $2 }
  | ID LPAREN body RPAREN { Node($1,$3) }
  ;

body:
    head { [$1] }
  | head COMMA body { $1::$3 }
  ;











