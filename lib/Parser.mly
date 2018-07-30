%{
  open Types
%}

%token RPAREN LPAREN EQUALS SEMICOLON CHILD COLOR RSQUARE LSQUARE DOT STAR EXCLAMATION CONTAINS
%token S CDO CDC INCLUDES DASHMATCH LBRACE RBRACE PLUS MINUS GREATER COMMA COLON PREFIX
%token SLASH ATIMPORT ATCHARSET ATMEDIA ATPAGE ATFONTFACE
%token <string> STRING IDENT HASH NUMBER URI CHAR FUNCTION ATKEYWORD
%token <int * int> UNICODE
%token <float> PERCENTAGE
%token <float * string> DIMENSION
%token <char> ERROR
%token INVALID
%token IMPORT_SYM IMPORTANT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM
%token EOF

%start <Types.rulesets> stylesheet	/* the entry point */
%%

/* stylesheet */
stylesheet:
  | EOF { None }
  | ruleset_list { Some $1 }
  ;

ruleset_list:
 | ruleset { $1 }
 | ruleset ruleset_list { $1 :: $2 }
 | EOF { [] }
 ;

ruleset:
  selector LBRACE rules RBRACE { ($1, $3) }
  ;

rules:
  | { [] }
  | IDENT COLON terms SEMICOLON rules { ($1, $3) :: $5 } 
  ;

terms:
  | { [] }
  | term { [ $1 ] }
  | term terms { $1 :: $2 }
 ; 

term: 
  | DIMENSION { match $1 with (f,u) -> Dimension(f,u) } 
  | STRING { String $1 }
  | IDENT { Ident $1 }
  | URI { URI $1 }
 (* | FUNCTION { Func $1 } *)
  ;

selector: 
  | element_name { $1 }
  | DOT IDENT { $2 }
  ;

element_name: 
  | IDENT { $1 }
  | STAR { "*" }
  ;