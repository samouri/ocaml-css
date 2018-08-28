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
  | rulesets EOF { Some $1 }
  ;

rulesets:
 | EOF { [] }
 | S* r=ruleset S* { [ r ] }
 | S* r=ruleset S* rs=rulesets S* { r :: rs }
 ;

ruleset:
  | S* s=selectors S* LBRACE S* r=rules S* RBRACE { (s , r, $loc) }
  ;

rules:
  | { [] }
  | S* p=IDENT S* COLON S* t=terms S* SEMICOLON S* r=rules S* 
    { (p, t, $loc) :: r } 
  ;

terms:
  | S* t=term S* { [ t ] }
  | S* t=term S* ts=terms S* { t :: ts }
 ; 

term: 
  | DIMENSION { match $1 with (f,u) -> Dimension(f,u) } 
  | NUMBER { Number (int_of_string $1) } 
  | STRING { String $1 }
  | IDENT { Ident $1 }
  | URI { URI $1 }
 (* | FUNCTION { Func $1 } *)
  ;

selectors: 
  | S* s=selector S* { [ s ] } 
  | S* s1=selector S+ s2=selector { [ s1 ^ " " ^ s2 ] } 
  | S* s1=selector s2=selector { [ s1 ^ s2 ] }
  | S* s=selector S* COMMA S* ss=selectors S* { s :: ss } 
 ; 
 
selector: 
  | element_name { $1 }
  | DOT i=IDENT { "." ^ i }
  ;

element_name: 
  | IDENT { $1 }
  | STAR { "*" }
  ;