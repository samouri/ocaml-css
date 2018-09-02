%{
  open Types
%}

%token RPAREN LPAREN EQUALS SEMICOLON CHILD COLOR RSQUARE LSQUARE DOT STAR EXCLAMATION CONTAINS
%token S CDO CDC INCLUDES DASHMATCH LBRACE RBRACE PLUS MINUS GREATER COMMA COLON PREFIX HASH
%token SLASH ATIMPORT ATCHARSET ATMEDIA ATPAGE ATFONTFACE
%token <string> STRING DOUBLESTRING IDENT NUMBER URI CHAR FUNCTION ATKEYWORD COMMENT
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
  | s=selectors S* LBRACE S* r=rules e=RBRACE { (s , r, ($startpos(s), $endpos(e))) }
  ;

rules:
  | { [] }
  | S* star=STAR? p=IDENT S* COLON S* t=term_w* S* e=SEMICOLON? S* r=rules { 
    let prefix = match star with None -> "" | Some _ -> "*" in
    (prefix ^ p, t, ($startpos(star), $startpos(e))) :: r } 
  ;

term_w: S* term S* { $2 }
  ; 

term: 
  | DIMENSION { match $1 with (f,u) -> Dimension(f,u) } 
  | NUMBER { Number $1 } 
  | PERCENTAGE { Percentage $1 } 
  | STRING { String $1 }
  | DOUBLESTRING { DoubleString $1 }
  | IDENT { Ident $1 }
  | URI { URI $1 }
 (* | FUNCTION { Func $1 } *)
  ;

selectors: 
  | s=selector S* { [ s ] } 
  | s1=selector S+ s2=selectors { match s2 with hd::tl -> (s1 ^ " " ^ hd) :: tl | [] -> [ s1 ] } 
  | s1=selector s2=selector { [ s1 ^ s2 ] }
  | s=selector S* COMMA S* ss=selectors S* { s :: ss } 
 ; 
 
selector: 
  | element_name { $1 }
  | DOT i=IDENT { "." ^ i }
  | HASH i=IDENT { "#" ^ i }
  | s=selector LSQUARE S* ident=IDENT EQUALS str=STRING S* RSQUARE { s ^ "[" ^ ident ^ "=" ^ "'" ^ str ^ "'" ^ "]" }
  | s=selector LSQUARE S* ident=IDENT EQUALS str=DOUBLESTRING S* RSQUARE { s ^ "[" ^ ident ^ "=" ^ "\"" ^ str ^ "\"" ^ "]" }
  ;

element_name: 
  | IDENT { $1 }
  | STAR { "*" }
  ;