%{
  open Types
%}

%token EOF 
%token RPAREN LPAREN EQUALS SEMICOLON CHILD COLOR RSQUARE LSQUARE DOT STAR EXCLAMATION CONTAINS
%token S CDO CDC INCLUDES DASHMATCH LBRACE RBRACE PLUS MINUS GREATER COMMA COLON PREFIX HASH
%token SLASH ATIMPORT ATCHARSET ATMEDIA ATPAGE ATFONTFACE ATNAMESPACE
%token <string> STRING DOUBLESTRING IDENT NUMBER URI CHAR FUNCTION ATKEYWORD COMMENT
%token <int * int> UNICODE
%token <float> PERCENTAGE
%token <float * string> DIMENSION
%token <char> ERROR
%token INVALID
%token IMPORT_SYM IMPORTANT_SYM PAGE_SYM MEDIA_SYM CHARSET_SYM

%start <Types.stylesheet> stylesheet	/* the entry point */
%%

/* stylesheet */
stylesheet:
  | r=rulesets S* EOF { r }
  ;

rulesets:
 | r=ruleset* { r }
 ;

at_rule:
 | k=ATKEYWORD S* prefix=IDENT? S* s=DOUBLESTRING SEMICOLON? { AtRule(k, prefix, DoubleString(s), $loc) }
 | k=ATKEYWORD S* prefix=IDENT? S* s=STRING SEMICOLON? { AtRule(k, prefix, String(s), $loc) }
 ;

ruleset:
  | s=selectors LBRACE S* r=rule_w* e=RBRACE { Ruleset (s , r, ($startpos(s), $endpos(e))) }
  | at_rule { $1 }
  ;

rule_w: r=rule S* { r }; 
rule:
  | star=STAR? p=IDENT S* COLON S* t=term_w+ e=SEMICOLON? { 
    let prefix = match star with None -> "" | Some _ -> "*" in
    Rule (prefix ^ p, t, ($startpos(star), $startpos(e))) 
    } 
  ;

term_w: t=term S* { t }; 
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
  | s=selector S* COMMA S* ss=selectors { s :: ss }
  ; 

(* simple_selector [ combinator selector | S+ [ combinator? selector ]? ]? *)
selector: 
  | simple_selector { $1 }
  | s1=selector S s2=simple_selector { s1 ^ " " ^ s2 }
  | s1=selector s2=simple_selector { s1 ^ s2 }
  | s=simple_selector LSQUARE S* ident=IDENT EQUALS str=STRING S* RSQUARE { s ^ "[" ^ ident ^ "=" ^ "'" ^ str ^ "'" ^ "]" }
  | s=simple_selector LSQUARE S* ident=IDENT EQUALS str=DOUBLESTRING S* RSQUARE { s ^ "[" ^ ident ^ "=" ^ "\"" ^ str ^ "\"" ^ "]" }
  ;

simple_selector:
  | element_name { $1 }
  | dotclass { $1 }
  | hash { $1 }

dotclass: DOT i=IDENT { "." ^ i }
hash: HASH i=IDENT { "#" ^ i }

element_name: 
  | IDENT { $1 }
  | STAR { "*" }
  ;