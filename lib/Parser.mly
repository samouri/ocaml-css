%{
  open Types
%}

%token EOF 
%token RPAREN LPAREN EQUALS SEMICOLON CHILD COLOR RSQUARE LSQUARE DOT STAR EXCLAMATION CONTAINS
%token S CDO CDC INCLUDES DASHMATCH LBRACE RBRACE PLUS MINUS GREATER COMMA COLON PREFIX HASH
%token SLASH ATIMPORT ATCHARSET ATMEDIA ATPAGE ATFONTFACE ATNAMESPACE
%token <string> IDENT NUMBER URI CHAR FUNCTION ATKEYWORD COMMENT
%token <char * string> STRING /* character representing either " or '*/
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
 | r=rule* { r }
 ;

rule: 
  | at_rule { $1 }
  | style_rule { $1 }

at_rule:
 | k=ATKEYWORD S* prefix=component_value_w* S* s=STRING SEMICOLON? { 
   AtRule({ 
     name=k; 
     prelude=prefix; 
     block= Some {token=Brace; value=[]; pos=$loc};
     pos=$loc;
   })
 };

style_rule:
  | s=component_value_w+ LBRACE S* r=declaration_w* e=RBRACE { 
    StyleRule({
      prelude=s; 
      declarations=r; 
      pos= ($startpos(s), $endpos(e));
    })
  }
  ;

declaration_w: d=declaration S* { d }; 
declaration:
  | star=STAR? p=IDENT S* COLON S* t=component_value_w+ e=SEMICOLON? { 
    let prefix = match star with None -> "" | Some _ -> "*" in
    Else({
      name = prefix ^ p;
      value = t;
      important = false;
      pos=($startpos(star), $startpos(e));
    })
  } 
  ;

component_value_w: cv=component_value S* { cv }; 
component_value: 
  | IDENT { Ident $1 }
  | FUNCTION { Func $1 }
  | HASH IDENT { Hash($2) }
  | NUMBER { Number $1 } 
  | DIMENSION { match $1 with (f,u) -> Dimension(f,u) } 
  | PERCENTAGE { Percentage $1 } 
  | STRING { match $1 with (c, s) -> String(c,s) }
  | URI { Uri $1 }
 (* | block { $1 } *)
  ;
 (* | UnicodeRange of string *)

(*selectors: 
  | s=selector S* { [ s ] }
  | s=selector S* COMMA S* ss=selectors { s :: ss }
  ; 

(* simple_selector [ combinator selector | S+ [ combinator? selector ]? ]? *)
selector: 
  | simple_selector { $1 }
  | s1=selector S s2=simple_selector { s1 ^ " " ^ s2 }
  | s1=selector s2=simple_selector { s1 ^ s2 }
  | s=simple_selector LSQUARE S* ident=IDENT EQUALS str=STRING S* RSQUARE { s ^ "[" ^ ident ^ "=" ^ "'" ^ str ^ "'" ^ "]" }
  ;

simple_selector:
  | element_name { $1 }
  | dotclass { $1 }
  | hash { $1 }

*)