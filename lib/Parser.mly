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

%start <Types.rules option> stylesheet	/* the entry point */ 
%% 

/* stylesheet */
stylesheet: 
  charset space_cdata_list import_list ruleset_list media_list page_list
    { $1, $3, $4, $5, $6, }
  ;

/* charset -> [ CHARSET_SYM STRING ';' ]? */
charset: 
  | CHARSET_SYM STRING SEMICOLON { Some $2 }
  | { NONE }
  ;

/* import_list -> [ import [ CDO S* | CDC S* ]* ]* */
import_list: 
  | import_item { Some $1 }
  | import_list import_item { Some ($1 :: $2) }
  | { None }
  ;

/* import_item -> import [ CDO S* | CDC S* ]* */
import_item: 
  import cdo_cdc_space_list { $1 }
  ;

/* import -> IMPORT_SYM S* [STRING|URI] S* media_list? ';' S* */
import: 
  IMPORT_SYM space_or_empty string_or_uri space_or_empty media_query_list SEMICOLON
    {
      $$ = {}
      if ($3 !== null) $$["import"] = $3;
      if ($5 !== null) $$["mediaqueries"] = $5;
    }
  ;

/* [STRING|URI] */
string_or_uri
  : STRING 			-> $1
  | URI				-> $1
  ;

/* media_query_list -> [ medium [ ',' S* medium]* ]? */
media_query_list
  : medium medium_list
    %{
      $$ = [];
      if ($1 !== null) $$.push($1);
      if ($2 !== null) {
        var r = $$;
        $2.forEach(function(e) {
          r.push(e);
        });
      }
    %}
  | -> null
  ;

/* medium_list -> [ ',' S* medium]* */
medium_list
  : ',' space_or_empty medium
    %{
      $$ = [];
      if ($3 !== null) $$.push($3);
    %}
  | medium_list ',' space_or_empty medium 
    %{
      $$ = [];
      if ($1 !== null) $$ = S1;
      if ($4 !== null) $$.push($4);
    %}
  | -> null
  ;

/* medium -> IDENT S* */
medium
  : IDENT space_or_empty	-> $1
  ;

/* ruleset_list -> [ ruleset [ CDO S* | CDC S* ]* ]* */
ruleset_list
  : ruleset_item -> $1
  | ruleset_list ruleset_item
    %{
      $$ = $1;
      var r = $$;
      $2 !== null? $2.forEach(function(e) { r.push(e); }) : ''
    %}
  | -> null
  ;

/* ruleset_item -> ruleset [ CDO S* | CDC S* ]* */
ruleset_item
  : ruleset cdo_cdc_space_list   	-> $1
  ;

/* ruleset -> selector [ ',' S* selector ]* '{' S* declaration? [ ';' S* declaration? ]* '}' S* */
ruleset
  : selector selector_list '{' space_or_empty declarations declaration_list '}' space_or_empty
    %{
      $$ = [];
      if ($1 !== null) {
        var s = {};
        s.selector = $1;
        s.declaration = [];
        $5 !== null? s.declaration.push($5) : '';
        $6 !== null? $6.forEach(function(d) { s.declaration.push(d); }) : ''
        $$.push(s);
      }
      if ($2 !== null) {
        var r = $$;
        $2.forEach(function(e) {
          var s = {};
          s.selector = e;
          s.declaration = [];
          $5 !== null? s.declaration.push($5) : '';
          $6 !== null? $6.forEach(function(d) { s.declaration.push(d); }) : ''
          r.push(s);
        });
      }
    %}
  ;

/* rulesets -> ruleset* */
rulesets
  : ruleset 			-> $1
  | rulesets ruleset
    %{
      $$ = [];
      $$ = $1;
      var r = $$;
      $2 !== null? $2.forEach(function(e) { r.push(e); }) : ''
    %}
  | -> null
  ;

/* selector_list -> [ ',' S* selector ]* */
selector_list
  : ',' space_or_empty selector
    %{
      $$ = [];
      $$.push($3);
    %}
  | selector_list ',' space_or_empty selector
    %{
      $$ = [];
      $$ = $1;
      if ($4 !== null) $$.push($4);
    %}
  | -> null
  ;

/* selector -> simple_selector [ combinator selector | S+ [ combinator? selector ]? ]? */
selector
  : simple_selector space_or_empty 				-> $1			/* modified: the space_or_empty is not defined in the spec */
  | simple_selector combinator selector				-> $1 + $2 + $3
  | simple_selector at_least_one_space selector			-> $1 + ' ' + $3
  | simple_selector at_least_one_space combinator selector	-> $1 + $3 + $4		/* TODO: should i add the space like above production? */
  ;

/* simple_selector
  : element_name [ HASH | class | attrib | pseudo ]*
  | [ HASH | class | attrib | pseudo ]+
*/
simple_selector
  : element_name simple_selector_atom_list1 	-> $1 + $2
  | simple_selector_atom_list2			-> $1
  ;

/* simple_selector_atom_list1 -> [ HASH | class | attrib | pseudo ]* */
simple_selector_atom_list1
  : simple_selector_atom					-> $1
  | simple_selector_atom_list1 simple_selector_atom 		-> $1 + $2
  | -> ""
  ; 

/* simple_selector_atom_list2 -> [ HASH | class | attrib | pseudo ]+ */
simple_selector_atom_list2
  : simple_selector_atom					-> $1
  | simple_selector_atom_list2 simple_selector_atom 		-> $1 + $2
  ; 

/* simple_selector_atom -> [ HASH | class | attrib | pseudo ] */
simple_selector_atom
  : HASH 	-> $1
  | class 	-> $1
  | attrib 	-> $1
  | pseudo 	-> $1
  ;

/* element_name -> IDENT | '*' */
element_name
  : IDENT 	-> $1
  | '*' 	-> $1
  ;

/* class -> '.' IDENT */
class
  : '.' IDENT 	-> $1 + $2
  ;

/* attrib -> '[' S* IDENT S* [ [ '=' | INCLUDES | DASHMATCH ] S* [ IDENT | STRING ] S* ]? ']' */
attrib
  : '[' space_or_empty IDENT space_or_empty ']' -> $1 + $3 + $5
  | '[' space_or_empty IDENT space_or_empty attrib_operator space_or_empty attrib_value space_or_empty ']' -> $1 + $3 + $5 + $7 + $9
  ;

/* attrib_operator -> [ '=' | INCLUDES | DASHMATCH ] */
attrib_operator
  : '='			-> $1
  | INCLUDES		-> $1
  | DASHMATCH		-> $1
  ;

/* attrib_value -> [ IDENT | STRING ] */
attrib_value
  : IDENT		-> $1
  | STRING		-> $1
  ;

/* pseudo : ':' [ IDENT | FUNCTION S* [IDENT S*]? ')' ] */
pseudo
  : ':' IDENT -> $1 + $2
  | ':' FUNCTION space_or_empty ')' -> $1 + $2 + $4
  | ':' FUNCTION space_or_empty IDENT space_or_empty ')' -> $1 + $2 + $4 + $6
  ;

/* declarations -> [ property ':' S* expr prio? ]? */
declarations
  : property ':' space_or_empty expr
    %{
      $$ = {};
      $$.key = $1;
      $$.value = $4;
    %}
  | property ':' space_or_empty expr prio
    %{
      $$ = {};
      $$.key = $1;
      $$.value = $4 + ' ' + $5;				/* TODO: should i need to add a space */
    %}
  | -> null
  ;

/* declaration_list -> [ ';' S* declaration? ]* */
declaration_list
  : ';' space_or_empty declarations
    %{
      $$ = [];
      if ($3 !== null) $$.push($3);
    %}
  | declaration_list ';' space_or_empty declarations
    %{
      $$ = [];
      $$ = $1;
      if ($4 !== null) $$.push($4);
    %}
  | -> null
  ;

/* property -> IDENT S* */
property
  : IDENT space_or_empty 	-> $1
  | VENDOR space_or_empty 	-> $1
  ;

/* expr -> term [ operator? term ]* */
expr
  : term term_list
    %{
      $$ = $1;
      if ($2 !== null) $$ = $1 + ' ' + $2;
    %}
  ;
term_list
  : term -> $1
  | operator term -> $1 + $2
  | term_list term -> $1 + ' ' + $2
  | term_list operator term -> $1 + ' ' + $2 + $3
  | -> null
  ;

/*
term
  : unary_operator?
    [ NUMBER S* | PERCENTAGE S* | LENGTH S* | EMS S* | EXS S* | ANGLE S* |
      TIME S* | FREQ S* ]
  | STRING S* | IDENT S* | URI S* | hexcolor | function
  ;
*/
term
  : numeric_term 			-> $1
  | unary_operator numeric_term 	-> $1 + $2
  | string_term				-> $1
  ;
numeric_term
  : NUMBER space_or_empty               -> $1 
  | PERCENTAGE space_or_empty           -> $1 
  | LENGTH space_or_empty               -> $1
  | EMS space_or_empty                  -> $1
  | EXS space_or_empty                  -> $1
  | ANGLE space_or_empty                -> $1
  | TIME space_or_empty                 -> $1
  | FREQ space_or_empty                 -> $1
  ;
string_term
  : STRING space_or_empty		-> $1
  | IDENT space_or_empty                -> $1
  | URI space_or_empty                  -> $1
  | hexcolor space_or_empty             -> $1
  | function space_or_empty 		-> $1
  ;

/* prio -> IMPORTANT_SYM S* */
prio
  : IMPORTANT_SYM space_or_empty	-> $1
  ;

/* function -> FUNCTION S* expr ')' S* */
function
  : FUNCTION space_or_empty expr ')' space_or_empty -> $1 + $3 + $4
  ;

/*
* There is a constraint on the color that it must
* have either 3 or 6 hex-digits (i.e., [0-9a-fA-F])
* after the "#"; e.g., "#000" is OK, but "#abcd" is not.
* hexcolor -> HASH S*
*/
hexcolor
  : HASH space_or_empty 	-> $1
  ;

/* media_list -> [ media [ CDO S* | CDC S* ]* ]* */
media_list
  : media_item -> $1
  | media_list media_item
    %{
      $$ = $1;
      var r = $$;
      $2 !== null? $2.forEach(function(e) { r.push(e); }) : ''
    %}
  | -> null
  ;

/* media_item -> media [ CDO S* | CDC S* ]* */
media_item
  : media cdo_cdc_space_list          -> $1
  ;

/* media -> MEDIA_SYM S* media_list '{' S* ruleset* '}' S* */
media
  : MEDIA_SYM space_or_empty media_query_list '{' space_or_empty rulesets '}' space_or_empty
    %{
      $$ = {}
      if ($3 !== null) $$["mediaqueries"] = $3;
      if ($6 !== null) $$["rulesets"] = $6;
    %}
  ;

/* page_list -> [ page [ CDO S* | CDC S* ]* ]* */
page_list
  : page_item -> $1
  | page_list page_item
    %{
      $$ = $1;
      var r = $$;
      $2 !== null? $2.forEach(function(e) { r.push(e); }) : ''
    %}
  | -> null
  ;

/* page_item -> page [ CDO S* | CDC S* ]* */
page_item
  : page cdo_cdc_space_list   	-> $1
  ;

/* page -> PAGE_SYM S* pseudo_page? '{' S* declaration? [ ';' S* declaration? ]* '}' S* */
page
  : PAGE_SYM space_or_empty pseudo_pages '{' space_or_empty declarations declaration_list '}' space_or_empty
    %{
      $$ = {};
      var s = $$;

      $3 !== null? $$.pseudo_class = $3 : '';
      $$.declaration = [];
      $6 !== null? $$.declaration.push($6) : '';
      $7 !== null? $7.forEach(function(d) { s.declaration.push(d); }) : ''
    %}
  ;

/* pseudo_page -> ':' IDENT S* */
pseudo_pages
  : ':' IDENT space_or_empty -> $2
  | -> null
  ;

/* S+ */
at_least_one_space
  : S				-> " "
  | at_least_one_space S 	-> " "
  ;

/* S* */
space_or_empty
  : at_least_one_space 		-> $1
  | -> ""
  ;

/* [S|CDO|CDC]* */
space_cdata_list
  : space_cdata				-> null
  | space_cdata_list space_cdata	-> null
  | -> null
  ;

/* S|CDO|CDC */
space_cdata
  : S			-> null
  | CDO			-> null
  | CDC			-> null
  ;

/* cdo_cdc_space_list -> [ CDO S* | CDC S* ]* */
cdo_cdc_space_list
  : cdo_cdc_space_empty			   -> null
  | cdo_cdc_space_list cdo_cdc_space_empty -> null
  | -> null
  ;

/* cdo_cdc_space_empty -> CDO S* | CDC S* */
cdo_cdc_space_empty
  : CDO space_or_empty		-> null
  | CDC space_or_empty     	-> null
  ;

/* combinator -> '+' S* | '>' S* */
combinator
  : '+' space_or_empty		-> $1
  | '>' space_or_empty		-> $1
  ;

/* unary_operator -> '-' | '+' */
unary_operator
  : '+' 			-> $1
  | '-'				-> $1
  ;

/* operator -> '/' S* | ',' S* */
operator
  : '/' space_or_empty		-> $1
  | ',' space_or_empty 		-> $1
  ;