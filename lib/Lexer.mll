{
open Parser
open Types

(*
 * Some code taken from Real World OCaml v2: https://dev.realworldocaml.org/parsing-with-ocamllex-and-menhir.html 
 *)
exception SyntaxError of string

let next_line (lexbuf:Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1;
    }
;;

let tail1 s = String.sub s 1 ((String.length s) - 1)
and head1 s = String.sub s 0 ((String.length s) - 1)
and inner s = String.sub s 1 ((String.length s) - 2)

let split_dimension s =
  let div = ref 0
  and len = String.length s
  and num = (function '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '+' | '-' | '.' -> true | _ -> false) in
  while !div < len && num s.[!div] do
    incr div
  done;
  DIMENSION(float_of_string(String.sub s 0 !div), String.sub s !div (len - !div))

(* Save space by sharing copies of the most common kewords. *)
(* XXX CSS unquoted words are case-insensitive. XML element names are
   case-sensitive. Is String.lowercase_ascii inappropriate for XML? *)
let intern s =
  (match String.lowercase_ascii s with "none" -> "none" | "normal" -> "normal" | s -> s)

let doident lexeme =
  let l = String.length lexeme in
  if lexeme.[l-1] = '(' then FUNCTION(head1 lexeme) else IDENT(intern lexeme)

let donumber lexeme =
  let l = String.length lexeme in
  if lexeme.[l-1] = '%' then
    PERCENTAGE(float_of_string(String.sub lexeme 0 (l-1)))
  else
    NUMBER(lexeme)

let atkeyword lexeme = ATKEYWORD(String.lowercase_ascii(tail1 lexeme));;

(* remove first two characters as well as the last two *)
let extractComment (comment:string) : string = 
  String.sub comment 2 ((String.length comment) - 4);;
  

let last_comments = ref [];;
let comments () = List.rev !last_comments;;
let clearComments () = last_comments := [];;

let decode_unicode s =
  let l = String.length s in
  let rv = int_of_string ("0x" ^ (String.sub s 1 (l-1))) in
  assert (rv >= 0 && rv < 256);
  char_of_int rv

  (* XXX Skip whitespace? *)
let uri lexeme =
  let l = String.length lexeme in
  URI(String.sub lexeme 4 (l-5))
}
let hex = [ '0' - '9' 'a' - 'f' ]
let HEX = [ '0' - '9' 'A' - 'F' ]
(* Technically only 1-6 hex characters are allowed but the lex tables
   are much smaller when + is used. *)
let css_unicode = '\\' (HEX | hex)+ ['\n' '\r' '\t' '\012']?
let css_escape = css_unicode | '\\' [ ' ' - '~' ] (* \200-\4177777 omitted *)
let css_nonascii = [^ '\000' - '\255' ]
let css_nmstart = ['a' - 'z' 'A' - 'Z' ] | css_nonascii | css_unicode
let css_nmchar = ['a' - 'z' 'A' - 'Z' '0' - '9' '-' ] | css_nonascii | css_unicode
(* let css_name = css_nmchar + *)
let css_ident = css_nmstart css_nmchar*
let css_num = ['0' - '9']+ | ['0' - '9']* '.' ['0' - '9']+
let css_nl = '\n' | '\r' '\n' | '\r' | '\012'
let css_string1 = '\034' ( [ '\t' ' ' '!' '#' '$' '%' '&' '(' - '~' ] | '\\' css_nl | '\'' | css_nonascii | css_escape )* '\034'
let css_string2 = '\039' ( [ '\t' ' ' '!' '#' '$' '%' '&' '(' - '~' ] | '\\' css_nl | '\034' | css_nonascii | css_escape )* '\039'
let css_string = css_string1 | css_string2
let css_w = [' ' '\t' '\r' '\012']*
let css_s = [' ' '\t' '\r' '\012']+
let css_comment = '/' '*' [^ '*']* '*'+ ([^'/''*'][^'*']* '*'+)* '/'
let css_number = ['+' '-']? css_num '%'?
let css_dimension = ['+' '-']? css_num css_ident

(* The CSS spec defines a grammar which allows just about any combination
   of tokens and advises against using the real grammar in appendix D. *)

rule css =
  parse
    (* Skip over CDO, CDC, and comments. *)
    | "<!--" | "-->" | css_comment { 
      let commentText = extractComment (Lexing.lexeme lexbuf) in
      let newComment = {value=commentText; pos=(Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf);} in
      last_comments := newComment :: !last_comments; 
      css lexbuf
    }
    (*| '#' css_ident { HASH(tail1(Lexing.lexeme lexbuf)) }*)
    | '@' css_ident { atkeyword(Lexing.lexeme lexbuf) }
    | css_ident '(' [^')']* ')' { FUNCTION(Lexing.lexeme(lexbuf)) }
    | css_ident '('? { doident (Lexing.lexeme lexbuf) }
    (* XXX String processing: strip quotes; delete backslash-newline *)
    | css_string { 
      (* store which character was used to create the string. either '' or "" *)
      let str = Lexing.lexeme lexbuf in
      STRING(str.[0], inner(str))
    }
    | css_nl { next_line lexbuf; css lexbuf }
    | css_number { donumber(Lexing.lexeme lexbuf) }
    | css_dimension css_num css_ident { split_dimension(Lexing.lexeme lexbuf) }
     (* XXX Should be case-insentive? *)
    | "url(" css_w css_ident css_w ')'
    | "url(" css_w (['!' '#' '$' '%' '&' - '~' ] | css_nonascii | css_escape)* css_w ')' { uri(Lexing.lexeme lexbuf) }
    | "U+" (HEX | '?')+ ('-' HEX+)? { UNICODE(0,0) }
    | '*' { STAR }
    | '.' { DOT }
    | '#' { HASH }
    | ',' { COMMA }
    | '[' { LSQUARE }
    | ']' { RSQUARE }
    | ':' { COLON }
    | '>' { DELIM(">") } 
    | '{' { LBRACE }
    | '}' { RBRACE }
    | ';' { SEMICOLON }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '!' { EXCLAMATION }
    | css_s { S }
    | "||" { DELIM("||") }
    | "~=" { DELIM("~=") }
    | "|=" { DELIM("|=") }
    | eof { EOF }
    | _ { DELIM(Lexing.lexeme lexbuf) }
