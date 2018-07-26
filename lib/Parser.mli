
(* The type of tokens. *)

type token = 
  | URI of (string)
  | UNICODE of (int * int)
  | STRING of (string)
  | STAR
  | SLASH
  | SEMICOLON
  | S
  | RSQUARE
  | RPAREN
  | RBRACE
  | PREFIX
  | PLUS
  | PERCENTAGE of (float)
  | PAGE_SYM
  | NUMBER of (string)
  | MINUS
  | MEDIA_SYM
  | LSQUARE
  | LPAREN
  | LBRACE
  | INVALID
  | INCLUDES
  | IMPORT_SYM
  | IMPORTANT_SYM
  | IDENT of (string)
  | HASH of (string)
  | GREATER
  | FUNCTION of (string)
  | EXCLAMATION
  | ERROR of (char)
  | EQUALS
  | EOF
  | DOT
  | DIMENSION of (float * string)
  | DASHMATCH
  | CONTAINS
  | COMMA
  | COLOR
  | COLON
  | CHILD
  | CHARSET_SYM
  | CHAR of (string)
  | CDO
  | CDC
  | ATPAGE
  | ATMEDIA
  | ATKEYWORD of (string)
  | ATIMPORT
  | ATFONTFACE
  | ATCHARSET

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val css: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Types.rules option)
