type operator = NoOp | Slash | Comma [@@deriving show, yojson];;

type lexing_position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} [@@deriving yojson,show]
;;

type position = lexing_position * lexing_position [@@deriving yojson, show];;

type term =
  | Dimension of float * string
  | String of string
  | DoubleString of string
  | Ident of string
  | Number of string
  | Percentage of float
  | URI of string
  | HexColor of string
  | Func of expression [@@deriving show, yojson]

and statement =
 | Term of term
 | Operator of operator [@@deriving show, yojson]

and expression = statement list [@@deriving show, yojson]
;;

type selectors = string list [@@deriving show, yojson];;
type comment = string * position [@@deriving show, yojson];;


type rule = string * (term list) * position [@@deriving show, yojson];;

type ruleset_item = 
  | Rule of rule 
  | RComment of comment 
  [@@deriving show, yojson];;

type ruleset = selectors * (ruleset_item list) * position [@@deriving show, yojson];;
type stylesheet_item = Ruleset of ruleset | SComment of comment [@@deriving show, yojson];; 

type stylesheet = (stylesheet_item list) [@@deriving show, yojson];;
