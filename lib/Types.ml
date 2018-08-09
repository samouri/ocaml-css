type operator = NoOp | Slash | Comma;;

type position = Lexing.position * Lexing.position;;
type term = 
  | Dimension of float * string
  | String of string
  | Ident of string
  | URI of string
  | HexColor of string
  | Func of expression 
  [@@deriving yojson show]

and statement = 
 | Term of term 
 | Operator of operator 
 [@@deriving yojson show]

and expression = statement list
;;

type selectors = string list;;

type rule = string * (term list);;
type ruleset = selectors * (rule list);;
type rulesets = (ruleset list) option [@@deriving yojson show];;