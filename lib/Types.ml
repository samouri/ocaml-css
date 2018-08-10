type operator = NoOp | Slash | Comma [@@deriving show, yojson];;

type position = Lexing.position * Lexing.position;;
type term =
  | Dimension of float * string
  | String of string
  | Ident of string
  | URI of string
  | HexColor of string
  | Func of expression [@@deriving show, yojson]

and statement =
 | Term of term
 | Operator of operator [@@deriving show, yojson]

and expression = statement list [@@deriving show, yojson]
;;

type selectors = string list [@@deriving show, yojson];;

type rule = string * (term list) [@@deriving show, yojson];;
type ruleset = selectors * (rule list) [@@deriving show, yojson];;
type rulesets = (ruleset list) option [@@deriving show, yojson];;
