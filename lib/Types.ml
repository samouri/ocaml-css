type operator = NoOp | Slash | Comma;;

type term = 
  | Dimension of float * string
  | String of string
  | Ident of string
  | URI of string
  | HexColor of string
  | Func of expression

and statement = Term of term | Operator of operator
and expression = statement list
;;

type selectors = string list;;

type rule = string * (term list);;
type ruleset = selectors * (rule list);;
type rulesets = (ruleset list) option;;