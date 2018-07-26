type unaryOperator = None | Plus | Minus;;
type dimension = unaryOperator * (float * string);;
type operator = NoOp | Slash | Comma;;

type term = Dimension of dimension
  | String of string
  | Ident of string
  | URI of string
  | HexColor of string
  | Func of expression
and statement = Term of term | Operator of operator
and expression = statement list
;;

type rule = string * expression * bool;;
type rules = rule list;;