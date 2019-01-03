type operator = NoOp | Slash | Comma [@@deriving show, yojson]

type lexing_position = Lexing.position =
  {pos_fname: string; pos_lnum: int; pos_bol: int; pos_cnum: int}
[@@deriving yojson, show]

type position = lexing_position * lexing_position [@@deriving yojson, show]

type blockType = Paren | SquareBracket | Brace [@@deriving yojson, show]

type block = {token: blockType; value: component_value list; pos: position}
[@@deriving yojson, show]

and component_value =
  | Ident of string
  | Func of string
  | Hash of string
  | Number of string
  | Dimension of float * string
  | Delim of string
  | Percentage of float
  | UnicodeRange of string
  | String of char * string
  | Uri of string
  | HexColor of string
  | Block of block
[@@deriving show, yojson]

type atPrelude = component_value list list [@@deriving show, yojson]

(* need to differentiate between single and complex because the components of
  compound are not space separated *)
type selector =
  (* Simple or Combinator *)
  | Simple of component_value list
  | Compound of component_value list
  | Complex of selector list
[@@deriving show, yojson]

type comment = {value: string; pos: position} [@@deriving show, yojson]

type 'a orComment = Comment of comment | Else of 'a [@@deriving show, yojson]

type atrule =
  {name: string; prelude: atPrelude; block: block option; pos: position}
[@@deriving show, yojson]

type declaration =
  {name: string; value: component_value list; important: bool; pos: position}
[@@deriving show, yojson]

(* type declarationOrComment = 
  | Comment of comment 
  | Declaration of declaration [@@deriving show, yojson];; *)
type styleRule =
  { prelude: selector list
  ; declarations: declaration orComment list
  ; pos: position }
[@@deriving show, yojson]

type rule = Comment of comment | StyleRule of styleRule | AtRule of atrule
[@@deriving show, yojson]

type stylesheet = rule list [@@deriving show, yojson]
