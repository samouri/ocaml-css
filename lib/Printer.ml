open Types

let hasFractionalPart (num : float) : bool =
  match modf num with 0., _ -> false | _ -> true

let printOpenerBlockType = function
  | Paren -> "("
  | SquareBracket -> "["
  | Brace -> "{"

let printClosingBlockType = function
  | Paren -> ")"
  | SquareBracket -> "]"
  | Brace -> "}"

type blockType = Paren | SquareBracket | Brace [@@deriving yojson, show]

let rec print_cvalue ?(wrap = true) = function
  | String (ch, str) -> if wrap then Printf.sprintf "%c%s%c" ch str ch else str
  | Ident str -> str
  | Number i -> i
  | Percentage i ->
      if hasFractionalPart i then string_of_float i ^ "%"
      else string_of_int (int_of_float i) ^ "%"
  | Uri str -> "url(" ^ str ^ ")"
  | HexColor str -> str
  | Delim d -> d
  | Dimension (num, str) -> Printf.sprintf "%f%s" num str
  | Func f -> f
  | UnicodeRange str -> str
  | Hash str -> "#" ^ str
  | Block {token; value; pos} ->
      String.concat ""
        [ printOpenerBlockType token
        ; List.fold_left (fun a b -> a ^ print_cvalue b) "" value
        ; printClosingBlockType token ]

(*Gross hack here rn. Basically almost all cvalues get pprinted by space separators except for blocks which
  should not be separated. So if we are at last elem we know no space, and also if we know next elem isblock then no space
 *)
let rec printCValueList ?(sep = " ") (expressions : component_value list) =
  let isBlock = function Block _ -> true | _ -> false in
  let isDelim = function Delim _ -> true | _ -> false in
  let next i lst = List.nth lst (i + 1) in
  expressions
  |> List.mapi (fun (i : int) (cval : component_value) ->
         print_cvalue cval
         ^
         if
           i + 1 >= List.length expressions
           || isBlock (next i expressions)
           || isDelim (next i expressions)
           || isDelim cval
         then ""
         else sep )
  |> String.concat ""

let print_comment ({value} : comment) = "/*" ^ value ^ "*/"

let print_declaration ({name; value; important} : declaration) =
  Printf.sprintf "  %s: %s%s;" name (printCValueList value)
    (if important then " !important" else "")

let print_declarations (rules : declaration orComment list) =
  let printDeclarationOrComment (term : declaration orComment) =
    match term with
    | Comment c -> "  " ^ print_comment c
    | Else declaration -> print_declaration declaration
  in
  rules |> List.map printDeclarationOrComment |> String.concat "\n"

let printAtRule {name; prelude; block} =
  let preludeStr = prelude |> List.map printCValueList |> String.concat ", " in
  Printf.sprintf "@%s %s;" name preludeStr

let rec printSelector = function
  | Simple selector -> printCValueList selector
  | Compound selector -> printCValueList ~sep:"" selector
  | Complex selectors ->
      selectors |> List.map printSelector |> String.concat " "

let printSelectors selectors = selectors 
  |> List.map printSelector 
  |> String.concat ",\n"

let printStyleRule {prelude : Types.selector list; declarations} =
  let selectors = printSelectors prelude in
  let declarationsS = print_declarations declarations in
  Printf.sprintf "%s {\n%s\n}" selectors declarationsS

let printRule (rule : rule) =
  match rule with
  | Comment c -> print_comment c
  | AtRule r -> printAtRule r
  | StyleRule r -> printStyleRule r

let optToStr = function None -> "" | Some s -> "" ^ s ^ " "

let positionToJson (position : position) : Yojson.Safe.json =
  match position with start, finish ->
    `Assoc
      [ ( "start"
        , `Assoc
            [ ("line", `Int start.pos_lnum)
            ; ( "column"
              , `Int (start.Lexing.pos_cnum - start.Lexing.pos_bol + 1) ) ] )
      ; ( "end"
        , `Assoc
            [ ("line", `Int finish.pos_lnum)
            ; ( "column"
              , `Int (finish.Lexing.pos_cnum - finish.Lexing.pos_bol + 1) ) ]
        )
      ; ("source", `String start.pos_fname) ]

let termsToJson (value : component_value list) =
  match value with _ ->
    `String (String.concat " " (List.map print_cvalue value))

let commentToJson ({value; pos} : comment) =
  `Assoc
    [ ("type", `String "comment")
    ; ("comment", `String value)
    ; ("position", positionToJson pos) ]

let ruleToJson (rule : declaration orComment) : Yojson.Safe.json =
  match rule with
  | Comment c -> commentToJson c
  | Else {name; value; pos} ->
      `Assoc
        [ ("type", `String "declaration")
        ; ("property", `String name)
        ; ("value", termsToJson value)
        ; ("position", positionToJson pos) ]

let rulesetToJson (ruleset : rule) : Yojson.Safe.json =
  match ruleset with
  | Comment c -> commentToJson c
  | AtRule {name; prelude; block; pos} ->
      `Assoc
        [ ("type", `String name)
        ; (name, `String (prelude |> List.map printCValueList |> String.concat ", "))
        ; ("position", positionToJson pos) ]
  | StyleRule {prelude; declarations; pos} ->
      `Assoc
        [ ("type", `String "rule")
        ; ( "selectors"
          , `List (List.map (fun s -> `String (printSelector s)) prelude) )
        ; ("declarations", `List (List.map ruleToJson declarations))
        ; ("position", positionToJson pos) ]

let rec rulesetsToJson (stylesheet : stylesheet) : Yojson.Safe.json =
  match stylesheet with
  | [] -> `List []
  | hd :: tl -> `List (rulesetToJson hd :: List.map rulesetToJson tl)

let stylesheetToJson (stylesheet : stylesheet) : Yojson.Safe.json =
  `Assoc
    [ ("type", `String "stylesheet")
    ; ("stylesheet", `Assoc [("rules", rulesetsToJson stylesheet)]) ]

let astPrint (rulesets : stylesheet) : string =
  Yojson.Safe.pretty_to_string ~std:true (stylesheetToJson rulesets)

let prettyPrint (stylesheet : stylesheet) =
  ( stylesheet
  |> List.map (fun ruleset -> printRule ruleset)
  |> String.concat "\n\n" )
  ^ "\n"
