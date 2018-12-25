open Types

let hasFractionalPart (num:float): bool = match modf num with 
  | (0., _) -> false 
  | _ -> true;;

let printOpenerBlockType = function
  | Paren -> "("
  | SquareBracket -> "["
  | Brace -> "{";;

let printClosingBlockType = function
  | Paren -> ")"
  | SquareBracket -> "]"
  | Brace -> "}";;

type blockType = Paren | SquareBracket | Brace [@@deriving yojson, show];;

let rec print_cvalue ?(wrap=true) = function
  | String (ch, str) -> if wrap then Printf.sprintf "%c%s%c" ch str ch else str
  | Ident str -> str
  | Number i -> i
  | Percentage i -> if hasFractionalPart i then (string_of_float i) ^ "%" else (string_of_int (int_of_float i)) ^ "%"
  | Uri str -> "url(" ^ str ^ ")"
  | HexColor str -> str 
  | Dimension (num, str) -> Printf.sprintf "%f%s" num str
  | Func _ -> "TODO func"
  | UnicodeRange str -> str
  | Hash str -> "#" ^ str
  | Block { token; value; pos } -> String.concat "n" [
    (printOpenerBlockType token);
    (List.fold_left (fun a b -> (a ^ (print_cvalue b))) "" value);
    (printClosingBlockType token);
  ]
  ;;

let rec printCValueList ?(sep=" ") (expressions: component_value list) =
  expressions
  |> List.map (print_cvalue)
  |> String.concat sep

let print_comment ({value;}: comment) = "/*" ^ value ^ "*/";;

let print_declaration ({name; value; important;}: declaration) = 
  Printf.sprintf "  %s: %s%s;" name (printCValueList value) (if important then " !important" else "");;

let print_declarations (rules: (declaration or_comment) list) =
  let printDeclarationOrComment (term:declaration or_comment)= match term with 
    | Comment c -> "  " ^ (print_comment c)
    | Else declaration -> print_declaration declaration
  in
  rules
  |> List.map printDeclarationOrComment
  |> String.concat "\n"
;;

let printAtRule {name; prelude; block;} = Printf.sprintf "@%s %s;" name (printCValueList prelude);;
let printStyleRule {prelude; declarations;} = 
  Printf.sprintf "%s {\n%s\n}" (prelude |> List.map printCValueList |> String.concat ",\n") (print_declarations declarations)

let printRule (rule: rule) = match rule with 
  | Comment c -> print_comment c
  | AtRule r -> printAtRule r
  | StyleRule r -> printStyleRule r
  ;;

let optToStr = function None -> "" | Some s -> "" ^ s ^" ";;

let print_selectors selectors = String.concat ",\n" selectors;;

let positionToJson (position:position): Yojson.Safe.json = match position with
  (start, finish) -> `Assoc [
    ("start", `Assoc [
      ("line", `Int start.pos_lnum);
      ("column", `Int (start.Lexing.pos_cnum - start.Lexing.pos_bol + 1))
    ]);
    ("end", `Assoc [
      ("line", `Int finish.pos_lnum);
      ("column", `Int (finish.Lexing.pos_cnum - finish.Lexing.pos_bol + 1))
    ]);
    ("source", `String start.pos_fname )
  ]

let termsToJson (value:component_value list) = match value with
  _ -> `String (String.concat " " (List.map print_cvalue value))
;;

let commentToJson ({value; pos}:comment) = 
  `Assoc [ 
     ("type", `String "comment");
     ("comment", `String value );
     ("position", positionToJson pos);
    ]
 ;;

let ruleToJson (rule:(declaration or_comment)): Yojson.Safe.json = match rule with
   | Comment c -> commentToJson c
   | Else ({name; value; pos}) -> `Assoc [ 
     ("type", `String "declaration");
     ("property", `String name );
     ("value", termsToJson value );
     ("position", positionToJson pos);
    ]
 ;;

let rulesetToJson (ruleset:rule): Yojson.Safe.json = match ruleset with
 | Comment c -> commentToJson c
 | AtRule ({name; prelude; block; pos;}) -> `Assoc [
   ("type", `String name);
   (* (str, `String( (optToStr prefix) ^ (print_cvalue v))); *)
   ("position", positionToJson pos); 
 ]
 | StyleRule {prelude; declarations; pos;} ->
  `Assoc [
   ("type", `String "rule");
   ("selectors", `List (List.map (fun s -> `String (printCValueList s)) prelude ));
   ("declarations", `List (List.map ruleToJson declarations));
   ("position", positionToJson pos);
  ]
;;

let rec rulesetsToJson (stylesheet:stylesheet): Yojson.Safe.json =  match stylesheet with 
 | [] -> `List []
 | (hd::tl) -> `List ((rulesetToJson hd) :: (List.map rulesetToJson tl))
 ;;

let stylesheetToJson (stylesheet:stylesheet):Yojson.Safe.json =
  `Assoc [
    ("type", `String "stylesheet");
    ("stylesheet", 
      `Assoc [ (
        "rules", (rulesetsToJson stylesheet)
      )]
    )
  ]
;;


let astPrint (rulesets:stylesheet): string = 
  Yojson.Safe.pretty_to_string ~std:true (stylesheetToJson rulesets);;

let prettyPrint (stylesheet:stylesheet) =
  (stylesheet
    |> List.map (fun ruleset -> printRule ruleset)
    |> String.concat "\n\n") ^ "\n"
;;
