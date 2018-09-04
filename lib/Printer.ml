open Types

let hasFractionalPart (num:float): bool = match modf num with 
  | (0., _) -> false 
  | _ -> true;;

let print_term ?(wrap=true) (term: term) = 
  let wrapFn s = if wrap then "'" ^ s  ^ "'" else s in
  let wrapDoubleFn s = if wrap then "\"" ^ s  ^ "\"" else s in
  match term with
  | String str -> wrapFn str
  | DoubleString str -> wrapDoubleFn str
  | Ident str -> str
  | Number i -> i
  | Percentage i -> if hasFractionalPart i then (string_of_float i) ^ "%" else (string_of_int (int_of_float i)) ^ "%"
  | URI str -> "url(" ^ str ^ ")"
  | HexColor str -> wrapFn str 
  | Dimension (num, str) -> Printf.sprintf "%f%s" num str
  | Func _ -> "TODO func"
    (* let op = match op with
      | None -> ""
      | Plus -> "+"
      | Minus -> "-"
    in
    Printf.sprintf "%s%d%s" op value ut *)
  ;;

let print_expr (expressions: term list) =
  expressions
  |> List.map (print_term)
  |> String.concat " "
  ;;

let print_comment ((comment, _):comment) = comment;;

let print_rule ((prop, expr, _):rule) = Printf.sprintf "  %s: %s;" prop (print_expr expr);;

let print_ruleset_item (ritem: ruleset_item)= match ritem with
  | RComment c -> "  " ^ print_comment c
  | Rule r -> print_rule r;;

let print_rules (rules: ruleset_item list) =
  rules
  |> List.map print_ruleset_item
  |> String.concat "\n"
;;


let print_selectors selectors = String.concat ",\n" selectors;;

let print_stylesheet_item  = function 
  | SComment c -> print_comment c
  | Ruleset (selectors, rules, _) ->
    Printf.sprintf "%s {\n%s\n}" (print_selectors selectors) (print_rules rules)
  ;;

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

let termsToJson (terms:term list) = match terms with
  _ -> `String (String.concat " " (List.map print_term terms))
;;

let commentToJson ((comment, pos):comment) = 
  `Assoc [ 
     ("type", `String "comment");
     ("comment", `String comment );
     ("position", positionToJson pos);
    ]
 ;;

let ruleToJson (rule:ruleset_item): Yojson.Safe.json = match rule with
   | RComment c -> commentToJson c
   | Rule (str, terms, pos) -> `Assoc [ 
     ("type", `String "declaration");
     ("property", `String str );
     ("value", termsToJson terms );
     ("position", positionToJson pos);
    ]
 ;;

let rulesetToJson (ruleset:stylesheet_item): Yojson.Safe.json = match ruleset with
 | SComment c -> commentToJson c
 | Ruleset (selectors, rules, pos ) ->
  `Assoc [
   ("type", `String "rule");
   ("selectors", `List (List.map (fun s -> `String s) selectors ));
   ("declarations", `List (List.map ruleToJson rules));
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
    |> List.map (fun ruleset -> print_stylesheet_item ruleset)
    |> String.concat "\n\n") ^ "\n"
;;
