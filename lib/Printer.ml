open Types

let print_term ?(wrap=true) (term: term) = 
  let wrapFn s = if wrap then "'" ^ s  ^ "'" else s in
  match term with
  | String str -> wrapFn str
  | Ident str -> str
  | Number i -> (string_of_int i)
  | URI str -> wrapFn str
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

let print_rules (rules: rule list) =
  rules
  |> List.map (fun (prop, expr,_) -> Printf.sprintf "  %s: %s;" prop (print_expr expr))
  |> String.concat "\n"
;;

let print_selectors selectors = String.concat ",\n" selectors;;

let print_ruleset (selectors, rules, _) =
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
  _ -> `String (String.concat "" (List.map print_term terms))
;;

let ruleToJson (rule:rule): Yojson.Safe.json = match rule with
   | (str, terms, pos) -> `Assoc [ 
     ("type", `String "declaration");
     ("property", `String str );
     ("value", termsToJson terms );
     ("position", positionToJson pos);
    ]
 ;;

let rulesetToJson (ruleset:ruleset): Yojson.Safe.json = match ruleset with 
 | (selectors, rules, pos) -> `Assoc [
   ("type", `String "rule");
   ("selectors", `List (List.map (fun s -> `String s) selectors ));
   ("declarations", `List (List.map ruleToJson rules));
   ("position", positionToJson pos);
 ]
;;

let rec rulesetsToJson (rulesets:rulesets): Yojson.Safe.json =  match rulesets with 
 | None | Some []-> `List []
 | Some (hd::tl) -> `List ((rulesetToJson hd) :: (List.map rulesetToJson tl))
 ;;

let stylesheetToJson (rulesets:rulesets):Yojson.Safe.json =
  `Assoc [
    ("type", `String "stylesheet");
    ("stylesheet", 
      `Assoc [ (
        "rules", (rulesetsToJson rulesets)
      )]
    )
  ]
;;


let astPrint (rulesets:rulesets): string = 
  Yojson.Safe.pretty_to_string ~std:true (stylesheetToJson rulesets);;

let prettyPrint (rulesets:rulesets) = match rulesets with
  | None -> "There is no css!"
  | Some r -> (r
    |> List.map (fun ruleset -> print_ruleset ruleset)
    |> String.concat "\n\n") ^ "\n"
;;
