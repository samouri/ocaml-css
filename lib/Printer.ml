open Types

let print_term (term: term) = match term with
  | String str -> "\"" ^ str ^ "\""
  | Ident str -> "\"" ^ str ^ "\""
  | URI str -> "\"" ^ str ^ "\""
  | HexColor str -> "\"" ^ str ^ "\""
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
  |> List.map (fun (prop, expr) -> Printf.sprintf " %s: %s;" prop (print_expr expr))
  |> String.concat " \n"
;;

let print_selectors selectors = String.concat ",\n" selectors;;

let print_ruleset (selectors, rules) =
  Printf.sprintf "%s {\n%s\n}" (print_selectors selectors) (print_rules rules)
  ;;

let prettyPrint (rulesets:rulesets) = match rulesets with
  | None -> "There is no css!"
  | Some r -> r
    |> List.map (fun ruleset -> print_ruleset ruleset)
    |> String.concat "\n\n"
;;