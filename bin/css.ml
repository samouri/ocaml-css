open Lib.Types

let test = "
h1 {
  font-family: \"Times New Roman\";
  font-size: 12px;
}

.classnames, h2 {
  hello: 'vent';
} 

.hello .world {}
";;


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

let printAST (rulesets:rulesets) = match rulesets with
  | None -> "There is no css!"
  | Some r -> r 
    |> List.map (fun ruleset -> print_ruleset ruleset)
    |> String.concat "\n\n"
;;

let print = printAST;;

let parse str =
  let lexbuf = Lexing.from_string str in
  try
    Lib.Parser.stylesheet Lib.Lexer.css lexbuf
  with exn -> ( 
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    (print_endline (Printf.sprintf "Error at location (%d:%d) with character: %s" line cnum tok));
    raise exn
  )

let parsed = parse test;;

print_endline test;; 
print_endline "\n---------------\n";;
print_endline (printAST parsed);;