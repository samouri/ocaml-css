open Lib.Types
open Cmdliner

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

let print = prettyPrint;;

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

type output_t = Pretty | Errors | AST;;

exception ArgException of string
let str_of_output = function
  | Pretty -> "pretty"
  | Errors -> "errors"
  | AST -> "ast";;

let output_of_str o = match (String.lowercase_ascii o) with
  | "pretty" -> Pretty
  | "errors"  -> Errors
  | "ast" -> AST
  | s -> raise (ArgException (Printf.sprintf "invalid string for output type: %s" s))
  ;;

let output =
  let doc = "Pick what ocaml-css should do: pretty print, astprint, or check for errors" in
   Arg.(value & opt string "pretty" & info ["o"; "output"] ~docv:"OUTPUT" ~doc)

let file =
  let doc = "The file " in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"FILE" ~doc)

let info =
  let doc = "parse a css document in order to detect errors" in
  let man = [
    `S Manpage.s_bugs;
    `P "Open bug reports as issues on the GitHub repo at https://github.com/samouri/ocaml-css." ]
  in
  Term.info "css" ~version:"%%VERSION%%" ~doc ~exits:Term.default_exits ~man
  ;;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
  ;;

let run file output_str =
  let css_str = load_file file in
  let parsed = parse css_str in
  let output_flag = output_of_str output_str in (* learn how to make the proper CMDLiner converter for this *)
  let output = if output_flag = Pretty then
    prettyPrint parsed
    else
    Yojson.Safe.pretty_to_string ~std:true (rulesets_to_yojson parsed)
  in
  print_endline output
  ;;

let css_t = Term.(const run $ file $ output)
let () = Term.exit @@ Term.eval (css_t, Term.info "css-parse")


