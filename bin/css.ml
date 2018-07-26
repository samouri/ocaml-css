let test = "
h1 {
  font-family: \"Times New Roman\";
}
";;

let printAST rules = match rules with
  | None -> "There is no css!"
  | Some _ -> "We found some!"
;;

let parse str =
  let lexbuf = Lexing.from_string str in
  try
    Lib.Parser.css Lib.Lexer.css lexbuf
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
print_endline (printAST parsed);;