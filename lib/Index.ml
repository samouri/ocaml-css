let print = Printer.prettyPrint;;
let astPrint = Printer.astPrint;;

let parse  (source: string) =
  (print_string source);
  (print_endline ("HELLO"));
  let getLast lst = List.hd (List.rev lst) in
  let lexbuf = Lexing.from_string source in
  (lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = getLast (String.split_on_char '/' "") } );
  try
    Parser.stylesheet Lexer.css lexbuf
  with exn -> (
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol + 1 in
    let tok = Lexing.lexeme lexbuf in
    (print_endline (Printf.sprintf "Error at location (%d:%d) with character: %s" line cnum tok));
    raise exn
  )