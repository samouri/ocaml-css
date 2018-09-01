let print = Printer.prettyPrint;;
let astPrint = Printer.astPrint;;

let parse (filepath:string) =
  let getLast lst = List.hd (List.rev lst) in
  let lexbuf = Lexing.from_channel (open_in filepath) in
  (lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = getLast (String.split_on_char '/' filepath) } );
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

let stylesheetToJson = Printer.stylesheetToJson;;