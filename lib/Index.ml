let print = Printer.prettyPrint;;

let parse ch =
  let lexbuf = Lexing.from_channel ch in
  try
    Parser.stylesheet Lexer.css lexbuf
  with exn -> (
    let curr = lexbuf.Lexing.lex_curr_p in
    let line = curr.Lexing.pos_lnum in
    let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol in
    let tok = Lexing.lexeme lexbuf in
    (print_endline (Printf.sprintf "Error at location (%d:%d) with character: %s" line cnum tok));
    raise exn
  )

let stylesheetToJson = Printer.stylesheetToJson;;