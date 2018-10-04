let print = Printer.prettyPrint;;
let astPrint = Printer.astPrint;;

let getLast lst = List.nth_opt (List.rev lst) 0;;

let getRulesetItemPos (t:Types.ruleset_item) = match t with 
  | Comment(_, pos)
  | Rule (_,_, pos) -> pos;;

let getStyleSheetItemPos (t:Types.stylesheet_item)= match t with
  | Comment(_, pos) 
  | Ruleset (_,_, pos) -> pos;;

let isBefore (pos1:Lexing.position) (pos2:Lexing.position) = 
  pos1.pos_lnum < pos2.pos_lnum || 
  (pos1.pos_lnum = pos2.pos_lnum && pos1.pos_cnum < pos2.pos_cnum);;

let isAfter p1 p2 = not (isBefore p1 p2);;

open Types;;
let insertComment (stylesheet:Types.stylesheet) (comment:Types.comment) =
  (* does comment start before the node starts? *)
  let (commentString, (cStartP, cEndP)) = comment in
  let nodeIsBeforeComment node = 
    let (startPos, _) = getStyleSheetItemPos node in
    isBefore startPos cStartP
  in
  let (prevNodes, postNodes) = List.partition nodeIsBeforeComment stylesheet in
  (* 
   * there are three potential cases:
   * 1: comment occurs after all of the code.
   * 2: comment occurs in between rulesets. then insert comments in between.
   * 3: comment occurs in between rules. must dive into the ruleset.
   *)
  (* List.iter (fun x -> print_endline (show_stylesheet_item x)) prevNodes;
     List.iter (fun x -> print_endline (show_stylesheet_item x)) postNodes; *)
  match getLast prevNodes with
  | None -> Comment comment :: postNodes
  | Some node -> 
    let (_, endPos) = getStyleSheetItemPos node in
    if isAfter cEndP endPos then
      prevNodes @ (Comment comment :: postNodes)
    else (
      (* mid ruleset case*)
      let newNode = match node with
        | Comment _ -> failwith "should never see nested comments in css"
        | Ruleset (s, rules, p) -> 
          let rulesIsBeforeComments rule = 
            let (startPos, _) = getRulesetItemPos rule in
            isBefore startPos cStartP
          in
          let (prevRules, postRules) = List.partition rulesIsBeforeComments rules in
          (s, prevRules @ (Comment comment :: postRules), p)
      in
      List.rev (List.tl (List.rev prevNodes)) 
      @ ((Ruleset newNode) :: postNodes)
    )
;;

let parse  ?(fp="") (source: string) =
  let lexbuf = Lexing.from_string source in
  let pos_fname = match (getLast (String.split_on_char '/' fp) ) with 
    | None -> "" 
    | Some s -> s in 
  (lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname } );
  try
    let stylesheet = Parser.stylesheet Lexer.css lexbuf in
    let comments = Lexer.comments () in
    let _ = Lexer.clearComments () in
    (* List.iter (fun (c,pos) -> print_endline (c ^ (show_position pos))) comments; *)
    (List.fold_left insertComment stylesheet comments)
  with exn -> (
      let _ = Lexer.clearComments () in
      let curr = lexbuf.Lexing.lex_curr_p in
      let line = curr.Lexing.pos_lnum in
      let cnum = curr.Lexing.pos_cnum - curr.Lexing.pos_bol + 1 in
      let tok = Lexing.lexeme lexbuf in
      (print_endline (Printf.sprintf "Error at location (%d:%d) with character: %s!" line cnum tok));
      raise exn
    )