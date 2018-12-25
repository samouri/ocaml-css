open Types;;

let print = Printer.prettyPrint;;
let astPrint = Printer.astPrint;;

let getLast lst = List.nth_opt (List.rev lst) 0;;

let getRulesetItemPos (t:Types.declaration or_comment) = match t with 
  | Comment {pos} -> pos
  | Else {pos} -> pos;;

let getStyleSheetItemPos (t:Types.rule)= match t with
  | Comment {pos} -> pos
  | AtRule {pos} -> pos
  | StyleRule {pos} -> pos;;

let isBefore (pos1:Lexing.position) (pos2:Lexing.position) = 
  pos1.pos_lnum < pos2.pos_lnum || 
  (pos1.pos_lnum = pos2.pos_lnum && pos1.pos_cnum < pos2.pos_cnum);;

let isAfter p1 p2 = not (isBefore p1 p2);;

let insertComment (stylesheet:Types.stylesheet) (comment:Types.comment) =
  (* does comment start before the node starts? *)
  let (cStartP, cEndP) = comment.pos in
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
        | (*TODO: atrules should handle comments *) AtRule(_) | Comment _ -> failwith "should never see nested comments in css"
        | StyleRule {prelude; declarations; pos } -> 
          let rulesIsBeforeComments (rule:Types.declaration or_comment) = 
            let (startPos, _) = match rule with 
              | Else {pos}  -> pos | Comment {pos} -> pos
            in
            isBefore startPos cStartP
          in
          let (prevRules, postRules) = List.partition rulesIsBeforeComments declarations in
          { prelude; declarations= (prevRules @ (Comment comment :: postRules)); pos;};
      in
      List.rev (List.tl (List.rev prevNodes)) 
      @ ((StyleRule newNode) :: postNodes)
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
      (print_endline (Printf.sprintf "Error at location (%d:%d) with token: %s!" line cnum tok));
      raise exn
    )