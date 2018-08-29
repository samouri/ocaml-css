(* open Lib.Types *)
open Lib.Index
open Cmdliner

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
  (* let css_str = load_file file in *)
  let parsed = parse file in
  let output_flag = output_of_str output_str in (* learn how to make the proper CMDLiner converter for this *)
  let output = if output_flag = Pretty then print parsed else astPrint parsed
  in
  print_endline output
  ;;

let css_t = Term.(const run $ file $ output)
let () = Term.exit @@ Term.eval (css_t, Term.info "css-parse")


