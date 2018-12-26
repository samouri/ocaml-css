open Lib.Index;;
open OUnit2;;

let dir_contents dir =
  Sys.readdir dir |> Array.to_list
;;

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n;
  close_in ic;
  Bytes.to_string s
  ;;

let rec printlst = function | [] -> (); | hd :: tl -> (print_endline hd); printlst tl;;

let test_path = "./test/cases"
let test_dirs = dir_contents test_path;;

let get_test_for_dir dir =
  let filename = (test_path ^ "/" ^ dir ^ "/input.css") in
  let inputStr = load_file filename in
  let expectedStr = load_file (test_path ^ "/" ^ dir ^ "/output.css") in
  let expectedAst = Yojson.Safe.prettify (load_file (test_path ^ "/" ^ dir ^ "/ast.json")) in
  dir >:: (fun _ -> (
    let actualAst = astPrint (parse ~fp:filename inputStr) in
    let actualStr = print (parse ~fp:filename inputStr) in
    
    assert_equal ~msg:"pretty-print" ~printer:(fun x -> x) expectedStr actualStr;
    assert_equal ~msg:"ast-print" ~printer:(fun x -> x) expectedAst actualAst;
  ))
;;

let suite = "suite">::: (List.map (fun dir -> get_test_for_dir dir) [
  (* "at-namespace";
  "charset";
  "charset-linebreak";
  "colon-space"; 
  "comma-attribute"; 
  "comma-selector-function"; *)
  "comment"; 
  "comment-in";
  "comment-url"; (*
  "custom-media";
  "custom-media-linebreak";
  "document";
  "document-linebreak";
  *)
  "empty";
  (*
  "escapes";
  "font-face";
  "font-face-linebreak";
  "hose-linebreak";
  "host";
  "import";
  "import-linebreak";
  "import-messed";
  "keyframes";
  "keyframes-advanced";
  "keyframes-complex";
  "keyframes-linebreak";
  "keyframes-messed";
  "keyframes-vendor";
  "media";
  "media-linebreak";
  "media-messed";
  "messed-up";
  "namespace";
  "namespace-linebreak"; 
  "no-semi";
  "page-linebreak";
  "paged-media"; *)
  "props";
  "quote-escape";
  "quoted";
  "rule";
  "rules";
  "selectors";
  (*"supports";
  "supports-linebreak";
  "wtf"; *)
  ])
;;

run_test_tt_main suite
