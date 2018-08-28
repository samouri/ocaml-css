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
  let inputFile = open_in (test_path ^ "/" ^ dir ^ "/input.css") in
  let expectedStr = load_file (test_path ^ "/" ^ dir ^ "/output.css") in
  let actual = print (parse inputFile) in
  (print_endline (string_of_int ((String.length expectedStr) - (String.length actual))));
  dir >:: (fun _ -> assert_equal ~msg:dir ~printer:(fun x -> x) expectedStr actual)
;;

let suite = "suite">::: (List.map (fun dir -> get_test_for_dir dir) [
  "rule"; 
  "rules";
  ])
;;

run_test_tt_main suite
