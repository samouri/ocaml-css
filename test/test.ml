(* open Lib.Index;; *)

let dir_contents dir =
  Sys.readdir dir |> Array.to_list
;;

let rec printlst = function | [] -> (); | hd :: tl -> (print_endline hd); printlst tl;;

(* we want to gather a list of the folders that are passing and failing. ideally just printing out thetones that succeed since
 * almost none will, as well as optionally which are failing
 *)
let run_test dir =
  (print_endline ("running test for case: ./test/cases/" ^ dir ^ "/*"));
  (dir_contents ( "./test/cases/" ^ dir))
    |> List.map (fun x -> "    " ^ x)
    |> printlst
;;

let main () =
  (print_endline "RUN ALL TESTS 0/0");
  let rec runtests = function
    | [] -> print_endline "DONE!"
    | hd::tl -> run_test hd; runtests tl
  in
  runtests (dir_contents "./test/cases")
;;


main ();;