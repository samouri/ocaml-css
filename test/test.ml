open Css;;

let dir_contents dir =
  Sys.readdir dir |> Array.to_list
;;

let rec printlst = function | [] -> (); | hd :: tl -> (print_endline hd); printlst tl;;

let run_test dir = 
  (print_endline ("running test for case: ./test/cases/*" ^ dir));
  (printlst (dir_contents dir))
;;

let main () = 
  (print_endline "RUN ALL TESTS 0/0");
  let rec runtests = function
    | [] -> print_endline "DONE!"
    | hd::tl -> run_test hd; runtests tl
  in
  runtests (dir_contents "./tests/cases")
;;


main ();;