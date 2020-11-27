(*TEST SUITE *)
open Main
open OUnit2
(* open Db
   open Dbquery  *)

let test_equal name exptd expr = 
  name >:: (fun _ -> assert_equal exptd expr) 

let number_tests = [
  test_equal "two is two" 2 2;
  test_equal "port is 3000" port 3000;
]

let tests = "test suite for uPick" >::: List.flatten [

  ]

let _ = run_test_tt_main tests
