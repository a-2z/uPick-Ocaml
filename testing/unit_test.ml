(*TEST SUITE *)
open OUnit2
open Lib.Server

let test_equal name exptd expr = 
  name >:: (fun _ -> assert_equal exptd expr) 

(* URL pointing to local host *)
LOCAL_URL = "http://localhost:3000"

let number_tests = [
  test_equal "two is two" 2 2;
  test_equal "port is 3000" port 3000;
]

let  = [

]

let 


let tests = "test suite for uPick" >::: List.flatten [
    number_tests;
  ]

let _ = run_test_tt_main tests
