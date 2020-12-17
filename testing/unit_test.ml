(*TEST SUITE *)
open OUnit2
open Lib.Server
(* open Sqlite3
open Lwt.Infix
open Yojson.Basic
open Yojson.Basic.Util *)
(* open Opium.Std *)
(* open Lib.Dbquery *)

(* URL pointing to local host *)
let local_url = "http://localhost:3000"

let empty = [] 
(* sample request bodies *)
let sample_user = [("reetuparikh", "reetu123", "Reetu")]
let sample_friend_list = [(1, 2); (3,4)]
let sample_restriction_list = [(1,2); (1,3)]

let test_equal name exptd expr = 
  name >:: (fun _ -> assert_equal exptd expr) 

let port_test = [
  test_equal "two is two" 2 2;
  test_equal "port is 3000" port 3000;
]

let add_user_test = [
  (* test_equal "user list added" user_list (add_user user_list) *)
]

let add_restrictions_test = [
]

let add_groups_test = [
]

let login_test = [
]

let get_user_test = [
]

let get_group_test = [
]

let get_restrictions_test = [
]

let filter_test = [
]

let get_restaurant_test = [
]

let tests = "test suite fo r uPick" >::: List.flatten [
    port_test;
    add_user_test;
    add_restrictions_test;
    add_groups_test;
    login_test;
    get_user_test;
    get_group_test;
    get_restrictions_test;
    filter_test;
    get_restaurant_test;
  ]

let _ = start(); run_test_tt_main tests
