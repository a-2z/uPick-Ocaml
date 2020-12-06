(*TEST SUITE *)
open OUnit2
open Lib.Server
open Sqlite3
open Lwt.Infix
open Yojson.Basic
open Yojson.Basic.Util
open Opium.Std
open Dbquery
open Script

(* delete db and open new db *)
let db = db_remove "upick.db"
let db = db_open "upick.db"

(* URL pointing to local host *)
let local_url = "http://localhost:3000"

(* sample request bodies *)
let sample_user = [("reetuparikh", "reetu123", "Reetu")]
let sample_friend_list = [(1, 2); (3,4)]
let sample_restriction_list = [!1,2); (1,3)]
let sample_ 

(* unwrap bodies *)

let test_equal name exptd expr = 
  name >:: (fun _ -> assert_equal exptd expr) 

let port_test = [
  test_equal "two is two" 2 2;
  test_equal "port is 3000" port 3000;
]


let add_user_test = [
  test_equal "no users added" None (add_user empty)
    test_equal "user list added" user_list (add_user user_list)
]

let add_restrictions_test = [
  test_equal "no users added" None (add_restrictions empty)
]

let add_groups_test = [
  test_equal "no users added" None (add_groups empty)
]

let login_test = [
]

let get_user_test = [
  test_equals "empty users" None (get_user empty)
]

let get_group_test = [
  test_equal "empty groups" None (get_group empty)
]

let get_restrictions_test = [
  test_equal "empty restricitons" None (get_restrictions empty)
]

let filter_test = [
]

let get_restaurant_test = [
]

let tests = "test suite for uPick" >::: List.flatten [
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

let _ = run_test_tt_main tests
