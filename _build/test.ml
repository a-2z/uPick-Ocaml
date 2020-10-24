open App_state
open Groups
open OUnit2
open Restaurant
open User 

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let add_restaurant_tests = 
  [ 
    (* "test restaurant1" 
       >:: (fun -> assert_equal "taco bell" (add_restaurant sample_t)); *)
  ]

let add_user_test = 
  [
    (* "test user added" 
       >:: (fun -> assert_equal "reetuparikh" (add_user sample_t)); *)
  ]

let make_friends_test = 
  [
    (* "Test if friends" 
       >:: (fun -> assert_equal (True) (make_friends sample_t)); *)
  ]

let make_group_test =
  [
    (* "test if new group created"
       >:: (fun -> assert_equal (True) (make_group sample_t)); *)
  ]

let join_group_test = 
  [
    (* "test if joined group"
       >:: (fun -> assert_equal (True) (join_group sample_t)); *)
  ]

let suite = 
  "test suite for uPick" >::: List.flatten [
    add_restaurant_tests;
    add_user_test;
    make_friends_test;
    make_group_test;
    join_group_test;
  ]

let _ = run_test_tt_main suite