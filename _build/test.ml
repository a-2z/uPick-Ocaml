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


let instance = load "state.json" 

(* let add_restaurant_test  
    (t : App_state.t) 
    (name : string) 
    (loc_x : float)
    (loc_y : float)
    (cuisine_type : string)
    (rating : float)
    (allergens : string list)
    (price : int)
    (wait_time : int)
    (expected_output : unit) : test = 
   name >:: (fun _ ->
      assert_equal expected_output (add_restaurant t name loc_x loc_y cuisine_type 
                                      rating allergens price wait_time))
*)
let add_restaurant_tests = 
  [ 
    (* add_restaurant instance 
       "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
       add_restaurant instance 
       "mcDonalds" 33.12 92.15 "fast food" 4.67 ["dairy"] 5 20;
       add_restaurant instance  
       "pizza hut" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
       add_restaurant instance 
       "Outback Steakhouse" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
       save instance; *)
  ]

(* let add_user_test
    (t : User.t)
    (username : string)
    (password : string)
    (name : string)
    (expected_output : unit) : test = 
   name >:: (fun _ ->  *)

let add_user_test = 
  []

let make_friends_test = 
  []

let make_group_test =
  []

let join_group_test = 
  []

let suite = 
  "test suite for uPick" >::: List.flatten [
    add_restaurant_tests;
    add_user_test;
    make_friends_test;
    make_group_test;
    join_group_test;
  ]

let _ = run_test_tt_main suite

