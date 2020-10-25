(* open App_state
   open Groups
   open OUnit2
   open Restaurant
   open User 
   open Yojson.Basic

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
   (* {|{"id": "|} ^ string_of_int t.id ^ {|", "name": "|} ^ t.name ^ 
   {|", "location_x": |} ^ string_of_float t.location_x ^ {|, "location_y": |} ^ 
   string_of_float t.location_y ^ {|, "food_type": "|} ^ t.food_type ^ 
   {|", "rating": |} ^ string_of_float t.rating ^ {|, "allergies": |} ^ 
   (json_string_lst t.allergies) ^ {|, "avg_price": |} ^ 
   string_of_int t.avg_price ^ {|, "avg_wait": |} ^ string_of_int t.avg_wait ^
   "}" *)

   let rest_json1 = from_string {|{
   "id": 1, 
   "name": "taco bell", 
   "location_x": 30.12, 
   "location_y": 44.15,
   "food_type": "fast food",
   "rating": 4.67,
   "allergies": ["eggs"; "dairy"],
   "avg_price": 15,
   "avg_wait": 30
   }|}

   let test_restaurant = [
   "from_json creates valid restaurant" >:: 
   (fun _ -> assert_equal Restaurant.from_json 
      {id = 1; name ="taco bell";
       location_x = 30.12; location_y = 44.15; food_type = "fast food";
       rating = 4.67; allergies = ["eggs"; "dairy"]; avg_price = 15; avg_wait = 30})]

   let grp_json1 = from_string {|{
   	"id": 1,
   	"name" : "Jane's birthday",
   	"host" : "Jimmy123",
   	"users": [],
   	"restaurants": [],
   	"survey_complete": [],
   	"voting_complete": [],
   	"candidates": [], 
   "final_choice": null}
   |} 

   let test_group = [
   "from_json instantiates " >:: 
   (fun _ -> assert_equal Groups.from_json {id = 1; 
                                           name = "Jane's birthday"; 
                                           host = "Jimmy123"; users= []; 
                                           restaurants = []; 
                                           survey_complete = []; 
                                           voting_complete = []; 
                                           candidates = []; 
                                           final_choice = None} ;)
   ]

   let usr_json1 = from string {|{
   "id" : 1,
   "username" : "zachtegtmeirer",
   "password" : "zach123",
   "name" : "zach",
   "friends" : [1,3,4],
   "restrictions : [], 
   "visited" : [],
   "groups" : [] }|}

   let test_user = [
   "from_json creates valid user" 
   >:: (fun _ -> assert_equal User.from_json {id = 1;
                                             username = "zachtegtmeirer";
                                             password = "zach123";
                                             name = "zach"; 
                                             friends = "zach";
                                             restrictions = [1,3,4];
                                             visited = [];
                                             groups = []})
   ]

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
   []

   (* let add_user_test
    (t : User.t)
    (username : string)
    (password : string)
    (name : string)
    (expected_output : unit) : test = 
   name >:: (fun _ ->  *)

   let add_user_test = 
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
*)
