(*Test Plan: We used a mix of manual and automated testing to ensure the 
  robustness of our backend. 

  *****************Automated (Dbquery, Search, Passwords)***********************

  We used OUnit2 specifically to test interaction with the Database in terms of 
  insertions, updates, and deletions. By performing operations using the in
  terfacing get and set functions in the Dbquery module, we thoroughly 
  checked that data were formatted correctly if acceptable and rejected if 
  formatted incorrectly. We directly assessed Dbquery and Search, as the 
  purpose of Db was to handle database table creation, which was not changed 
  throughout the development process; testing of it was handled manually. 
  The Server module was tested manually as well, because testing routes us
  ing OUnit2 presented challenges with request creation and running the test 
  suite and server concurrently.

  We used dummy inputs generated by a script in order to validate our data 
  insertion. However, in testing search, the program made live requests to the
  Zomato API.

  Types of testing: We used a mix of black box and glass box testing for the
  automatic portion. We created tests by using "make docs" and using the 
  generated documentation to ensure that specifications were met. We then 
  reviewed the actual implementation to ensure coverage of all branches. 
  We did not utilize randomized testing. 

  ************************Manual (Db, Server)*********************************  
  We used Postman, an application for sending and receiving HTTP requests for 
  testing the server module. We confirmed that the insertion of new users, 
  voting, and performing surveys was reliable and that unique and primary key 
  constraints in the database led to the rejection of invalid requests.
  We also employed DB Browser for sqlite--a graphical application for viewing
  and interacting with sqlite ".db" files, which allowed us to visualize the
  database structure and test the raw SQL that was in our source code. The DB
  module was tested with a thorough, manual review of each table.

  Types of testing: We used a combination of both black-box and glass-box 
  testing for the manual portion. For black-box testing, we used the HTTP 
  interface to make sure that we got responses for all well-formed bodies and 
  routes (as documented). With glass-box testing, we were able to go 
  line-by-line and ensure that for every table definition that we executed,
  the proper constraints, types, and structures were applied. Since the 
  interface of Db does not expose any of its structure, we had to rely on 
  glass-box testing exclusively for it.

  Testing Approach: This manual testing approach demonstrates the correctness
  of the system since we were able to ensure that our HTTP requests are 
  successful and that the tables we created in sqlite3. Through the 
  implementation of both automated and manual testing alonside Postman and DB 
  Browser we can enssure the correctness of our system by implementing a wide 
  variety of testing that covers all aspects of our functions and routes.
  After ensuring that our functions and routes are successful individually, our
  testing plan ensures that they correctly come together when testing our 
  modules.
*)
open Lib
open Lib.Dbquery 
open OUnit2

(**[open_db] is evaluated when the module is first processed*)
let open_db = Lib.Dbquery.create_tables ()

(*Testing user functions*)
let make_user ?friends:(f = []) ?restrictions:(r = []) ?groups: (g = []) 
    id un pw n =
  {
    id = id;
    username = un; 
    password = pw; 
    name = n; 
    friends = f; 
    restrictions = r;
    groups = g 
  }

let make_group ?members:(m = []) ?voting:(v = false) ?top_5:(t5 = None) 
    ?top_pick:(t = None) id name host =
  {
    id = id;
    name = name;
    host_id = host;
    members = m; 
    voting_allowed = v;
    top_5 = t5;
    top_pick = t;
  }

let make_restriction id name =
  { 
    id = id;
    name = name;
  }

(**[fail_thrower exp] raises [Failure "assertion"] if the evaluation
   of [exp] leads to an assertion_error*)
let fail_thrower exp = 
  try 
    exp; 
    true 
  with Assert_failure _ -> raise (Failure "assertion")

let ins_user (u, n, p) = 
  add_user u n p

let ins_group (g_id, u_id) = 
  join_group g_id u_id 

let ins_group_info (g_name, h_id) = 
  add_group_info g_name h_id

let try_test ins_func data = 
  try match ins_func data with 
    | Some _ -> true 
    | None -> false 
  with _ -> false

(**[comp_bool] x y checks whether [x] and [y] are the same using 
   Stdlib.compare*)
let comp_bool x y = compare x y = 0 

(**Inverses the output of comp_bool *)
let not_comp x y = not (comp_bool x y)

(*OUnit2 test creators*)

let test_equal ?compare:(cmp = comp_bool) name exptd expr = 
  name >:: (fun _ -> assert_equal ~cmp:cmp exptd expr)

let test_passes name ?succ:(s = true) ins_func data = 
  name >:: (fun _ -> assert_equal s (try_test ins_func data)) 

(**[try_get get_func id] is false if an attempt to retrieve data with [id] 
   using get_func raises and exception. The resulting data are ignored.*)
let try_get name get_func id = 
  let success = begin
    try 
      match get_func id with 
      | _ -> true
    with 
      _ -> false end in
  test_equal name true success

(**ensures that the user matching [n] in the database matches the user 
   generated with [make_user]*) 
let test_user name id (u, p, n) = 
  (*add a user to the database*)
  let user = make_user id u (Bcrypt.hash p |> Bcrypt.string_of_hash) n in 
  test_equal name {(get_user id) with password = p} {user with password = p}

let test_password name bool pw = test_equal name bool (Passwords.is_valid pw)

(**ensures that the group matching [n] in the database matches the user 
   generated with [make_group]*) 
let test_group_info ?is_eq:(eq = comp_bool) ?members:(m = []) 
    ?voting:(v = false) ?top_5:(t5 = None) ?top_pick:(t = None) 
    name group_id n h = 
  let group = make_group ~members:m ~voting:v ~top_5:t5 
      ~top_pick:t group_id n h in 
  test_equal ~compare:eq name (get_group group_id) group

let user1 = add_user "reetuparikh" "Reetu123" "Reetu"
let user2 = add_user "andrewzeng" "Andrew1" "Andrew"
let user3 = add_user "andrewosorio" "Andrew2" "Andrew"
let user4 = add_user "zachtegtmeier" "Zach123" "Zachary"
let user5 = add_user "johndoe" "Johnny1" "John"
let user6 = add_user "janedoe" "Jane123" "Jane"
let user7 = add_user "peterparker" "Jane123" "Peter"

let add_user_test = [
  (*validate existing insertions*)
  test_user "ensure that the 5th user is correct" 5
    ("johndoe", "Johnny", "John"); 
  test_user "ensure that the first user added is correct" 1
    ("reetuparikh", "Reetu123", "Reetu");
  test_user "ensure that the first user added is correct" 1
    ("reetuparikh", "Reetu123", "Reetu");
  test_user "ensure that all users have been added, passwords can be reused" 7
    ("peterparker", "Jane123", "Peter");
  test_passes "test when adding people with same name"
    ~succ:true ins_user ("andrew1235", "baa5j1", "Andrew");
  test_passes "usernames must be unique"
    ~succ:false ins_user ("andrewosorio", "Andrew3", "John");
  test_passes "username is incorrect"  ~succ:false ins_user 
    ("", "Jane123", "Jane");
  test_passes "malformed name" ~succ:false ins_user 
    ("peterparker", "Jane123", "");
]

let password_test = [
  (*ensure passwords meet criteria*)
  test_password "password cannot be empty" false "";
  test_password "contains no capital letter" false "abcd123";
  test_password "numeric password" false "123135424354";
  test_password "password contains no numbers" false "abcdefghi";
  test_password "missing lowercase password" false "ASD123";
  test_password "weak password less than seven characters" false "Abcder123";
  test_password "missing uppercase password" false "asdf234";
]

let test_friends ?are_friends:(ff = true) name f1 f2 = 
  let friends_1 = (get_user f1).friends in 
  let expr = if ff then List.mem f2 friends_1 
    else not (List.mem f2 friends_1) in
  name >:: (fun _ -> assert_bool "not friends" expr)

let friends1 = add_friends 1 2
let friends2 = add_friends 2 3
let friends3 = add_friends 3 4
let friends4 = add_friends 4 5
let friends5 = add_friends 5 6

let add_friends_test = [
  (*validate existing insertions*)
  test_friends "test can not add friend that already is a friend" 2 3;
  test_friends "test friend reciprocity" 3 2;
  test_friends "test can not add friend that already is a friend" 2 1;
  test_friends ~are_friends:false "users that are not friends" 5 3;
  test_friends ~are_friends:false "a user cannot friend themself" 3 3;  
  test_friends ~are_friends:false "a user cannot friend nonexisting user" 3 0;
  try_get "invalid friend ids are handled" get_user 13;
  (*attempt new insertions*)
]

let no_group_test = [
  test_passes "join group before creation" ~succ:false ins_group (1, 2);
]

let group_info1 = add_group_info "birthday party" 3
let group_info2 = add_group_info "anniversary dinner" 1
let group_info3 = add_group_info "lunch" 2 

let add_group_info_test = [
  (* validate existing insertions *)
  test_group_info "ensure that the details of group 1 are correct" 1 
    "birthday party" 3;
  test_group_info ~is_eq:not_comp "mismatched group id and details" 1 
    "lunch" 3;
  test_group_info ~is_eq: not_comp "non existing group" 1 
    "dinner" 1;
  test_group_info ~is_eq: not_comp "non existing host" 1 "birthday party" 0;
  (*attempt new insertions*)
  test_passes ~succ:true "same group name, different hosts allowed" 
    ins_group_info ("birthday party", 2);
  test_passes ~succ:false "same name, different case is not permitted" 
    ins_group_info ("Birthday Party", 2);
  test_passes ~succ:true "one host can create two groups with different names" 
    ins_group_info ("garden party", 2);
  test_passes ~succ:false "one host cannot create two groups with the same name"
    ins_group_info ("birthday party", 3);
  test_passes ~succ:false "incorrect hostid cannot create group" 
    ins_group_info ("birthday party", 0);
]

let add_group_test = []

let is_member_test = []

let is_friend_test = []

let get_user_test = []

let get_group_test = []

let get_restrictions_test = []

let filter_test = []

let voting_test = []

let tests = "test suite for uPick" >::: List.flatten [
    no_group_test;
    add_user_test;
    add_friends_test;
    add_group_info_test;
    (* add_restrictions_test; *)
    get_user_test;
    get_group_test;
    get_restrictions_test;
    filter_test;
  ]

let _ = run_test_tt_main tests