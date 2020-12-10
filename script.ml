open Lib.Dbquery

let user_list = [
  ("reetuparikh", "reetu123", "Reetu");
  ("andrewzeng", "andrew1", "Andrew");
  ("andrewosorio", "andrew2", "Andrew");
  ("zachtegtmeier", "zach123", "Zachary");
  ("johndoe", "johnny", "John");
  ("janedoe", "jane123", "Jane");
  ("ben", "benny", "Ben");
]

let friends_list = [
  (1, 2); (3, 4); (2, 3); (1, 4); (1, 5); (1, 6); (1, 7); (3, 7); (3, 6); (4, 5)
]

let group_info_list = [
  ("bday party", 1); ("anniversary dinner", 3); ("lunch", 4)
]

let group_list = [
  (1, 2); (1, 5); (1, 6); (1, 7); (3, 5); (3, 3)
]

let restriction_list = [
  (1, 2); (1, 3); (4, 1); (5, 3); (7, 1); (7, 2); (7, 3); (7, 4)
]

let restriction_index_list = [
  "eggs"; "dairy"; "gluten"; "vegan"; "peanuts"
]

let sample_survey = [
  (4, 3, 42.0, -76.3, "22", 30, 5000);
  (5, 3, 42.0, -76.3, "25", 30, 5000);
  (3, 3, 42.0, -76.3, "25", 30, 5000);
]

let sample_votes = [
  (3, 4, [20; 30; 10; 50; 40]);
  (3, 5, [20; 30; 40; 50; 10]);
  (3, 3, [50; 20; 10; 30; 40]);
]

let rec add_users' = function 
  | [] -> ();
  | (un, pw, n) :: t -> 
    let hash = pw |> Bcrypt.hash |> Bcrypt.string_of_hash in 
    ignore(add_user un hash n); add_users' t

let rec add_friends' = function 
  | [] -> ();
  | (f1, f2) :: t -> ignore(add_friends f1 f2); add_friends' t

let rec add_group_info' = function 
  | [] -> ();
  | (gn, hid) :: t -> ignore(add_group_info gn hid); add_group_info' t

let rec add_groups' = function 
  | [] -> ();
  | (gid, mid) :: t -> ignore(add_groups gid mid); add_groups' t

let rec add_restrictions' = function 
  | [] -> ();
  | (uid, rid) :: t -> ignore(add_restrictions uid rid); add_restrictions' t

let rec add_rest_idx' = function 
  | [] -> ();
  | rest_name :: t -> ignore(add_restrictions_index rest_name); add_rest_idx' t

let rec add_survey' = function 
  | [] -> ();
  | (user_id, group_id, loc_x, loc_y, cuisine, price, range) :: t -> 
    ignore(ans_survey user_id group_id loc_x loc_y cuisine price range); 
    add_survey' t

let rec add_votes' = function
  | [] -> ()
  | (g_id, user_id, lst) :: t -> ignore (add_votes g_id user_id lst);
  add_votes' t

(**Insert Information*)
let _ = 
  create_tables ();
  add_users' user_list;
  add_friends' friends_list;
  add_group_info' group_info_list;
  add_groups' group_list;
  add_restrictions' restriction_list;
  add_rest_idx' restriction_index_list;
  add_survey' sample_survey;
  add_votes' sample_votes;




