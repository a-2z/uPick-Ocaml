open Lib.Dbquery

(* make passwords good? *)
let user_list = [
  ("reetuparikh", "Reetu123", "Reetu");
  ("andrewzeng", "Andrew123", "Andrew");
  ("andrewosorio", "Andrewo123", "Andrew");
  ("zachtegtmeier", "Zach123", "Zachary");
  ("johndoe", "johnny", "John");
  ("janedoe", "jane123", "Jane");
  ("ben", "benny", "Ben");
]

let friends_list = [
  (2, 1); (3, 4); (2, 3); (1, 4); (1, 5); (6, 1); (1, 7); (3, 7); 
  (6, 3); (4, 5)
]

let group_info_list = [
  ("bday party", 1); ("anniversary dinner", 3); ("lunch", 4);
]

let group_invite_list = [
  (1, 2); (1, 5); (1, 6); (1, 7); (3, 5); (3, 3)
]

let restriction_list = [
  (1, 2); (1, 3); (4, 1); (5, 3); (7, 1); (7, 2); (7, 3);
]

let restriction_index_list = [
  (1, "Vegetarian Friendly"); (2, "Vegan Options"); (1, "Gluten Free Options");
]

let preference_index_list = [
  (1, "Breakfast"); (1, "Brunch"); (1, "Lunch"); (1, "Dinner"); 
  (1, "Outdoor Seating"); (1, "Nightlife"); (1, "Buffet"); (1, "Delivery"); 
  (1, "Kid Friendly"); 
  ]

let cuisine_index_list = [
  (1, 95, "Thai"); (1, 83, "Seafood"); (1, 82, "Pizza"); (1, 1064, "Pasta"); 
  (1, 73, "Mexican"); (1, 55, "Italian"); (1, 40, "Fast Food"); 
  (1, 25, "Chinese"); (1, 30, "Cafe"); (1, 148, "Indian"); (1, 177, "Sushi" );
  (1, 70, "Mediterranean");
]

let sample_survey = [
  (4, 3, 40.78938, -74.057085, "", 100, 2500, "");
  (5, 3, 40.709792, -73.904728, "", 100, 5000, "" );
  (3, 3, 40.825761, -73.941741, "", 100, 7500, "");
  (* (1, 1, 40.78938, -74.057085, "", 100, 2500, "");
  (2, 1, 40.709792, -73.904728, "", 100, 5000, "" );
  (5, 1, 40.825761, -73.941741, "", 100, 7500, "");
  (6, 1, 40.78938, -74.057085, "", 100, 2500, "");
  (7, 1, 40.709792, -73.904728, "", 100, 5000, "" ); *)

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
  | (gid, mid) :: t -> begin 
  ignore(add_group_invites gid mid (snd (List.nth group_info_list (gid - 1)))); 
  ignore(join_group gid mid); add_groups' t end

let rec add_restrictions' = function 
  | [] -> ();
  | (uid, rid) :: t -> ignore(add_restrictions uid rid); add_restrictions' t

let rec add_rest_idx' = function 
  | [] -> ();
  | (admin, rest_name) :: t -> 
  ignore(add_restrictions_index admin rest_name); add_rest_idx' t

let rec add_pref_idx' = function 
  | [] -> ();
  | (admin, preference) :: t -> 
  ignore(add_preferences_index admin preference); add_pref_idx' t

let rec add_cuisine_idx' = function 
  | [] -> ();
  | (admin, id, cuisine) :: t -> 
  ignore(add_cuisine admin id cuisine); add_cuisine_idx' t


let rec add_survey' = function 
  | [] -> ();
  | (user_id, group_id, loc_x, loc_y, cuisine, price, range, preferences) :: 
  t -> 
    ignore
    (ans_survey user_id group_id loc_x loc_y cuisine price range preferences); 
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
  add_groups' group_invite_list;
  add_restrictions' restriction_list;
  add_rest_idx' restriction_index_list;
  add_pref_idx' preference_index_list;
  add_cuisine_idx' cuisine_index_list;
  add_survey' sample_survey;
  (* add_votes' sample_votes; *)


