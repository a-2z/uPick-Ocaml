open Groups
open Json_helpers
open Restaurant
open User
open Yojson.Basic
open Yojson.Basic.Util


type t = {
  mutable groups : Groups.t ref list;
  mutable restaurants : Restaurant.t ref list;
  mutable users : User.t ref list;
}

let file_name = ref "" 

(**[load_users group_list] is a list of [Groups.t ref].
   Requires: group_list is a valid Yojson.Basic.t list*)
let rec load_groups = function
  | [] -> []
  | h :: t -> ref (Groups.from_json h) :: load_groups t

let rec load_restaurants = function
  | [] -> []
  | h :: t -> ref (Restaurant.from_json h) :: load_restaurants t

let rec load_users = function
  | [] -> []
  | h :: t -> ref (User.from_json h) :: load_users t

let load file = 
  file_name := file;
  let state = from_file file in 
  {
    groups = state |> member "groups" |> to_list |> load_groups;
    restaurants = state |> member "restaurants" |> to_list |> load_restaurants;
    users = state |> member "users" |> to_list |> load_users;
  }

let user_pred x y = (User.get_username !x = User.get_username !y)

let rest_pred x y = (Restaurant.get_name !x = Restaurant.get_name !y) && 
                    (Restaurant.get_x !x = Restaurant.get_x !y) && 
                    (Restaurant.get_y !x = Restaurant.get_y !y)

let group_pred x y = ((Groups.get_name !x = Groups.get_name !y) && 
                      (Groups.get_host !x = Groups.get_host !y)) 

let compare_nosort pred x y = 
  if pred x y then 0 else ~-1


let get_restaurant t name loc_x loc_y =  
  let rec aux = function
    | [] -> None 
    | hd :: tl -> begin 
        if (Restaurant.get_name hd == name) && (Restaurant.get_x hd == loc_x) && 
           (Restaurant.get_y hd == loc_y) then Some (Restaurant.get_id hd)
        else aux tl end in 
  aux (List.map (fun x -> !x) t.restaurants)

let get_group t name host = 
  let rec aux = function
    | [] -> None
    | hd :: tl ->  begin
        if (Groups.get_name hd == name) && (Groups.get_host hd == host)  
        then Some (Groups.get_id hd)
        else aux tl end in
  aux (List.map (fun x -> !x) t.groups)

let get_user t username = 
  let rec aux = function
    | [] -> None 
    | hd :: tl -> begin 
        if User.get_username hd == username then Some (User.get_id hd)
        else aux tl end in 
  aux (List.map (fun x -> !x) t.users)

(**[groups_to_list group_lst] converts a Groups.t list to a list of strings
   representing the groups in a JSON format.*)
let rec groups_to_list = function 
  | [] -> []
  | h :: t -> Groups.to_json !h :: groups_to_list t

let rec rests_to_list = function 
  | [] -> []
  | h :: t -> Restaurant.to_json !h :: rests_to_list t

let rec users_to_list = function 
  | [] -> []
  | h :: t -> User.to_json !h :: users_to_list t

let save state = 
  {|{"groups": |} ^ json_dict_lst (groups_to_list state.groups) ^
  {|, "restaurants": |} ^ json_dict_lst (rests_to_list state.restaurants) ^ 
  {|, "users": |} ^ json_dict_lst (users_to_list state.users) ^
  "}" |> from_string |> to_file !file_name

let get_head_id (id_getter : 'a -> int) list =
  match list with
  | [] -> -1
  | h :: t -> id_getter h

let add_user t username password name = 
  let deref_user = List.map (fun x -> !x) t.users in
  let top_id = 1 + (get_head_id User.get_id deref_user) in 
  t.users <- List.sort_uniq (compare_nosort user_pred) 
      (ref (User.create top_id username password name) :: t.users)

let add_restaurant 
    t name loc_x loc_y cuisine_type rating allergens price wait_time = 
  let deref_rest = List.map (fun x -> !x) t.restaurants in
  let top_id = 1 + (get_head_id Restaurant.get_id deref_rest) in 
  t.restaurants <- List.sort_uniq (compare_nosort rest_pred)
      (ref (Restaurant.create top_id name loc_x loc_y cuisine_type 
              rating allergens price wait_time) :: t.restaurants)

let add_group t group_name host = 
  let deref_group = List.map (fun x -> !x) t.groups in
  let top_id = 1 + (get_head_id Groups.get_id deref_group) in 
  t.groups <- List.sort_uniq (compare_nosort group_pred)
      (ref(Groups.create top_id group_name host) :: t.groups)

let find_user t id_number = 
  let rec helper id = function 
    | [] -> None
    | h :: t -> if User.get_id h = id then Some h else helper id t in
  helper (Option.get id_number) (List.map (fun x -> !x) t.users)

let find_group t id_number = 
  let rec helper id = function 
    | [] -> None
    | h :: t -> if Groups.get_id h = id then Some h else helper id t in 
  helper id_number (List.map (fun x -> !x) t.groups)

let make_friends t sender recipient = 
  try 
    let sender_t = Option.get (find_user t sender) in 
    let recipient_t = Option.get (find_user t recipient) in
    User.add_friend sender_t recipient_t;
    User.add_friend recipient_t sender_t;
  with Invalid_argument _ -> ()

let join_group t group_id user_id =
  try 
    let usr = Option.get (find_user t user_id) in
    let grp = Option.get (find_group t group_id) in
    User.update_groups usr group_id;
    Groups.add_user grp (Option.get user_id);
  with Invalid_argument _ -> ()












































































