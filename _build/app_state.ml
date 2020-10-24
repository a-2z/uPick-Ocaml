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

type group_id = string
type res_id = string
type user_id = string

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
  {|, "restaurants : |} ^ json_dict_lst (rests_to_list state.restaurants) ^ 
  {|, {"users": |} ^ json_dict_lst (users_to_list state.users) ^
  "}"

let add_user new_user t = 
  t.username <- new_user.id :: t.username

let add_restaurant t new_restaurant = 
  t.restaurant <- new_restaurant :: t.restaurant

let add_group t group_name host = 
  t.groups <- new_group :: t.groups

let make_friends new_friends t = 
  t.friends <- new_friends :: t.friends

let join_group t group_id  = 
  t.new_group <- new_group ::  t.groups 



