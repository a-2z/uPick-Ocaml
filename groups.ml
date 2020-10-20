open Yojson.Basic.Util


type t = {
  users : string list;
  restaurants : string list;
  voting_progress : bool;
  user_voted : bool;
  group_voted : int list
}

let get_users t = 
  t.users

let get_restaurants t =
  t.restaurants

let voting_in_progress t =
  t.voting_progress

let has_user_voted t =
  t.user_voted

let has_group_voted t = 
  t.group_voted

let groups_info json = 
  {
    users = json |> member "users" |> to_list |> List.map to_string;
    restaurants = json |> member "restaurants" |> to_list |> List.map to_string;
    voting_progress = json |> member "voting_progress" |> to_bool;
    user_voted = json |> member "user_voted" |> to_bool;
    group_voted = json |> member "group_voted" |> to_list |> List.map to_int;
  }