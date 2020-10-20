open Yojson.Basic.Util

type t = {
  users : string list;
  restaurants : string list;
  voting_progress : bool;
  user_voted : bool;
  group_voted : (string * bool) list
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
