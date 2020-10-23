open Yojson.Basic.Util


type t = {
  id : int;
  name : string;
  host : string;
  users : int list;
  survey_complete : int list;
  voting_complete : int list;
  candidates : int list option;
  final_choice : int option;
}

(* FIX THIS *)
let from_json json = 
  {
    id = json |> member "group_id" |> to_string;
    name = json |> member "group_name" |> to_string;
    users = json |> member "users" |> to_list |> List.map to_string;
    restaurants = json |> member "restaurants" |> to_list |> List.map to_string;
    voting_progress = json |> member "voting_progress" |> to_bool;
    user_voted = json |> member "user_voted" |> to_bool;
    group_voted = json |> member "group_voted" |> to_list |> List.map to_int;
  }

let to_json t = 
  failwith "unimplemented"

let get_name t = t.name

let get_users t = t.users

let get_candidates t = t.candidates

let get_winner t = t.final_choice

let has_user_voted t user =
  List.mem user t.voting_complete

let has_group_voted t = 
  List.length t.voting_complete = List.length t.voting_complete

let create name location cuisine_type rating allergens price wait_time = 
  failwith "unimplemented"


