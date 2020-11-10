open Yojson.Basic.Util
open Json_helpers

exception NoVotes

exception NoGroup

type t = {
  id : int;
  name : string;
  host : string;
  mutable users : int list;
  mutable restaurants : int list;
  mutable survey_complete : int list;
  mutable voting_complete : int list;
  mutable candidates : int list option;
  mutable final_choice : int option;
}

(* FIX THIS, working on it, do not see a json schema to show exactly what
   each of these values we're pulling from the json will be called: ex. not sure if
   "survey" is the currect variable name to pull for survey_complete, however
   very easy to fix. Record definition is based off assumption that 
   definition of type t above is correct.*)

let from_json json = 
  {
    id = json |> member "group_id" |> to_int;
    name = json |> member "group_name" |> to_string;
    host = json |> member "host" |> to_string;
    users = json |> member "users" |> to_list |> List.map to_int;
    restaurants = json |> member "restaurants" |> to_list |> List.map to_int;
    survey_complete = json |> member "survey" |> to_list |> List.map to_int;
    voting_complete = json |> member "voting" |> to_list |> List.map to_int;
    candidates = 
      Some (json |> member "candidates" |> to_list |> List.map to_int);
    final_choice = json |> member "final_choice" |> to_int_option
  }

let to_json t = 
  let unoption optional = 
    match optional with 
    | None -> "null"
    | Some v -> string_of_int v in
  {|{"group_id": "|} ^ string_of_int t.id ^ 
  {|", "group_name": "|} ^ t.name ^ 
  {|", "host": "|} ^ t.host ^ 
  {|", "users": |} ^ json_int_lst t.users ^ 
  {|, "restaurants": |} ^ json_int_lst t.restaurants ^ 
  {|, "survey": |} ^ json_int_lst t.survey_complete ^ 
  {|, "voting": |} ^ json_int_lst t.voting_complete ^ 
  {|, "candidates": |} ^ json_int_lst_opt t.candidates ^ 
  {|, "final_choice": |} ^ unoption t.final_choice ^ "}"

let get_id t = t.id

let get_name t = t.name

let get_host t = t.host

let get_users t = t.users

let get_candidates t = t.candidates

let get_winner t = t.final_choice

let surveys_done t = 
  List.length t.survey_complete = List.length t.users

let voting_done t = 
  List.length t.voting_complete = List.length t.users

let add_user t user = 
  t.users <- user :: t.users

(*We want a function to create a restaurant in group.ml?*)
let create grp_id name host = 
  {
    id = grp_id;
    name = name;
    host = host;
    users = [];
    restaurants = [];
    survey_complete = [];
    voting_complete = [];
    candidates = None;
    final_choice = None;
  }