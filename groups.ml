open Yojson.Basic.Util


type t = {
  id : int;
  name : string;
  host : string;
  users : int list;
  restaurants : int list;
  survey_complete : int list;
  voting_complete : int list;
  candidates : int list option;
  final_choice : int option;
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

(**Note: the return is not a valid JSON itself. Copied from user.ml*)
let json_int_lst (lst : int list) = 
  let rec aux in_lst acc = 
    match in_lst with 
    | [] -> acc ^ "]"
    | last :: [] -> acc ^ string_of_int last ^ "]"
    | h :: t -> aux t (acc ^ (string_of_int h) ^ ", ") in
  aux lst "["

(**Similar helper function but for a int list option, not sure if this can
   be combined in any way with function above to save code since functions are 
   two different types. *)
let json_int_lst_opt (lst : int list option) = 
  let rec aux in_lst acc = 
    match in_lst with 
    | [] -> acc ^ "]"
    | last :: [] -> acc ^ string_of_int last ^ "]"
    | h :: t -> aux t (acc ^ (string_of_int h) ^ ", ") in
  match lst with
  | None -> ""
  | Some v -> aux v "["

let to_json t = 
  let unoption optional = 
    match optional with 
    | None -> 0
    | Some v -> v in
  {|{"group_id": "|} ^ string_of_int t.id ^ 
  {|", "group_name": "|} ^ t.name ^ 
  {|", "host": |} ^ t.host ^ 
  {|, "users": |} ^ json_int_lst t.users ^ 
  {|", "restaurants": |} ^ json_int_lst t.restaurants ^ 
  {|", "survey": |} ^ json_int_lst t.survey_complete ^ 
  {|, "voting": |} ^ (json_int_lst t.voting_complete) ^ 
  {|, "candidates": |} ^ json_int_lst_opt t.candidates ^ 
  {|, "final_choice": |} ^ string_of_int (unoption t.final_choice) ^ "}"

let get_name t = t.name

let get_users t = t.users

let get_candidates t = t.candidates

let get_winner t = t.final_choice

let has_user_voted t user =
  List.mem user t.voting_complete

let has_group_voted t = 
  List.length t.voting_complete = List.length t.voting_complete

(*We want a function to create a restaurant in group.ml?*)
let create name location cuisine_type rating allergens price wait_time = 
  failwith "unimplemented"


