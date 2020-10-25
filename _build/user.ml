open Yojson.Basic.Util

type t = {
  id : int; 
  username : string;
  mutable password : string;
  name : string; 
  mutable friends : int list;
  mutable restrictions : int list;
  mutable visited : int list;
  mutable groups : int list;
}

let get_id t = t.id

let get_username t = t.username

let get_name t = t.name

let get_restaurants t = t.visited

let get_friends t = t.friends

let get_restrictions t = t.restrictions

let in_group t group = List.mem group t.groups

(*let login = 
  failwith "unimplemented"
  let t_of_preference json = {
  time = json |> member "time" |> to_int;
  dist = json |> member "dist" |> to_int;
  price = json |> member "price" |> to_int;
  openness = json |> member "id" |> to_bool;
  restrictions = json |> member "restrictions" |> to_list |> List.map to_int;
  }
*)

let from_json json = {
  id = json |> member "user_id" |> to_int;
  username = json |> member "username" |> to_string;
  password = json |> member "password" |> to_string;
  name = json |> member "name" |> to_string;
  friends = json |> member "friend_ids" |> to_list |> List.map to_int;
  restrictions = json |> member "restriction_ids" |> to_list |> List.map to_int;
  visited = json |> member "visited" |> to_list |> List.map to_int;
  groups = json |> member "groups" |> to_list |> List.map to_int;
}

(**Returns a string representing [lst] in a JSON format.

   Note: the return is not a valid JSON itself.*)
let json_int_lst (lst : int list) = 
  let rec aux in_lst acc = 
    match in_lst with 
    | [] -> acc ^ "]"
    | last :: [] -> acc ^ string_of_int last ^ "]"
    | h :: t -> aux t (acc ^ (string_of_int h) ^ ", ") in
  aux lst "["

(**[to_json user] is a valid JSON string representing a user. 
   Raw strings are used to name the fields and add brackets.*)
let to_json user = 
  {|{"user_id": |} ^ (string_of_int 1) ^ {|, "name": "|} ^ user.name ^ 
  {|", "username": "|} ^ user.username ^ {|", "password": "|} ^ user.password ^ 
  {|", "friend_ids": |} ^ json_int_lst user.friends ^ 
  {|, "restriction_ids": |} ^
  json_int_lst user.restrictions ^ {|, "visited": |} ^ 
  json_int_lst user.visited ^ "}"

(* "user: {id: 1}" ^ (string_of_int user.id) ^ "}" *)
(*Instantiate a user, 
  added a id variable, pretty sure we need that as an input? -Zach*)
let create id_user user pass name = {
  id = id_user; 
  username = user;
  password = pass;
  name = name;
  friends = [];
  restrictions = [];
  visited = [];
  groups = []
}

let is_friend other_t t =
  let rec helper id = function
    | [] -> false
    | h :: t -> 
      if h == id then true 
      else helper id t in 
  helper other_t.id t.friends

(* Does the order for these lists matter? I would change to prepending function
   to make it constant time efficiency. *)
let add_friend other_t t = 
  if List.mem other_t.id t.friends 
  then t.friends <- other_t.id :: t.friends 
  else ()


(* Based on interface type definitions for other functions like add_friend 
   I am guessing that we would want this to in some way take a restaurant in as
   an input and grab its id, however with functions that we can access outside
   the restaurant module I am not quite sure how to do this, might just be 
   missing something. Would just require a get id function in Restaurant just 
   don't want to mess with Restaurant.ml- Zach. *)
let add_visited int t = t.visited <- int :: t.visited

(*Commented out guessing we don't want this function in user*)
(*let add_restaurant id t = t.visited <- t.visited @ [id]*)

let change_restrictions restrictions t = t.restrictions <- restrictions

(**Appends the id of the last group joined to the end of the group list. 
   Specification says it should go to end of the list, However I would change it
   in the same way I did for add_friend and add_visited if order does not matter
   .*)
let update_groups t group = t.groups <- group :: t.groups

