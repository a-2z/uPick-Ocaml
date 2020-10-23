open Yojson.Basic.Util

(*
type preference = {
  time : int;
  dist : int;
  price : int;
  openness : bool;
  restrictions : int list;
}
*)
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

let get_preferences t = t.restrictions

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

(**[to_json user] is a valid JSON string representing a user. Raw strings are used to name the fields and add brackets.*)
let to_json user = 
  {|{"user_id": |} ^ (string_of_int 1) ^ {|, "name": "|} ^ user.name ^ {|", "username": "|} ^ user.username ^ {|", "password": "|} ^ user.password ^ {|", "friend_ids": |} ^ json_int_lst user.friends ^ {|, "restriction_ids": |} ^
  json_int_lst user.restrictions ^ {|, "visited": |} ^ json_int_lst user.visited ^ "}"

(* "user: {id: 1}" ^ (string_of_int user.id) ^ "}" *)
(*Instantiate a user*)
let create_user user pass name = {
  id = 0; 
  username = user;
  password = pass;
  name = name;
  friends = [];
  restrictions = [];
  visited = [];
  groups = []
}

let add_friend id t = t.friends <- t.friends @ [id]

let is_friend id t = 
  let rec helper id = function
    | [] -> false
    | h :: t -> 
      if h == id then true 
      else helper id t in 
  helper id t.friends

let add_restaurant id t = t.visited <- t.visited @ [id]

let change_preferences restrictions t = t.restrictions <- restrictions

(**Appends the id of the last group joined to the end of the group list*)
let update_groups t group = t.groups <- t.groups @ [group]

