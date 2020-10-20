open Yojson.Basic.Util

type foods = Dairy | Shellfish | Cake | Mexican 

type preference = {
  time : int;
  dist : int;
  price : int;
  openness : bool;
  restrictions : int list;
}

type t = {
  id : int; 
  username : string;
  password : string;
  name : string; 
  friends : int list;
  preferences : preference;
  visited : int list;
  in_group : bool;
}

let get_id t = t.id

let get_username t = t.username

let get_name t = t.name

let get_restaurants t = t.visited

let get_friends t = t.username

let get_preferences t = t.preferences

let get_in_group t = t.in_group

(* Do we need this function? *)
let create_user user pass name preferences = {
  id = 0; (*??*)
  username = user;
  password = pass;
  name = name;
  friends = [];
  preferences = preferences;
  visited = [];
  in_group = false
}

let t_of_preference json = {
  time = json |> member "time" |> to_int;
  dist = json |> member "dist" |> to_int;
  price = json |> member "price" |> to_int;
  openness = json |> member "id" |> to_bool;
  restrictions = json |> member "restrictions" |> to_list |> List.map to_int;
}

let t_of_json json = {
  id = json |> member "id" |> to_int;
  username = json |> member "username" |> to_string;
  password = json |> member "password" |> to_string;
  name = json |> member "name" |> to_string;
  friends = json |> member "friends" |> to_list |> List.map to_int;
  preferences = json |> member "preferences" |> t_of_preference;
  visited = json |> member "restaurants" |> to_list |> List.map to_int;
  in_group = json |> member "in_group" |> to_bool;
}

let get_json json = t_of_json json

let add_friend id t = {t with friends = id :: t.friends}

let rec is_friend id t = 
  let rec helper id = function
    | [] -> false
    | h :: t -> if h == id then true else helper id t in 
  helper id t.friends

let add_restaurant id t = {t with visited = id :: t.visited}

let change_preferences preference t = {t with preferences = preference}

let update_in_group t = {t with in_group = not t.in_group}

