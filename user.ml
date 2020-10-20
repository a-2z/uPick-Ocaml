open Yojson.Basic.Util

type restrictions = Dairy | Shellfish 

type foods = Dairy | Shellfish | Cake | Mexican 

type preference = 
  {
    time : int;
    dist : int;
    price : int;
    openness : bool;
    restrictions : restrictions;
  }

type t = 
  {
    id : int; 
    username : string;
    password : string;
    name : string; 
    friends : int list;
    preferences : preference;
    visited : int list;
  }

let get_id t = t.id

let get_username t = t.username

let get_name t = t.name

let get_restaurants t = t.visited

let get_friends t = t.username

let get_preferences t = t.preferences

let get_in_group t = failwith "Unimplemented"