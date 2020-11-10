open Yojson.Basic.Util
open Json_helpers

type t = {
  id : int;
  name : string;
  location_x : float;
  location_y : float;
  food_type : string;
  rating : float;
  allergies : string list;
  avg_price : int;
  avg_wait : int;
}

let from_json json = 
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    location_x = json |> member "location" |> to_float;
    location_y = json |> member "location" |> to_float;
    food_type = json |> member "food_type" |> to_string;
    rating = json |> member "rating" |> to_float;
    allergies = json |> member "allergies" |> to_list |> List.map to_string;
    avg_price = json |> member "avg_price" |> to_int;
    avg_wait = json |> member "avg_wait" |> to_int
  }


let to_json t = 
  {|{"id": "|} ^ string_of_int t.id ^ {|", "name": "|} ^ t.name ^ 
  {|", "location_x": |} ^ string_of_float t.location_x ^ {|, "location_y": |} ^ 
  string_of_float t.location_y ^ {|, "food_type": "|} ^ t.food_type ^ 
  {|", "rating": |} ^ string_of_float t.rating ^ {|, "allergies": |} ^ 
  (json_string_lst t.allergies) ^ {|, "avg_price": |} ^ 
  string_of_int t.avg_price ^ {|, "avg_wait": |} ^ string_of_int t.avg_wait ^
  "}"

let get_id t = 
  t.id

let get_name t = 
  t.name 

let get_x t =
  t.location_x

let get_y t =
  t.location_y

let get_cuisines t = 
  t.food_type

let get_allergies t = 
  t.allergies

let get_rating t =
  t.rating

let get_price t =
  t.avg_price

let create 
    res_id name loc_x loc_y cuisine_type rating allergens price wait_time =
  {
    id = res_id;
    name = name;
    location_x = loc_x;
    location_y = loc_y;
    food_type = cuisine_type;
    rating = rating;
    allergies = allergens;
    avg_price = price;
    avg_wait = wait_time;
  }


