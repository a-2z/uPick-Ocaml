open Yojson.Basic.Util

type t = {
  id : int;
  name : string;
  location : string;
  food_type : string;
  rating : float;
  allergies : string list;
  avg_price : string;
}

let get_name t = 
  t.name

let get_location t =
  t.location

let get_food_type t = 
  t.food_type

let get_allergies t = 
  t.allergies

let get_rating t =
  t.rating

let get_price t =
  t.avg_price

(* yojson problem *)
let from_json json = 
  {
    id = json |> member "id" |> to_int;
    name = json |> member "name" |> to_string;
    location = json |> member "location" |> to_string;
    food_type = json |> member "food_type" |> to_string;
    rating = json |> member "rating" |> to_float;
    allergies = json |> member "allergies" |> to_list |> List.map to_string;
    avg_price = json |> member "avg_price" |> to_string
  }
