open Cohttp.Header
open Lwt.Infix
open Yojson.Basic
open Yojson.Basic.Util

type result = {
  name : string;
  address : string;
  cuisines : string;
  price : int;
  highlights : string list;
  rating : float;
  photo : string;
  timing : string;
  phone : string;
  reservation : bool;
  takeout : bool;
}

type t = result list

(*Zomato API Key*)
let user_key = "0b2c1f4d2cea4f954b70b3d12683036c"

let string_of_t t = 
  List.map (fun hd -> Printf.sprintf {|
			{
				"name": "%s",
				"address": "%s",
				"cuisines": "%s",
				"price": %d,
				"highlights: ["%s"],
				"rating": %f,
				"photo": "%s",
        "timing": "%s",
				"phone": "%s",
				"reservation": %b,
				"takeout": %b
				}
			|} hd.name hd.address hd.cuisines hd.price 
			(String.concat "\", \"" hd.highlights) 
							hd.rating hd.photo hd.timing hd.phone hd.reservation hd.takeout) t
							|> String.concat ",\n"
							|> (fun l -> "[\n" ^ l ^ "\n]")

let is_optional = function
| None -> "Not Available"
| Some o -> o

(****TODO: IMPLEMENT OPTIONS *)
let to_rest json = 
  {
    name = json |> member "restaurant" |> to_string;
    address = json |> member "location" |> member "address" |> to_string;
    cuisines = json |> member "cuisines" |> to_string;
    price = json |> member "average_cost_for_two" |> to_int |> (fun p -> p / 2);
    highlights = json |> member "highlights" |> to_list |> List.map to_string;
    rating = json 
             |> member "user_rating" 
             |> member "aggregate_rating" 
             |> to_string
             |> float_of_string;
    photo = json |> member "photos_url" |> to_string;
    timing = json |> member "timings" |> to_string;
    phone = json |> member "phone_numbers" |> to_list |> List.hd |> to_string;
    reservation = json |> member "is_table_reservation_supported" 
                  |> to_int 
                  |> ( <> ) 0;
    takeout = json |> member "has_online_delivery" 
              |> to_int 
              |> ( = ) 1;
  }

let from_body json = 
  try
    json 
    |> member "restaurants" 
    |> to_list  (*restaurant list*)
    |> List.map to_rest
  with Type_error (s, _) -> failwith ("Parsing error: " ^ s)

let get_rests ?num:(n = 5) loc_x loc_y range cuisine =
  let hdr = add_list (init ()) 
      [("Accept", "application/json"); ("user-key", user_key)] in 
  let url = Printf.sprintf 
      {|https://developers.zomato.com/api/v2.1/search?lat=%f&lon=
%f&radius=%f&cuisines=%s&sort=rating&order=desc|} 
      loc_x loc_y range (String.concat "%2c" cuisine) in
  Cohttp_lwt_unix.Client.get ~headers:hdr (Uri.of_string url)
  >>= fun a -> snd a 
               |> Cohttp_lwt__.Body.to_string 
  >>= fun b -> (from_string b 
                |> from_body 
                |> string_of_t 
                |> print_endline); 
  Lwt.return b 