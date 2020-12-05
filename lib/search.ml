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

(*Zomato API Key*)
let user_key = "0b2c1f4d2cea4f954b70b3d12683036c"

let string_of_t t = 
  List.map (fun hd -> let str = Printf.sprintf 
                          {|
   {
   	"name": "%s",
   	"address": "%s",
   	"cuisines": "%s",
   	"price": %d,
   	"highlights": ["%s"],
   	"rating": %f,
   	"photo": "%s",
   	"timing": "%s",
   	"phone": "%s",
   	"reservation": %b,
   	"takeout": %b
   }
   |} hd.name hd.address hd.cuisines hd.price 
                          (String.concat "\", \"" hd.highlights) hd.rating 
                          hd.photo hd.timing hd.phone hd.reservation hd.takeout 
             in print_endline str; str) t
  |> String.concat ",\n"
  |> (fun l -> "{\"restaurants\": [\n" ^ l ^ "\n]}")

let split_str_lst l = 
  match String.split_on_char ',' l with
  | [] -> ""
  | h :: _ -> String.trim h

(**[splice n lst] returns the first [n] elements of [lst]
   Requires: [lst] has at least [n] elements.*)
let splice n lst =
  let rec aux lst i acc = 
    if i = n then 
      acc 
    else 
      match lst with 
      | [] -> failwith "invariant violated"
      | h :: t -> aux t (i + 1) (h :: acc)
  in List.rev (aux lst 0 [])

let to_rest json = 
  let json = member "restaurant" json in
  {
    name = json |> member "name" |> to_string;
    address = json |> member "location" |> member "address" |> to_string;
    cuisines = json |> member "cuisines" |> to_string;
    price = json |> member "average_cost_for_two" |> to_int |> (fun p -> p / 2);
    highlights = json |> member "highlights" |> to_list |> List.map to_string; 
    rating = json |> member "user_rating" |> member "aggregate_rating" 
             |> to_float;
    photo = json |> member "photos_url" |> to_string;
    timing = json |> member "timings" |> to_string;
    phone = json |> member "phone_numbers" |> to_string |> split_str_lst;
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

(**Sets a semi-arbitrary upper bound for [price].*)
let set_bound price = begin
  match float_of_int price with 
  | p when p <= 70. -> p *. (1.4 -. (p -. 10.) *. 0.005)
  | p -> p 
end
                      |> int_of_float

let filter_results price l = 
  let filtered = List.filter (fun x -> (x.price <= price)) l in 
  if List.length filtered <= 5 then l |> splice 5
  else filtered |> splice 5

let get_rests ?num:(n = 20) ?cuisine:(c = []) loc_x loc_y range price =
  let return = ref "" in
  let price = set_bound price in
  let hdr = add_list (init ()) 
      [("Accept", "application/json"); ("user-key", user_key)] in 
  let url = Printf.sprintf 
      {|https://developers.zomato.com/api/v2.1/search?count=%d&lat=%f&lon=%f&radius=%f&cuisines=%s&sort=rating&order=desc|} 
      n loc_x loc_y (float_of_int range) (String.concat "%2c" c) in ignore(
    Cohttp_lwt_unix.Client.get ~headers:hdr (Uri.of_string url)
    >>= fun a -> snd a 
                 |> Cohttp_lwt__.Body.to_string 
    >>= fun b -> let fmt = (from_string b 
                            |> from_body 
                            |> filter_results price
                            |> string_of_t) in 
    return := fmt;
    Lwt.return (from_string fmt));
  !return