open Yojson.Basic
open Opium

let sf = string_of_float
let get_rests ?num:(n = 5) loc_x loc_y range cuisine =
  let url = "https://developers.zomato.com/api/v2.1/search?lat=" ^ sf loc_x ^ 
	"&lon=" ^ sf loc_y ^ "&radius=" ^ sf range ^ "&cuisines=%22" ^ cuisine ^ "
	%22&sort=rating&order=desc" in Cohttp_lwt_unix.Client.get (Uri.of_string url)
	>>= fun a -> ignore ()