open Dbquery
open Lwt.Infix
open Opium.Std
open Yojson.Basic
open Yojson.Basic.Util

(*Helper functions*)

(**[user_inserter json ins_func] inserts the data representing a user into
the database*)
let user_inserter json ins_func = 
  ins_func
    ((member "username" json) |> to_string) 
    ((member "password" json) 
     |> to_string 
     |> Bcrypt.hash 
     |> Bcrypt.string_of_hash) 
    ((member "name" json) |> to_string)

let friend_inserter json ins_func = 
  ins_func
    ((member "friend1" json) |> to_int) 
    ((member "friend2" json) |> to_int)

let rest_inserter json ins_func = 
  ins_func
    ((member "user_id" json) |> to_int) 
    ((member "restriction_id" json) |> to_int)

let rest_indx_inserter json ins_func = 
  ins_func
    ((member "restriction" json) |> to_string) 

let group_info_inserter json ins_func = 
  ins_func
    ((member "group_name" json) |> to_int) 
    ((member "host_id" json) |> to_int)

let group_inserter json ins_func = 
  ins_func
    ((member "group_id" json) |> to_int) 
    ((member "member_id" json) |> to_int)
    
(**[load_json req ins_func insrter] inserts the contents of load_jso
  into the database*)
let load_json req ins_func inserter =
  req.Request.body
  |> Body.to_string
  >>= fun a -> 
  Lwt.return (inserter (from_string a) ins_func)

let default =
  not_found (fun _req ->
      `Json Ezjsonm.(dict [("message", string "Route not found")]) |> respond')

let json_of_user {id; username; password; name; friends; restrictions; 
	groups} =
  let open Ezjsonm in
  dict [("id", int id); ("username", string username); 
	("password", string password);
        ("name", string name); ("friends", list int friends);
        ("restrictions", list int restrictions);
        ("groups", list int groups);]

let get_user =
  get "/user/:id" (fun req -> 
			let user = Dbquery.get_user (int_of_string (param req "id")) in
      `Json (user |> json_of_user) |> respond')

(* let get_user_groups = 
  get "/invites/:id" (fun req -> 
      let user = Dbquery.get_user (int_of_string (param req "id")) in
      `Json (user.groups |> Ezjsonm.list Ezjsonm.int) |> respond') *)

(* let json_of_group {id; host_id; member_id} = 
  let open Ezjsonm in 
  dict [("id", int id); ("host_id", int host_id); ("member_id", int member_id)]

let get_group = 
  get "groups/:id" (fun req ->
      let group = Dbquery.get_group (int_of_string (param req "id")) in
      `Json (group |> json_of_group) |> respond')

let json_of_restaurant {id; name; price; image; rating; description; wait_time;
                        phone; location_x; location_y} = 
  let open Ezjsonm in 
  dict [("id", int id); ("name", string name); ("price", int price); 
        ("image", string image); ("rating", int rating); 
        ("description", string description); ("wait_time", int wait_time); 
        ("phone", string phone); ("location_x", float location_x); 
        ("location_y", float location_y)]

let get_restaurant = 
  get "restaurants/:id" (fun req ->
      let group = Dbquery.get_restaurant (int_of_string (param req "id")) in
      `Json (group |> json_of_group) |> respond')

let json_of_friends {friend1; friend2} = 
  let open Ezjsonm in 
  dict [("friend1", int friend1), ("friend2", int friend2)]

let get_friends = 
  get "pending/:id" (fun req ->
      let friends = Dbquery.get_friends (int_of_string (param req "id")) in 
      `Json (friends |> json_of_friends) |> respond')

let json_of_restriction {user_id; restriction} = 
  let open Ezjsonm in 
  dict [("user_id", int user_id), ("restriction", string restriction)]

(**Added a function for restrictions even though it was not in interface *)
let get_restriction = 
  get "restriction/:id" (fun req ->
      let restriction = Dbquery.get_restriction 
          (int_of_string (param req "id")) in 
      `Json (restriction |> json_of_restriction) |> respond') *)

(* let get_group =
  get "/groups/:id" (fun req -> 
			let grp = Dbquery.get_group (param req "id") in
      `Json (grp |> json_of_gr) |> respond') *)

(* let print_json (req : Request.t) =
   req.body |> Request. |>

              let post_user =
                post "/user" print_json *)
                (*POST Methods *)
                
let make_response = function
      | Some id -> respond' 
                     (`Json (Ezjsonm.from_string 
                               ({|{"success": true, "id": |} ^ 
                                Int64.to_string id ^ "}")))
      | None -> respond' (`Json (Ezjsonm.from_string {|{"success": false|}))          

let insert_user = 
  post "/users" (fun req -> 
      load_json req (add_user) user_inserter >>= fun a -> make_response a)

let insert_friends = 
  post "/friends" (fun req -> 
      load_json req (add_friends) friend_inserter >>= fun a ->
      make_response a)

let insert_restriction = 
  post "/restrictions" (fun req -> 
      load_json req (add_restrictions) rest_inserter >>= 
      fun a -> make_response a)
      
let insert_restrictions_index = 
  post "/restrictions/add" (fun req -> 
      load_json req (add_restrictions_index) rest_indx_inserter >>= 
      fun a -> make_response a)

let _ = 
create_tables (); 
print_endline "Server running on port http://localhost:3000";
  App.empty 
	|> default 
	|> get_user 
  (* |> get_user_groups
  |> get_group
  |> get_restaurant
  |> get_friends
  |> get_restriction *)
  |> insert_user
  |> insert_restriction
  |> insert_restrictions_index
  |> insert_friends
  |> App.run_command

  