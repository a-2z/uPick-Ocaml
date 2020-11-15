open Dbquery
open Lwt.Infix
open Opium.Std
open Yojson.Basic
open Yojson.Basic.Util

(**************************JSON builders and parsers***************************)
(**[load_json req ins_func insrter] inserts the contents of load_json
   into the database*)
let load_json req ins_func inserter =
  req.Request.body
  |> Body.to_string
  >>= fun a -> 
  Lwt.return (inserter (from_string a) ins_func)

let json_of_user {id; username; password; name; friends; restrictions; 
                  groups} =
  let open Ezjsonm in
  dict [("id", int id); ("username", string username); 
        ("password", string password);
        ("name", string name); ("friends", list int friends);
        ("restrictions", list int restrictions);
        ("groups", list int groups);]

let json_of_group {id; name; host_id; members} = 
  let open Ezjsonm in 
  dict [("id", int id); ("name", string name); ("host_id", int host_id); 
        ("members", list int members)]

(* grabs by user id *)
let json_of_restriction_id restriction = 
  let open Ezjsonm in 
  dict [("restriction", string restriction)]

(*****************************database inserters*******************************)
(**[user_inserter json ins_func] inserts the data representing a user into
   the database*)
let user_inserter json ins_func = 
  ins_func
    (member "username" json |> to_string)
    (member "password" json
     |> to_string 
     |> Bcrypt.hash 
     |> Bcrypt.string_of_hash) 
    (member "name" json |> to_string)

let friend_inserter json ins_func = 
  ins_func
    (member "friend1" json |> to_int)
    (member "friend2" json |> to_int)

let rest_inserter json ins_func = 
  ins_func
    (member "user_id" json |> to_int)
    (member "restriction_id" json |> to_int)

let rest_indx_inserter json ins_func = 
  ins_func
    (member "restriction" json |> to_string)

let group_info_inserter json ins_func = 
  ins_func
    (member "group_name" json |> to_string)
    (member "host_id" json |> to_int)

let group_inserter json ins_func = 
  ins_func
    (member "group_id" json |> to_int)
    (member "member_id" json |> to_int)

let make_response = function
  | Some id -> respond' 
                 (`Json (Ezjsonm.from_string 
                           ({|{"success": true, "id": |} ^ 
                            Int64.to_string id ^ "}")))
  | None -> respond' (`Json (Ezjsonm.from_string {|{"success": false|}))  

(*******************************route list*************************************)

(* Route not found *)     
let default =
  not_found (fun _req ->
      `Json Ezjsonm.(dict [("message", string "Route not found")]) |> respond')

(* grabs a list of all restrictions that exist *)
(*Added a function for restrictions even though it was not in interface *)

let get_list = [
  (* user *)
  get "/users/:id" (fun req -> 
      let user = Dbquery.get_user (int_of_string (param req "id")) in
      `Json (user |> json_of_user) |> respond'); 

  (* groups *)  
  get "/groups/:id" (fun req ->
      let group = Dbquery.get_group (int_of_string (param req "id")) in
      `Json (group |> json_of_group) |> respond');

  (* get restriction by id *)
  get "/restrictions/:id" (fun req ->
      try 
        let restriction = Dbquery.get_restriction_by_id 
            (int_of_string (param req "id")) in 
        (`Json 
           (Ezjsonm.from_string 
              (Printf.sprintf {|{"success": true, "data": "%s"}|} restriction)))
        |> respond'
      with e -> ignore (e); 
        respond' (`Json (Ezjsonm.from_string {|{"success": false|})));

  (* get all restrictions *)
  get "/restrictions" 
    (fun _ -> let restriction = Dbquery.get_restrictions () in 
      `Json (Ezjsonm.list Ezjsonm.string restriction) |>  
      respond');
]

let post_list = [
  (* let insert_user =  *)
  post "/users" (fun req -> 
      load_json req add_user user_inserter >>= fun a -> make_response a);

  (* let insert_friends =  *)
  post "/friends" (fun req -> 
      load_json req add_friends friend_inserter >>= fun a ->
      make_response a);

  (* let insert_restriction =  *)
  post "/restrictions" (fun req -> 
      load_json req add_restrictions rest_inserter >>= 
      fun a -> make_response a);

  (* let insert_restrictions_index =  *)
  post "/restrictions/add" (fun req -> 
      load_json req add_restrictions_index rest_indx_inserter >>= 
      fun a -> make_response a);

  (* let insert_group =  *)
  post "/groups" (fun req -> 
      load_json req add_groups group_inserter >>= 
      fun a -> make_response a);

  post "/groups/add" (fun req -> 
      load_json req add_group_info group_info_inserter >>= 
      fun a -> make_response a);

  post "/login" (fun req -> 
      req.Request.body
      |> Body.to_string
      >>= fun a -> 
      let usrname = a 
                    |> from_string 
                    |> member "username"
                    |> to_string in 
      let pw = a
               |> from_string 
               |> member "password"
               |> to_string in 
      match (Dbquery.login usrname) with
      | None -> respond' 
                  (`Json (Ezjsonm.from_string {|{"success": false}|}))  
      | Some password -> begin 
          if Bcrypt.verify pw (Bcrypt.hash_of_string password) then
            respond' 
              (`Json (Ezjsonm.from_string {|{"success": true}|}))
          else respond' 
              (`Json (Ezjsonm.from_string {|{"success": false}|})) 
        end);
]

let rec app_builder lst app = 
  match lst with 
  | [] -> app
  | h :: t -> app_builder t (app |> h)

let port = 5000

let _ = 
  create_tables (); 
  print_endline 
    ("Server running on port http://localhost:" ^ string_of_int port);
  App.empty
  |> App.port port
  |> default 
  |> app_builder get_list
  |> app_builder post_list
  |> App.run_command

