open Dbquery
open Lwt.Infix
open Opium.Std
open Yojson.Basic
open Yojson.Basic.Util

(**Creates a server module with database query module M1 and database module 
   M2 with names "upick.db"*)
(* module type ServerMaker = functor (M1 : Dbquery) -> functor (M2 : Db) ->  *)

(**************************JSON builders and parsers***************************)
exception Login_failure of string

let login json =
  let pw = (member "password" json |> to_string) in 
  let stor_pw = (member "username" json |> to_string |> login) in 
  match stor_pw with 
  | None -> raise (Login_failure ", Invalid username or password")
  | Some p -> if Bcrypt.verify pw (Bcrypt.hash_of_string p) then ()
    else raise (Login_failure ", Invalid username or password")

let make_response = function
  | Some id -> respond' 
                 (`Json (Ezjsonm.from_string 
                           ({|{"success": true, "id": |} ^
                            Int64.to_string id ^ "}")))
  | None -> respond' (`Json (Ezjsonm.from_string 
                               {|{"success": false }|}))  

let load_json req ins_func inserter =
  req.Request.body
  |> Body.to_string
  >>= fun a -> 
  Lwt.return (inserter (from_string a) ins_func)

(**[load_json req ins_func inserter] inserts the contents of load_json
   into the database*)
let load_json_login req ins_func inserter =
  req.Request.body
  |> Body.to_string
  >>= fun a -> 
  try 
    let json = (from_string a) in 
    login json;
    Lwt.return (inserter (from_string a) ins_func) >>= fun a -> make_response a
  with 
  | Login_failure _ -> (fun x -> 
      print_endline "login credentials did not match"; x)
                         Lwt.return None >>= fun a -> make_response a

let json_of_user 
    {id; username; password; name; friends; restrictions; groups} =
  ignore (password); (*Do not return the user's password on a get request*)
  let open Ezjsonm in
  dict [("id", int id); ("username", string username); 
        ("name", string name); ("friends", list int friends);
        ("restrictions", list int restrictions);
        ("groups", list int groups);]

let json_of_group 
    {id; name; host_id; members; voting_allowed; top_5; top_pick} = 
  let open Ezjsonm in 
  dict [("id", int id); ("name", string name); ("host_id", int host_id); 
        ("members", list int members); ("voting_allowed", bool voting_allowed); 
        ("top_5", option string top_5); 
        ("top_pick", option string top_pick)]

(* grabs by user id *)
let json_of_restriction_id restriction = 
  let open Ezjsonm in 
  dict [("restriction", string restriction)]

(**Checks to see if [u_id] is in [g_id*)
let is_member g_id u_id =
  List.mem g_id (get_user u_id).groups

(**Checks to see if two users are friends, with [f1] being the sender*)
let is_friend f1 f2 = 
  List.mem f2 (get_user f1).friends

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
    (member "username" json |> to_string |> id_by_usr)

let group_host_user_inserter json ins_func = 
  let h_id = id_by_usr (member "username" json |> to_string) in 
  ins_func 
    (member "group_id" json |> to_int)
    (member "member_id" json |> to_int)
    h_id

let survey_inserter json ins_func = 
  let u_id = id_by_usr (member "username" json |> to_string) in
  if is_member (member "group_id" json |> to_int) u_id then 
    begin 
      ins_func 
        u_id
        (member "group_id" json |> to_int)
        (member "loc_x" json |> to_float)
        (member "loc_y" json |> to_float)
        (member "cuisine" json |> to_string)
        (member "price" json |> to_int)
        (member "range" json |> to_int)
    end 
  else None 

let vote_status_inserter json ins_func = 
  let h_id = id_by_usr (member "username" json |> to_string) in 
  ins_func 
    (member "group_id" json |> to_int)
    h_id

let vote_inserter json ins_func = 
  let u_id = id_by_usr (member "username" json |> to_string) in
  if is_member (member "group_id" json |> to_int) u_id then
    begin 
      ins_func 
        (member "group_id" json |> to_int)
        (member "username" json |> to_string |> id_by_usr)
        (member "votes" json |> to_list |> List.map to_int)
    end 
  else (fun x -> print_endline "was not in group"; x) None 

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
      load_json_login req add_friends friend_inserter);

  (* let insert_restriction =  *)
  post "/restrictions" (fun req -> 
      load_json req add_restrictions rest_inserter >>= 
      fun a -> make_response a);

  (* let insert_restrictions_index =  *)
  post "/restrictions/add" (fun req -> 
      load_json req add_restrictions_index rest_indx_inserter >>= 
      fun a -> make_response a);

  (* let insert_group =  *)
  post "/groups/join" (fun req ->
      load_json_login req join_group group_inserter);

  post "/groups/add" (fun req ->
      load_json_login req add_group_info group_info_inserter);

  post "/groups/newhost" (fun req ->
      load_json_login req reassign_host group_host_user_inserter);

  post "/groups/rmuser" (fun req ->
      load_json_login req delete_from_group group_host_user_inserter);

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
      | Some password -> 
        if Bcrypt.verify pw (Bcrypt.hash_of_string password) then
          respond' 
            (`Json (Ezjsonm.from_string {|{"success": true}|}))
        else respond' 
            (`Json (Ezjsonm.from_string {|{"success": false}|})));

  (*Voting Routes*)
  post "/survey" (fun req ->
      load_json_login req ans_survey survey_inserter);

  post "/ready" (fun req -> 
      load_json_login req process_survey vote_status_inserter);

  post "/vote" (fun req ->
      load_json_login req add_votes vote_inserter);

  post "/done" (fun req ->
      load_json_login req calculate_votes vote_status_inserter);
]

let rec app_builder lst app = 
  match lst with 
  | [] -> app
  | h :: t -> app_builder t (app |> h)

let port = 3000

let start () = 
  create_tables (); 
  print_endline 
    ("Server running on port http://localhost:" ^ string_of_int port);
  App.empty
  |> App.port port
  |> default 
  |> app_builder get_list
  |> app_builder post_list
  |> App.run_command

