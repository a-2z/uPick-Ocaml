open Dbquery
open Opium.Std

let default =
  not_found (fun _req ->
      `Json Ezjsonm.(dict [("message", string "Route not found")]) |> respond')

let json_of_user {id; username; password; name; friends; restrictions; visited; 
	groups} =
  let open Ezjsonm in
  dict [("id", int id); ("username", string username); 
	("password", string password);
        ("name", string name); ("friends", list int friends);
        ("restrictions", list int restrictions);
        ("visited", list int visited);
        ("groups", list int groups);]

let get_user =
  get "/user/:id" (fun req -> 
			let user = Dbquery.get_user (int_of_string (param req "id")) in
      `Json (user |> json_of_user) |> respond')

(* let get_group =
  get "/groups/:id" (fun req -> 
			let grp = Dbquery.get_group (param req "id") in
      `Json (grp |> json_of_gr) |> respond') *)

(* let print_json (req : Request.t) =
   req.body |> Request. |>

              let post_user =
                post "/user" print_json *)

let _ = 
create_tables (); 
print_endline "Server running on port http://localhost:3000";
  App.empty 
	|> default 
	|> get_user 
  |> App.run_command

  
(* let _ = create_tables (); add_user "reetuparikh" "reetu123" "Reetu";
add_user "andrewzeng" "andrew123" "Andrew";
add_restrictions 1 "egg"; add_restrictions 1 "meat"; add_restrictions 1 "meat";
add_friends 1 2; add_group_info "party" 1; add_groups 1 3 *)
