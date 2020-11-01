open Graphql_lwt
open Yojson.Basic
open Yojson.Basic.Util


type user = {
  id : int; 
  username : string;
  mutable password : string;
  name : string; 
  mutable friends : int list option;
  mutable restrictions : int list option;
  mutable visited : int list option;
  mutable groups : int list option;
}

let from_json_user json = {
  id = json |> member "user_id" |> to_int;
  username = json |> member "username" |> to_string;
  password = json |> member "password" |> to_string;
  name = json |> member "name" |> to_string;
  friends = Some (json |> member "friend_ids" |> to_list |> List.map to_int);
  restrictions = 
    Some(json |> member "restriction_ids" |> to_list |> List.map to_int);
  visited = Some(json |> member "visited" |> to_list |> List.map to_int);
  groups = Some (json |> member "groups" |> to_list |> List.map to_int);
}

let generate_users json = 
  json |> member "users" |> to_list |> List.map from_json_user

(*Empty list as a placeholder *)
let users = generate_users (from_file "users.json")

let user = Schema.(obj "user"
                     ~doc:"A user in the system"
                     ~fields:(fun _ -> [
                           field "id"
                             ~doc:"Unique user identifier"
                             ~typ:(non_null int)
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.id);
                           field "username"
                             ~typ:(non_null string)
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.username);
                           field "password"
                             ~typ:(non_null string)
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.password);
                           field "friends"
                             ~typ:(list(non_null int))
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.friends);
                           field "restrictions"
                             ~typ:(list(non_null int))
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.restrictions);
                           field "visited"
                             ~typ:(list(non_null int))
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.visited); 
                           field "groups"
                             ~typ:(list(non_null int))
                             ~args:Arg.[]
                             ~resolve:(fun _ p -> p.groups);
                           (* io_field "print_input" 
                             ~typ:(non_null int)
                             ~args:[]
                             ~resolve: (fun _ p -> (p.id, Ok p.id));
                           Lwt.return (Ok input_int);
                         ]) *)
                     ])

let schema = Schema.(schema [
    field "users"
      ~typ:(non_null (list (non_null user)))
      ~args:Arg.[]
      ~resolve:(fun _ () -> users)
  ])

module Graphql_cohttp_lwt = Graphql_cohttp.Make (Schema) (Cohttp_lwt_unix.IO)
    (Cohttp_lwt.Body)

let () =
  print_endline("server up");
  let callback = Graphql_cohttp_lwt.make_callback (fun _req -> ()) schema in
  let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in
  let mode = `TCP (`Port 8080) in
  Cohttp_lwt_unix.Server.create ~mode server
  |> Lwt_main.run
