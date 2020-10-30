open Lwt.Infix
open Graphql_lwt
open Graphql

type restrictions = Dairy | Egg

type user = {
  id : int; 
  username : string;
  password : string;
  name : string; 
  friends : int list;
  restrictions : restrictions list;
  visited : int list;
  groups : int list;
}

let users = [
  { id = 1;
  username = "reetuparikh";
  password = "reetu123";
  name = "Reetu"; 
  friends = [];
  restrictions = [Dairy];
  visited = [];
  groups = []}
]

let rstrictions = Schema.(enum "restrictions"
  ~doc:"The restrictions of a user"
  ~values:[
    enum_value "Dairy" ~value:Dairy;
    enum_value "Egg" ~value:Egg;
  ]
)

let user = Schema.(obj "user"
  ~doc:"A user in the system"
  ~fields:(fun _ -> [
    field "id"
      ~doc:"Unique user identifier"
      ~typ:(non_null int)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.id)
    ;
    field "username"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.username)
    ;
    field "password"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.password)
    ;
    field "name"
      ~typ:(non_null string)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.name)
    ;
    field "friends"
      ~typ:(non_null int list)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.friends)
    ;
    field "restrictions"
      ~typ:(non_null restrictions list)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.restrictions)
    ;
    field "visited"
      ~typ:(non_null int list)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.visited)
    ;
    field "groups"
      ~typ:(non_null int list)
      ~args:Arg.[]
      ~resolve:(fun info p -> p.groups)
  ])
)

let schema = Schema.(schema [
  field "users"
    ~typ:(non_null (list (non_null user)))
    ~args:Arg.[]
    ~resolve:(fun info () -> users)
])

module Graphql_cohttp_lwt =
  Graphql_cohttp.Make (Graphql_lwt.Schema) (Cohttp_lwt_unix.IO)
    (Cohttp_lwt.Body)

let () =
  let open Printf in
  let on_exn = function
    | Unix.Unix_error (error, func, arg) ->
        printf "Client connection error %s: %s(%S)" (Unix.error_message error)
          func arg
    | exn -> printf "Unhandled exception: %s\n%!" (Printexc.to_string exn)
  in
  let callback = Graphql_cohttp_lwt.make_callback (fun _req -> ()) schema in
  let server = Cohttp_lwt_unix.Server.make_response_action ~callback () in
  let port = 8080 in
  let mode = `TCP (`Port port) in
  printf "listening on http://localhost:%d/graphql\n%!" port;
  Cohttp_lwt_unix.Server.create ~on_exn ~mode server |> Lwt_main.run