(* open App_state
   open Db 
   open Sqlite3 *)
open Opium.Std

let e1 = get "/version" (fun _ -> `String "testing" |> respond')

let e2 =
  get "/hello/:name" (fun req ->
      let name = param req "name" in
      `String ("hello " ^ name) |> respond')

let e3 =
  get "/xxx/:x/:y" (fun req ->
      let x = "x" |> param req |> int_of_string in
      let y = "y" |> param req |> int_of_string in
      let sum = float_of_int (x + y) in
      let open Ezjsonm in
      `Json (`A [int x; int y; float sum]) |> respond')

let e4 =
  put "/hello/:x/from/:y" (fun req ->
      let x, y = (param req "x", param req "y") in
      let msg = Printf.sprintf "Hello %s! from %s." x y in
      `String msg |> respond |> Lwt.return)

let set_cookie =
  get "/set/:key/:value" (fun req ->
      let key, value = (param req "key", param req "value") in
      `String (Printf.sprintf "Set %s to %s" key value)
      |> respond
      |> Cookie.set ~key ~data:value
      |> Lwt.return)



let get_cookie =
  get "/get/:key" (fun req ->
      Logs.info (fun f -> f "Getting cookie") ;
      let key = param req "key" in
      let value =
        match Cookie.get req ~key with
        | None -> Printf.sprintf "Cookie %s doesn't exist" key
        | Some s -> s
      in
      `String (Printf.sprintf "Cookie %s is: %s" key value)
      |> respond |> Lwt.return)

let splat_route =
  get "/testing/*/:p" (fun req ->
      let p = param req "p" in
      `String (Printf.sprintf "__ %s __" p ^ (req |> splat |> String.concat ":"))
      |> respond')

let all_cookies =
  get "/cookies" (fun req ->
      let cookies =
        req |> Cookie.cookies
        |> List.map (fun (k, v) -> k ^ "=" ^ v)
        |> String.concat "\n"
      in
      `String (Printf.sprintf "<pre>%s</pre>" cookies) |> respond |> Lwt.return)

(* exceptions should be nicely formatted *)
let throws =
  get "/yyy" (fun _ ->
      Logs.warn (fun f -> f "Crashing...") ;
      failwith "expected failure!")

(* TODO: a static path will not be overriden. bug? *)
let override_static =
  get "/public/_tags" (fun _ ->
      `String "overriding path" |> respond |> Lwt.return)

let app =
  App.empty |> e1 |> e2 |> e3 |> e4 |> get_cookie |> set_cookie |> all_cookies
  |> throws |> middleware Cookie.m
  |> middleware (Middleware.static ~local_path:"./" ~uri_prefix:"/public" ())
  |> splat_route

type person = {name: string; age: int}

let json_of_person {name; age} =
  let open Ezjsonm in
  dict [("name", string name); ("age", int age)]

let print_param =
  put "/hello/:name" (fun req ->
      `String ("Hello " ^ param req "name") |> respond')

let streaming =
  let open Lwt.Infix in
  get "/hello/stream" (fun _req ->
      (* [create_stream] returns a push function that can be used to
         push new content onto the stream. [f] is function that
         expects to receive a promise that gets resolved when the user
         decides that they have pushed all their content onto the stream.
         When the promise forwarded to [f] gets resolved, the stream will be
         closed. *)
      let f, push = App.create_stream () in
      let timers =
        List.map
          (fun t ->
             Lwt_unix.sleep t
             >|= fun () -> push (Printf.sprintf "Hello after %f seconds\n" t))
          [1.; 2.; 3.]
      in
      f (Lwt.join timers))

let default =
  not_found (fun _req ->
      `Json Ezjsonm.(dict [("message", string "Route not found")]) |> respond')

let print_person =
  get "/person/:name/:age" (fun req ->
      let person =
        {name= param req "name"; age= "age" |> param req |> int_of_string}
      in
      `Json (person |> json_of_person) |> respond')

let json_of_user {name; age} =
  let open Ezjsonm in
  dict [("name", string name); ("age", int age)]

let get_user =
  get "/user" (fun _ -> 
      `Json ({name="Andrew"; age=21323} |> json_of_user) |> respond')

let print_json req =
  req |> App.json_of_body_exn
  |> Lwt.map (fun _json -> respond (`String "Received response"))

let post_user =
  post "/user" print_json

let _ =
  App.empty |> print_param |> print_person |> get_user |> post_user |> streaming |> default 
  |> fun x -> print_endline 
    "Server running on port http://localhost:3000"; 
  x
  |> App.run_command

