(*Test Plan: We used a mix of manual and automated testing to ensure the robustness of our backend. 

  ********************************Automated (Dbquery, Search)******************

  We used OUnit2 specifically to test interaction with the Database in terms of in
  sertions, updates, and deletions. By performing operations using the in
  terfacing get and set functions in the Dbquery module, we thoroughly 
  checked that data were formatted correctly if acceptable and rejected if 
  formatted incorrectly. We directly assessed Dbquery and Search, as the 
  purpose of Db was to handle database table creation, which was not changed 
  throughout the development process; testing of it was handled manually. 
  The Server module was tested manually as well, because testing routes us
  ing OUnit2 presented challenges with request creation and running the te
  st suite and server concurrently. T

  Types of testing: We used a mix of black box and glass box testing for the
  automatic portion. We created tests by using "make docs" and using the 
  generated documentation to ensure that specifications were met. We then reviewed
  the actual implementation to ensure coverage of all branches. We did not utilize randomized testing. 

  Manual (Db, Server)  
  We used Postman to specifically test our http regarding adding a new user, friends, restriction, or creating a new group. With Postman we can also manually test whether our login were correct and if our hashing and Bcrypt of passwords were correct from the server file.
  From Postman we can see whether or unique keys were created properly 
  and we can see whether our get and post functions were implemented correctly 
  on Postman through a raw JSON body format to have a visual representation of 
  what users,rfriends, restrictions, or groups were created. In addition, we used 
  DB Browser to manually test out sqlite 3 tables where w visual table format is 
  presented to determine whether our functions performed properly and if tables
  are created and formatted correctly. 

  Types of testing: We used a combination of both black box and glass box testing for the manual portion. We 

*)

(*TEST SUITE *)
open OUnit2
(* open Lib.Dbquery *)
open Lwt.Infix
open Lib.Server
(* open Opium.Std *)
open Yojson.Basic
open Yojson.Basic.Util

(* URI pointing to local host*)
let make_uri route = Uri.of_string ("http://localhost:3000" ^ route)

let is_success json = json |> from_string |> member "success" |> to_bool

let make_body = Cohttp_lwt__Body.of_string

(* let send_get route body unwrapper = 
   Cohttp_lwt_unix.Client.post ~body:(make_body body) (make_uri route)
   >>= fun a -> snd a 
               |> Cohttp_lwt__.Body.to_string 
   >>= fun b -> b |> unwrapper |> Lwt.return *)

let test_equal name exptd expr = 
  name >:: (fun _ -> assert_equal exptd expr) 

let port_test = [
  test_equal "two is two" 2 2;
  test_equal "port is 3000" port 3000;
]
(* 
let add_user_test = [
  assert_equal "add andrew01 user" 
] *)

let tests = "test suite for uPick" >::: List.flatten [
    (* port_test; *)
  ]

let run_server = Lwt_timeout.create 1 (fun () -> start ())

                 |> Coh__.Body.to_
  >>= fun a -> print_endline "hi"; snd a 
                                   |> Co 
                                   |> Co_string_string_string_string_string_string_string_string_string_string_string_string_string_string_string_string_string_string_stringhttp_lwt__.Body.to_stringhttp_lwt__.Body.to_stringhttp_lwt__.Body.to_string 
                                   |> Cohttp_lwt__.Body.to_stringhttp_lwt__.Body.to_string 
