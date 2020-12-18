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

let run_tests () = run_test_tt_main tests

let send_get () = Cohttp_lwt_unix.Client.get (Uri.of_string "http://localhost:3000/users/2") 
  >>= fun a -> print_endline "hi"; snd a 
                                   |> Cohttp_lwt__.Body.to_string 
  >>= fun b -> b |> fun x -> print_endline b; x |> Lwt.return

(**Fix later *) 
let _ = Lwt_timeout.start run_server; run_tests (); ignore(send_get ());