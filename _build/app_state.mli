open User 
open Restaurant 
open Groups


(**The state of the app at any given point, containing information about all users, groups, and restaurants*)

type t 

val add_user : User.t -> t -> unit

open Yojson.Basic.Util
(**The state of the app at any given point, containing information about
   all users, groups, and restaurants*)

type t 

(** Returns id of user about to be added *)
val add_user : t -> int

(**Returns group list user about to be added *)
val old_group : t -> int list

(**Return new group list with user added *)
val new_group : t -> int list
