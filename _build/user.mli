open Yojson.Basic.Util

(** The type for the user. Regardless of implementation this should include
    a user's username, password, name, visited restaurants list, friends list,    and the automatic ranked preference between location, price, and meal 
    quality, and if the user is currently in a group *)
type t

(** The following are all get functions for data stored in type t, be sure to 
    implement them consistently based on the implementation of t *)

val get_id : t -> int

val get_username : t -> string 

val get_name : t -> string 

val get_restaurants : t -> int list

val get_friends : t -> int list

(**Check whether or not a user is in a group*)
val get_in_group : t -> int -> bool

(**Returns the data for the user taken as a json as type t to be used with
    our built functionality for the user module. *)
val get_json : Yojson.Basic.t -> t (* Consider using Yojson.Basic.t as 'a *)

(**Creates a user based on username, password, and name. Returns true
if creation was successful and false if it failed.*)
val create_user : string -> string -> string -> unit -> bool

(** Take's in the id of a user and adds that to a list of friends*)
val add_friend : t -> t -> unit

(** Returns true if a user is on the friends list of the user of interest
    and vice-versa*)
val is_friend : t -> t -> bool

(** Takes in the id of a restaurant and adds that to a list of restaurants 
    visited*)
val add_restaurant : int -> t -> unit

(** Take's in the new preference list and updates it for the user.*)
val change_preferences: int list -> t -> unit

(** Updates if the user joins or leaves a group*)
val update_groups : t -> unit
