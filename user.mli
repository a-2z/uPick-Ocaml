<<<<<<< HEAD
open Yojson.Basic.Util

(** The type for the user. Regardless of implementation this should include
    a user's username, password, name, visited restaurants list, friends list,    and the automatic ranked preference between location, price, and meal 
    quality, and if the user is currently in a group *)
type t

(** The following are all get functions for data stored in type t, be sure to 
    implement them consistently based on the implementation of t *)

val get_username : t -> string 

val get_name : t -> string 

val get_restaurants : t -> string list

val get_friends : t -> string list

val get_preferences : t -> 'a list (*maybe??*)

val get_in_group : t -> bool

(** Returns the data for the user taken as a json as type t to be used with
    our built functionality for the user module. *)
val from_json : Yojson.Basic.t -> t (* Consider using Yojson.Basic.t as 'a *)

(** Returns a new user. Takes a username, password, and name
    as an input, and the ranked preference between location, price, and meal 
    quality. !!We need to consider checking for matching usernames ign the 
    database !!*)
val create_user : string -> string -> string -> ('a -> 'a -> 'a -> t) -> t

(** Take's in the username of a user and adds that to a list of friends*)
val add_friend : string -> t -> t

(** Returns true if a user is on the friends list of the user of interest*)
val is_friend : string -> t -> bool

(** Take's in the name of a restaurant and adds that to a list of restaurants 
    visited*)
val add_restaurant : string -> t -> t

(** Take's in the new preferences the user would like to be automatic or for 
    this current group and updates *)
val change_preferences: 'a -> 'a -> 'a -> t

(** Updates if the user joins or leaves a group*)
val update_in_group : bool -> t


=======
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

val get_restaurants : t -> string list

val get_friends : t -> string list

val get_preferences : t -> 'a list (*maybe??*)

val get_in_group : t -> bool

(** Returns the data for the user taken as a json as type t to be used with
    our built functionality for the user module. *)
val from_json : Yojson.Basic.t -> t (* Consider using Yojson.Basic.t as 'a *)

(** Returns a new user. Takes a username, password, and name
    as an input, and the ranked preference between location, price, and meal 
    quality. !!We need to consider checking for matching usernames ign the 
    database !!*)
val create_user : string -> string -> string -> 'a -> t

(** Take's in the id of a user and adds that to a list of friends*)
val add_friend : int -> t -> t

(** Returns true if a user is on the friends list of the user of interest*)
val is_friend : int -> t -> bool

(** Take's in the id of a restaurant and adds that to a list of restaurants 
    visited*)
val add_restaurant : int -> t -> t

(** Take's in the new preferences the user would like to be automatic or for 
    this current group and updates *)
val change_preferences: 'a -> t

(** Updates if the user joins or leaves a group*)
val update_in_group : t -> t


>>>>>>> 34c646e7e7548676171ec97c2adaaad514c6919d
