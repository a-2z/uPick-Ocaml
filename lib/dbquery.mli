(** type user defines a type for each user *)
type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  groups : int list 
}

(* type group contains all relevant information for a group *)
type group = {
  id : int;
  name : string;
  host_id : int;
  members : int list;
}

(* type restriction defines an association between a user and their 
   restriction *)
type restriction = {
  id : int;
  name : string;
}

(*Insertion functions for the database*)

(**[add_user username password_hash name] registers a user in the database.*)
val add_user : string -> string -> string -> int64 option

(** [add_friends friend1 friend2] inserts a pairing of two friends into the 
    database
    Requires: friend1 is not friend2
    Raises: Invalid_arg *)
val add_friends : int -> int -> int64 option

(** [add_restrictions user_id restriction_id] performs a restriction insertion 
    into a table
    Requires: restriction_id and user_id are both valid integers *)
val add_restrictions : int -> int -> int64 option

(** [add_restrictions_index restriction_name] inserts a restriction into a 
    table and associates it with an id *)
val add_restrictions_index : string -> int64 option

(** [add_group_info group_name host_id] inserts information about a group into
    a table *)
val add_group_info : string -> int -> int64 option

(** [add_groups user_id group_id] *)
val add_groups : int -> int -> int64 option

val add_votes : int -> int -> int list -> int64 option
(* 
val lst_from_col : ?unique:bool -> string -> string -> string -> (
    string -> 'a) -> 'a list *)

val login : string -> string option

val ans_survey : int -> int -> float -> float -> string -> int -> int
  -> int64 option

val calculate_votes : int -> int -> int64 option

val process_survey : int -> int -> int64 option

(* get from database *)
(**[get_user user_id] returns a user given [user_id*)
val id_by_usr : string -> int

val get_user : int -> user

val get_group : int -> group

val get_restriction_by_id : int -> string

val get_restrictions : unit -> string list

val create_tables : unit -> unit