type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  groups : int list 
}

type friends = {
  friend1 : int;
  friend2 : int;
}

type restrictions  = {
  user_id : int;
  restriction : string;
}

type groups = {
  id : int;
  host_id : int;
  member_id : int;
}

(*Insertion functions for the database*)

(**[add_user username password_hash name] registers a user in the database.*)
val add_user : string -> string -> string -> int64 option

(**[add_friends friend1 friend2 inserts a pairing of two friends]
Requires: friend1 is not friend2
Raises: Invalid_arg*)
val add_friends : int -> int -> int64 option

(**[add_restrictions user_id restriction_id] performs a restriction insertion
Requires: restriction_id and user_id are both valid*)
val add_restrictions : int -> int -> int64 option

(**[add_restrictions_index restriction_name]*)
val add_restrictions_index : string -> int64 option

(**[add_group_info group_name host_id] *)
val add_group_info : string -> int -> int64 option

(**[add_groups user_id group_id] *)
val add_groups : int -> int -> int64 option

(*Get functions for the database*)

(**[get_user user_id] returns a user given [user_id*)
val get_user : int -> user

val create_tables : unit -> unit