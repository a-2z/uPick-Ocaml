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

(*Insertion functions *)
(**Register the user with a [username], hashed [password], and [name].

  Returns: Some [last_inserted_id] or None if the insertion failed. 
  Raises: *)
val add_user : username:string -> password:string -> name:string -> int64 option

val add_friends : user2_id:int -> user1_id:int -> int64 option

val add_restrictions : user_id:int -> restriction_id:int -> int64 option

val add_restrictions_index : rest_name:string -> int64 option

val add_group_info : group_name:string -> host:int -> int64 option

val add_groups : group_id:int -> member_id:int -> int64 option

val create_tables : unit -> unit

val get_user : int -> user

(* val get_test : string -> unit

val get_group :

val get_restaurant :

val get_friends :

val get_restrictions :  *)



(* create get and use serialize functions *)
