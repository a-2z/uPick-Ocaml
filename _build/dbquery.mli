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

val add_user : string -> string -> string -> int64 option

val add_friends : int -> int -> int64 option

val add_restrictions : int -> int -> int64 option

val add_restriction_index : string -> int64 option

val add_group_info : string -> int -> unit

val add_groups : int -> int -> unit

val create_tables : unit -> unit

val get_user : int -> user

(* val get_test : string -> unit

val get_group :

val get_restaurant :

val get_friends :

val get_restrictions :  *)



(* create get and use serialize functions *)
