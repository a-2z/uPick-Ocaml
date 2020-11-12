type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  visited : int list; 
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

val add_user : string -> string -> string -> unit

val add_friends : int -> int -> unit

val add_restrictions : int -> string -> unit

val add_group_info : string -> int -> unit

val add_groups : int -> int -> unit

val create_tables : unit -> unit

val get_user : int -> user

val get_test : string -> unit

(* create get and use serialize functions *)
