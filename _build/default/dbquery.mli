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
type friends
type restrictions
type groups

val add_user : string -> string -> string -> unit

val add_friends : int -> int -> unit

val add_restrictions : int -> string -> unit

val add_group_info : string -> int -> unit

val add_groups : int -> int -> unit

val create_tables : unit -> unit

val get_user : int -> user

(* val get_test : string -> unit *)

(* create get and use serialize functions *)
