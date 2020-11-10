type user
type friends
type restrictions
type groups

val add_user : string -> string -> string -> unit

val add_friends : friends -> unit

val add_restrictions : int -> string -> unit

val add_groups : groups -> unit

(* create get and use serialize functions *)

 