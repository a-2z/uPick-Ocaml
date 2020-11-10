type user
type friends
type restrictions
type groups

val create_user : int -> string -> string -> string -> user

val create_friends : int -> int -> friends

val create_restrictions : int -> string -> restrictions

val create_groups : int -> int -> groups

val add_user_data : string -> string -> string -> unit

val add_friends_data : friends -> unit

val add_restrictions_data : restrictions -> unit

val add_groups_data : groups -> unit

 