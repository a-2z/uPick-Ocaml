(**The state of the app at any given point, containing information about all users, groups, and restaurants*)

type t

type group_id
type res_id
type user_id


(**The state of the app at any given point, containing information about
   all users, groups, and restaurants*)

val file_name : string ref

val load : string -> t 

val save : t -> unit 

(**Instantiate a person given a username, password, and name./*)
val add_user : t -> int -> string -> string -> string -> unit 

(**Instantiate a person given a username, password, and name./*)
val add_restaurant : t -> string -> string -> string -> unit 

(**Instantiate a group given a username and hostname./*)
val add_group : t -> string -> string -> string -> unit 

val make_friends : t -> User.t ref -> User.t ref -> unit

(**Returns group list user about to be added *)
val join_group : t -> group_id -> user_id -> unit

