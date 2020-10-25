(**The state of the app at any given point, containing information about all users, groups, and restaurants*)

type t

(**The state of the app at any given point, containing information about
   all users, groups, and restaurants*)

val file_name : string ref

val load : string -> t 

val get_restaurant : t -> string -> float -> float -> int option

val get_group : t -> string -> string -> int option

val get_user : t -> string -> int option

val save : t -> unit 

(**Instantiate a person given a username, password, and name./*)
val add_user : t -> string -> string -> string -> unit 

(**Instantiate a person given a username, password, and name./*)
val add_restaurant : t -> string -> float -> float -> string -> float -> 
  string list -> int -> int -> unit 

(**Instantiate a group given a username and hostname./*)
val add_group : t -> string -> string -> unit 

val make_friends : t -> int -> int -> unit

(**Returns group list user about to be added *)
val join_group : t -> int -> int -> unit

