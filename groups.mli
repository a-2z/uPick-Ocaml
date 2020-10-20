open Yojson.Basic.Util
(** A type representation containing all information that would be 
    relevant for a group. This would include a list of users, list of available
    restaurants, if voting has taken place / is in progress, and for each 
    user if they have voted*)
type t

(** The following are all get functions for data stored in type t, be sure to 
    implement them consistently based on the implementation of t *)
val get_users : t -> string list

val get_restaurants : t -> string list

(** Returns if the voting process has started in the group. *)
val voting_in_progress : t -> bool

(** Returns if a specific user has voted (helper or stays in interface?) *)
val has_user_voted : t -> string -> bool

(** Returns a list of every user with if they have or have not voted. 
    Could change output to 'a if a different data type is preferred *)
val has_group_voted : t -> (string * bool) list

