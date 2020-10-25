open Yojson.Basic.Util
(** A type representation containing all information that would be 
    relevant for a group. This would include a list of users, list of available
    restaurants, if voting has taken place / is in progress, and for each 
    user if they have voted*)

exception NoVotes

exception NoGroup

type t



val from_json : Yojson.Basic.t -> t

val to_json : t -> string

(** The following are all get functions for data stored in type t, be sure to 
    implement them consistently based on the implementation of t *)

val get_id : t -> int

val get_name : t -> string

val get_host : t -> string

val get_users : t -> int list

val get_candidates : t -> int list option

val get_winner : t -> int option

(** Returns if the voting process has started in the group. *)
val surveys_done : t -> bool

(** Returns a list of every user with if they have or have not voted. 
    Could change output to 'a if a different data type is preferred *)
val voting_done : t -> bool

val add_user : t -> int -> unit 

val create : int -> string -> string -> t
