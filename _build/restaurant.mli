open Yojson.Basic.Util

(** The type representation for a restaurant. Regardless of implementation
    the type should include data regarding restaurant name, location (maybe the
    address??), type of food, allergies, and the average rating, 
    the price rating, and the wait time.*)
type t

(** Returns the data for the restaurant taken as a json as type t to be used 
    with our built functionality for the restaurant module. *)
val from_json : Yojson.Basic.t -> t 

(** [to_json restaurant] is the string representing the restaurant*)
val to_json : t -> string

val get_name : t -> string 

val get_x : t -> float 

val get_y : t -> float 

val get_cuisines : t -> string

val get_allergies : t -> string list

val get_rating : t -> float 

val get_price : t -> int

(** [create name loc_x loc_y cuisine_type rating allergens price wait_time]
returns a restaurant given its attributes*)
val create : string -> float -> float -> string -> float -> string list -> 
int -> int -> t
