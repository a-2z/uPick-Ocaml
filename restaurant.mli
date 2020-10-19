(*open Yojson.Basic.Util*)

(** The type representation for a restaurant. Regardless of implementation
    the type should include data regarding restaurant name, location (maybe the
    address??), type of food, allergies, and the average rating, 
    the price rating, and the wait time.*)
type t

(** The following are all get functions for data stored in type t, be sure to 
    implement them consistently based on the implementation of t *)
val get_name : t -> string 

val get_location : t -> string 

val get_food_type : t -> string

val get_allergies : t -> string list

val get_rating : t -> float 

val get_price : t -> float 
(*could be done in many ways, string, float, variant, etc.*)

val get_wait_time : t -> int


(** Returns the data for the restaurant taken as a json as type t to be used 
    with our built functionality for the restaurant module. *)
val from_json : 'a -> t (* Consider using Yojson.Basic.t as 'a *)

(** Returns a new wait time as time passes or incase there is a sudden 
    change in business at the restaurant, extra comment for push check - zach*)
val change_wait_time : int -> t