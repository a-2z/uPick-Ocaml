(**[get_rests num cuisine loc_x Loc_y range price] returns a list of [num] 
   restaurants as a string of a json. The data in the jsons can be seen in 
   [to_rest json]

   Requires: [cuisine] must be a list of strings that represent Zomato 
   cuisine IDs*)
val get_rests : ?num : int -> ?cuisine : string list -> float -> float -> 
  int -> int -> string Lwt.t
