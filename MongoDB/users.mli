type t

val person_to_bson : t -> 'a

val bson_to_person : 'a -> t