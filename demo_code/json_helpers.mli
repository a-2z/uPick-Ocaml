(** Returns a string representing [lst] in a JSON format.
    Note: the return is not a valid JSON itself.*)
val json_string_lst : string list -> string


val json_dict_lst : string list -> string

(** Note: the return is not a valid JSON itself *)
val json_int_lst : int list -> string 

(** Similar helper function but for a int list option, not sure if this can
    be combined in any way with function above to save code since functions are 
    two different types. *)
val json_int_lst_opt : int list option -> string