(** type user defines a type for each user *)
type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  groups : int list;
  visited : string list
}

(* type group contains all relevant information for a group *)
type group = {
  id : int;
  name : string;
  host_id : int;
  members : int list;
  voting_allowed : bool;
  top_5 : string option;
  top_pick : string option;
}

(* type restriction defines an association between a user and their 
   restriction *)
type restriction = {
  id : int;
  name : string;
}

exception Not_found

(**[add_user username password_hash name] inserts a user in the database.*)
val add_user : string -> string -> string -> int64 option

(**[update_username user_id username] updates the database with the new 
   desired username for the user.
   Requires: user_id exists in the database *)
val update_username : int -> string -> int64 option

(**[update_password user_id password] updates the database with the new 
   desired password for the user.
   Requires: user_id exists in the database *)
val update_password : int -> string -> int64 option

(** [delete_user user_id delete_id] removes the user associated with the 
    delete_id as long as user_id is equivalent to delete_id or user_id 
    associates with a user who is also an admin. 
    Requires: user_id and delete_id exist in the database *)
val delete_user : int -> int -> int64 option

(** [add_friends friend1 friend2] inserts a pairing of two friends into the 
    database based on their ids.
    Requires: friend1 and friend2 exist in the database *)
val add_friends : int -> int -> int64 option

(** [add_restrictions user_id restriction_id] performs a restriction insertion 
    into a table pairing a user and their dietary restriction.
    Requires: restriction_id and user_id exist in the database *)
val add_restrictions : int -> int -> int64 option 

(** [add_restrictions_index user_id restriction] inserts a restriction into a 
    table and associates it with an id if the user is an admin.
    Requires: user_id exists in the database and that the restriciton is a 
    valid highlight in the Zomato API *)
val add_restrictions_index : int -> string -> int64 option 

(** [add_preferences_index user_id preference] inserts a preference into a 
    table if the user is an admin.
    Requires: user_id exists in the database and that preference is a valid 
    highlight in the Zomato API *)
val add_preferences_index : int -> string -> int64 option

(** [remove_restrictions_index user_id restriction_id] removes a restriction
    from the restriction index table associated with the restriction id if the
    user is an admin.
    Requires: user_id and restriction_id exist in the database *)
val remove_restrictions_index : int -> int -> int64 option 

(** [remove_preferences_index user_id preference_id] removes a preference
    from the preferences table associated with it's preference_id if the user 
    is an admin.
    Requires: user_id and preference_id exist in the database *)
val remove_preferences_index : int -> int -> int64 option

(** [add_cuisine user_id cuisine_id cuisine] inserts a type of cuisine and its 
    corresponding Zomato API cuisine id into a table if the user is an admin.
    Requires: user_id exists in the database *)
val add_cuisine : int -> int -> string -> int64 option

(** [remove_cuisine user_id cuisine_id] removes a cuisine from the cuisine 
    table associated with the cuisine id if the user is an admin.
    Requires: user_id and cuisine_id exist in the database *)
val remove_cuisine : int -> int -> int64 option

(** [add_group_info group_name host_id] inserts information about a group 
    including the group name and host id into a table, associating the group
    with an id. 
    Requires: the host_id must be the id of a user that exists in the
    database *)
val add_group_info : string -> int -> int64 option

(** [delete_from_group group_id user_id host_id] deletes a user from a group,
    romoving all data from the group associated with that user.
    Requires: the group_id, user_id (member_id), and host_id, must be ids of a 
    user and group respectively that exists in the database *)
val delete_from_group : int -> int -> int -> int64 option

(** [reassign_host group_id user_id host_id] changes the host of a group from
    the user associated with [host_id] to a different member of the group 
    associated with [user_id].
    Requires: the group_id, user_id (member_id), and host_id, must be ids of a 
    user and group respectively that exists in the database *)
val reassign_host : int -> int -> int -> int64 option

(** [add_groups user_id group_id] associates a group with a user, effectively
    putting a user into a group.
    Requires: the user_id and group_id must be ids of a user and group 
    respectively that exists in the database *)
val join_group : int -> int -> int64 option

(** [delete_group user_id group_id] deletes the group associated with group_id
    if the user associated with user_id is either the host of the group or an 
    admin.
    Requires: the user_id and group_id must be ids of a user and group 
    respectively that exists in the database *)
val delete_group : int -> int -> int64 option

(** [add_group_invites group_id user_id host_id] adds an entry to the 
    group_invites table representing a pending invite from the host user to 
    another user for the group associated with group_id.
    Requires: the user_id, host_id, and group_id must be ids of a user and  
    group respectively that exists in the database *)
val add_group_invites: int -> int -> int -> int64 option

(** [add_votes group_id user_id restaurant_id_lst] adds users votes for a 
    specific group 
    Requires: [user_id] and [group_id] must be ids of a user and group 
    respectively that exists in the database. [restaurant_id_lst] must contain 
    all restaurant ids once in order of preference (front is first priority, 
    last is least priority) stored in the database associated with that group.*)
val add_votes : int -> int -> int list -> int64 option

(** [login username] checks if user is in the database. *)
val login : string -> string option

(** [ans_survey user_id group_id loc_x loc_y cuisine price range preferences] 
    adds survey answers to the user's corresponding information in the groups 
    table 
    Requires: the user_id and group_id must be ids of a user and group 
    respectively that exists in the database  *)
val ans_survey : int -> int -> float -> float -> string -> int -> int -> string
  -> int64 option

(** [calculate_votes g_id h_id] returns the id of the most preferred restaurant
    based on the rankings of users in group associated with [g_id] that 
    voted. 
    Requires: the group_id and host_id must be ids of a group and user 
    respectively that exists in the database *)
val calculate_votes : int -> int -> int64 option

(** [process_survey g_id h_id] uses survey results from users in group
    associated with [g_id] to return a list of restaurants most closely
    matching users preferences. *)
val process_survey : int -> int -> int64 option

(**[id_by_usr usr] is the id of the user with unique username [usr]*)
val id_by_usr : string -> int

(** [get_user userid] is the user with [userid]
    Requires: [userid] is associated with a user that exists in the database. 
    Raises: Not_found*)
val get_user : int -> user

(** [get_group groupid] is the group with [groupid]
    Requires: [groupid] is associated with a group that exists in the 
    database. *)
val get_group : int -> group

(** [get_restrictions] gets a list of all restrictions that exist in the 
    database *)
val get_restrictions : unit -> string list

(** [get_restriction_by_id rest_id] is the restriction with [rest_id]
    Requires: [rest_id] is associated with a restriction that exists in the 
    database. *)
val get_restriction_by_id : int -> string

(** [get_preferences] gets a list of all preferences that exist in the 
    database *)
val get_preferences : unit -> string list 

(** [get_preference_by_id pref_id] is the preference with [pref_id]
    Requires: [pref_id] is associated with a preference that exists in the 
    database. *)
val get_preference_by_id : int -> string

(** [get_cuisines] gets an association list of all cuisine_id's and cuisines 
    that exist in the database *)
val get_cuisines : unit -> int list * string list

(** [get_cuisine_by_id cuisine_id] is the cuisine with [cuisine_id]
    Requires: [cuisine_id] is associated with a cuisine that exists in the 
    database. *)
val get_cuisine_by_id : int -> string

(** [add_feedback rating comments] inserts a users feedback about the 
    application into the database anonymously *)
val add_feedback : float -> string -> int64 option

(** [top_visited] returns a list of the top visited restaurants among users in 
    the database *)
val top_visited : unit -> string list

(** [create_tables] creates all tables if they do not already exist in the 
    database *)
val create_tables : unit -> unit