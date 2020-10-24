open Yojson.Basic.Util

type t = {
  id : int;
  username : string;
  groups : int list;
  new_group : int list;
  session : string list;
  restaurant : string list;
  friends : string list;
}

let add_user new_user t = 
  t.username <- new_user.id :: t.username

let add_restaurant new_restaurant t = 
  t.restaurant <- new_restaurant :: t.restaurant

let add_groupt new_group t = 
  t.groups <- new_group :: t.groups

let make_friends new_friends t = 
  t.friends <- new_friends :: t.friends

let join_group new_group t = 
  t.new_group <- new_group ::  t.groups 



