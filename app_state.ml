open Yojson.Basic.Util

type t = {
  id : int;
  username : string;
  groups : int list;
  new_group : int list;
  session : string list
}

let add_user t = 
  t.id

let old_group t =
  t.groups

let new_group id new_group = 
  fun new_group -> {id :: t.new_group}


