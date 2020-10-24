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

let add_restaurant t = 
  failwith "Unimplemented"

let add_groupt t = 
  failwith "Unimplemented"

let make_friends t = 
  failwith "Unimplemented"

let join_group t = 
  failwith "Unimplemented"




