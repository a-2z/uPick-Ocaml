open Sqlite3

let db = db_open "upick.db"

type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  visited : int list; 
  groups : int list
}

type friends = {
  friend1 : int;
  friend2 : int;
}

type restrictions  = {
  user_id : int;
  restriction : string;
}

type groups = {
  id : int;
  host_id : int;
  member_id : int;
}

(*
let serialize_user 
user_id username password name friends restrictions visited groups =
  {
    id = user_id;
    username = username;
    password = password;
    name = name;
    friends = friends;
    restrictions = restrictions;
    visited = visited;
    groups = groups;
  }
 
let serialize_friends id_1 id_2 = 
  {
    friend1 = id_1;
    friend2 = id_2;
  }

let serialize_restrictions id restrict = 
  {
    user_id = id;
    restriction = restrict;
  }

let serialize_groups group_id host_id member_id = 
  {
    id = group_id;
    host_id = host_id;
    member_id = member_id;
  } *)

let add_user username password name =
  let sql =
    Printf.sprintf "INSERT INTO users VALUES('%s','%s','%s')"
      username password name in
  match exec db sql with 
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let add_friends friend1 friend2 = 
  let sql =
    Printf.sprintf "INSERT INTO friends VALUES(%d, %d)"
      friend1 friend2 in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restrictions VALUES(%d,'%s')"
      user_id restriction in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf "INSERT INTO groupsInfo VALUES('%s', %d)"
      group_name host_id in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let add_groups group_id member_id = 
  let sql =
    Printf.sprintf "INSERT INTO groups VALUES(%d, %d)"
      group_id member_id in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let get_user user_id = 
(*query for user with user_id, returns type user *)
{
    id = user_id;
    username = "asdas";
    password = "password";
    name = "name";
    friends = [1];
    restrictions = [];
    visited = [];
    groups = [];
  }

(* let get_user_username user = user.name

let get_user_password user = user.password

let get_user_name user = user.name

let get_friends_id1 friends = friends.friend1

let get_friends_id2 friends = friends.friend2

let get_restrictions_userid restrictions = restrictions.user_id

let get_restrictions_restriction restrictions = restrictions.restriction

let get_groups_id groups = groups.id

let get_groups_hostid groups = groups.host_id

let get_groups_memberid groups = groups.member_id*)

let create_tables () = Db.create_tables ()

let get_test field = 
  let sql = Printf.sprintf "SELECT ('%s') FROM users" field in
  match exec db sql with
  | Rc.OK -> ()
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db) 
  
(* match row.() with 
| Some a ->
if x = ele then begin
let () = print_endline "Creating the table with new elements" in 
create_tables()
| None -> () 
end *)