open Sqlite3

let db = db_open "upick.db"

type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
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

(**[make_response] returns [Some last_id] if an insertion operation succeeded
and [None] otherwise.*)
let make_response = function 
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

(*insertion functions *)
let add_user username password name =
  let sql =
    Printf.sprintf "INSERT INTO users VALUES('%s','%s','%s')"
      username password name in
  make_response (exec db sql)

(**[add_friends friend1 friend2 inserts a pairing of two friends]
Requires: friend1 is not friend2
Raises: Invalid_arg*)
let add_friends friend1 friend2 = 
try 
  assert (friend1 <> friend2);
  let sql =
    Printf.sprintf "INSERT INTO friends VALUES(%d, %d)"
      friend1 friend2 in
  make_response (exec db sql)
with e -> 
print_endline (Printexc.to_string e);
print_endline "Cannot friend yourself";
None

let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restrictions VALUES(%d, %d)"
      user_id restriction in
  make_response (exec db sql)

let add_restrictions_index restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restriction_index VALUES('%s')"
      restriction in
  make_response (exec db sql)

let add_groups group_id member_id = 
  let sql =
    Printf.sprintf "INSERT INTO groups VALUES(%d, %d)"
      group_id member_id in
  make_response (exec db sql)

let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf "INSERT INTO group_info VALUES('%s', %d)"
      group_name host_id in
  match exec db sql with
   | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id;
    ignore (add_groups (Int64.to_int id) host_id); Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None



let make_stmt sql = prepare db sql 

(**[lst_from_col sql_col sql_tbl sql_where f] is a list of lists containing 
the values of [sql_col] in [sql_tbl] satisfying [sql_where], converted into 
their primitive types from a string representation with [f]

Returns: a list of lists of values for a query
Requires: [sql_col] contains only one column
[sql_col], [sql_tbl], abd [sql_where] are defined in the schema.
*)
let lst_from_col 
(sql_col : string) 
(sql_tbl : string) 
(sql_where : string) 
(f : string -> 'a) = 
let arr = ref [||] in
  let stmnt = 
  make_stmt (Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s;
  |} sql_col sql_tbl sql_where) in 
  while (step stmnt) = ROW do 
   let value = (row_data stmnt).(0)
   |> Data.to_string_coerce 
   |> f in
   arr := Array.append !arr [|value|]
  done;
  List.sort_uniq compare (Array.to_list !arr)

(** [get_user userid] returns a representation of a single user from the 
database in type user.  
Requires: A valid userid is inputted, valid [username], [password] inputted, 
valid [name] inputted, valid [friends] inputted, [restricitons] inputted,
[groups] inputted definined in the same user *)
let get_user userid = 
  let sql = Printf.sprintf "SELECT * FROM users WHERE rowid = %d" userid in
  let stmnt = make_stmt sql in 
  ignore (step stmnt); 
  let partial = {
    id = userid;
    username = Data.to_string_coerce (row_data stmnt).(0);
    password = Data.to_string_coerce (row_data stmnt).(1);
    name = Data.to_string_coerce (row_data stmnt).(2); 
    friends = [];
    restrictions = [];
    groups = [];
  } in
  let friends1 = lst_from_col "friend_2" "friends" 
  ("friend_1 = " ^ string_of_int userid) int_of_string in
  let friends2 = lst_from_col "friend_1" "friends" 
  ("friend_2 = " ^ string_of_int userid) int_of_string in
  let friends = List.sort_uniq compare (friends1 @ friends2) in
  let restrictions = lst_from_col "restriction" "restrictions" 
  ("user_id = " ^ string_of_int userid) int_of_string in
  let groups = lst_from_col "group_id" "groups" 
  ("member_id = " ^ string_of_int userid) int_of_string in
  {
    partial with 
  friends = friends;
  restrictions = restrictions;
  groups = groups
  }
      
let create_tables () = Db.create_tables ()