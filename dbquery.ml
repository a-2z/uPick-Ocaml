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

(**[make_response] should match with [exec db sql]*)
let make_response = function 
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

let add_user username password name =
  let sql =
    Printf.sprintf "INSERT INTO users VALUES('%s','%s','%s')"
      username password name in
  make_response (exec db sql) 

(**raises assertion_error exception *)
let add_friends friend1 friend2 = 
  assert (friend1 <> friend2);
  let sql =
    Printf.sprintf "INSERT INTO friends VALUES(%d, %d)"
      friend1 friend2 in
  make_response (exec db sql) 

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

let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf "INSERT INTO groupsInfo VALUES('%s', %d)"
      group_name host_id in
  make_response (exec db sql) 

let add_groups group_id member_id = 
  let sql =
    Printf.sprintf "INSERT INTO groups VALUES(%d, %d)"
      group_id member_id in
  make_response (exec db sql) 

let create_tables () = Db.create_tables ()

(**Helper to prepare a statement for stepping*)
let make_stmt (sql : string) = prepare db sql 

(**[lst_from_query sql_col sql_tbl sql_where f] is a unique list of values taken 
   from column [sql_col] in [sql_tbl] satisfying predicate [sql_where], with 
   values created using [f] from their string representation.

   Only the first column of the query is used, so sql_col should specify only
   one column.

   Requires: all sql_ arguments are defined in the schema. *)
let lst_from_query 
    (sql_col : string) 
    (sql_tbl : string) 
    (sql_where : string)
    (f : string -> 'a) = 
  (*arr accumulates the values in the column*)
  let arr = ref [||] in
  let stmnt = make_stmt (Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s
  |} sql_col sql_tbl sql_where) in 
  (*Each step corresponds to an iteration over a row*)
  while (step stmnt) = ROW do 
    let f1 = (row_data stmnt).(0)
             |> Data.to_string_coerce 
             |> f in
    arr := Array.append !arr [|f1|]
  done;
  List.sort_uniq compare (Array.to_list !arr)

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
  let friends1 = lst_from_query "friend_2" "friends" 
      ("friend_1 = " ^ string_of_int userid) int_of_string in 
  let friends2 = lst_from_query "friend_1" "friends" 
      ("friend_2 = " ^ string_of_int userid) int_of_string in 
  let friends = List.sort_uniq compare (friends1 @ friends2) in
  let restrictions = lst_from_query "restriction" "restrictions" 
      ("user_id = " ^ string_of_int userid) int_of_string in
  let groups = lst_from_query "group_id" "groups" 
      ("member_id = " ^ string_of_int userid) int_of_string in
  {
    partial with 
    friends = friends;
    restrictions = restrictions;
    groups = groups
  }