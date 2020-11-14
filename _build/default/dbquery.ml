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

type group = {
  id : int;
  name : string;
  host_id : int;
  members : int list;
}

type restriction = {
  id : int;
  name : string;
}

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

(**[single_row_query sql_col sql_tbl sql_where] is an array of strings 
   representing the columns [sql_select] in a single row matching [sql_where]*)
let single_row_query
    (sql_select : string) 
    (sql_tbl : string) 
    (sql_where : string) = 
  let sql = Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s;
  |} sql_select sql_tbl sql_where in
  let stmnt = 
    make_stmt sql in 
  ignore (step stmnt);
  Array.map (fun x -> print_endline (Data.to_string_coerce x); Data.to_string_coerce x) (row_data stmnt)

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

let login username = 
  try
    Some (single_row_query "password" "users" 
            ("username = '" ^ username ^ "'")).(0)
  with e -> ignore(e); None

(** [get_user userid] returns a representation of a single user from the 
    database in type user.  
    Requires: A valid userid is inputted, valid [username], [password] inputted, 
    valid [name] inputted, valid [friends] inputted, [restricitons] inputted,
    [groups] inputted definined in the same user *)
let get_user userid = 
  let arr1 = single_row_query "username, password, name" "users" 
      ("rowid = " ^ string_of_int userid) in
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
    id = userid;
    username = arr1.(0);
    password = arr1.(1);
    name = arr1.(2); 
    friends = friends;
    restrictions = restrictions;
    groups = groups;
  }

let get_group groupid = 
  let arr1 = single_row_query 
      "group_name, host_id" "group_info" 
      ("rowid = " ^ string_of_int groupid) in 
  let mem_lst = lst_from_col 
      "member_id" "groups" 
      ("group_id = " ^ string_of_int groupid) int_of_string in 
  {
    id = groupid;
    name = arr1.(0);
    host_id = arr1.(1) |> int_of_string; 
    members = mem_lst
  }

let get_restrictions () = 
  lst_from_col "restriction" "restriction_index" "1 = 1" (fun x -> x)

let get_restriction_by_id rest_id = 
  let rest = single_row_query 
      "restriction" "restriction_index" 
      ("rowid = " ^ string_of_int rest_id) in
  rest.(0)

let create_tables () = Db.create_tables ()