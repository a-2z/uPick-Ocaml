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

let add_user username password name =
  let sql =
    Printf.sprintf "INSERT INTO users VALUES('%s','%s','%s')"
      username password name in
  match exec db sql with 
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

let add_friends friend1 friend2 = 
  let sql =
    Printf.sprintf "INSERT INTO friends VALUES(%d, %d)"
      friend1 friend2 in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restrictions VALUES(%d, %d)"
      user_id restriction in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

let add_restrictions_index restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restriction_index VALUES('%s')"
      restriction in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

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

let create_tables () = Db.create_tables ()

let make_stmt sql = prepare db sql 
(*
 {
    id = Sqlite3.column stmnt 0 |> to_int_exn
  }
 *)

(**[get_int_lst selection table cond] is an int list of values
from the column indicated by selection that satisfy predicate [cond] in 
[table]

Requires: selection contains only one column*)
let get_int_lst selection table cond = 
let arr = ref [||] in
  let stmnt = 
  make_stmt (Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s;
  |} selection table cond) in 
  while (step stmnt) = ROW do 
   let f1 = (row_data stmnt).(0)
   |> Data.to_string_coerce 
   |> int_of_string in
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
  let friend_arr = ref [||] in
  let friendstmt = 
  make_stmt (Printf.sprintf {|
    SELECT friend_1, friend_2 
    FROM friends 
    WHERE friend_1 = %d OR friend_2 = %d;
  |} userid userid) in 
  while (step friendstmt) = ROW do 
    let f1 = 
    (row_data friendstmt).(0) |> Data.to_string_coerce |> int_of_string in
    let f2 = 
    (row_data friendstmt).(1) |> Data.to_string_coerce |> int_of_string in
    match f1 with
      | x -> if x = userid then friend_arr := Array.append !friend_arr [|f2|] 
      else friend_arr := Array.append !friend_arr [|f1|]
  done; 
  let restrictions = get_int_lst "restriction" "restrictions" 
  ("user_id = " ^ string_of_int userid) in
  let groups = get_int_lst "group_id" "groups" 
  ("member_id = " ^ string_of_int userid) in
  {
    partial with 
  friends = List.sort_uniq compare (Array.to_list !friend_arr);
  restrictions = restrictions;
  groups = groups
  }
      
(* creating query *)
(* let sql = "SELECT user FROM sqlite_master WHERE type = 'table'; "
    (* execute query *)
    match exec db ~cb show_default_tables with
    | Rc.OK -> ()
    | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db) *)

(* display result of query *)
(* let cb row headers =
  let n = Array.length row - 1 in
  let () = for i = 0 to n do
      let value = match row.(i) with | Some s -> s | None -> "Null" in
      Printf.printf "| %s: %s |" headers.(i) value
    done
  in print_endline "" *)
  
(* let get_user_query = "SELECT username FROM users" *)


(* https://github.com/cedlemo/ocaml-sqlite3-notes/blob/master/README_sqlite3_tutorial.md#sqlite-simple-query *)