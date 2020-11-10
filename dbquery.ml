open Sqlite3

let db = db_open "test.db"

type user = {
  id : int;
  username : string;
  password : string;
  name : string;
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

let serialize_user user_id username password name =
{
  id = user_id;
  username = username;
  password = password;
  name = name;
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
}

let add_user username password name =
      let sql =
        Printf.sprintf "INSERT INTO Users VALUES('%s','%s','%s')"
          username password name in
      match exec db sql with 
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)
      
let add_friends friend1 friend2 = 
      let sql =
        Printf.sprintf "INSERT INTO Friends VALUES(%d, %d)"
          friend1 friend2 in
      match exec db sql with
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)


let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO Restrictions VALUES(%d,'%s')"
      user_id restriction in
  match exec db sql with
    | Rc.OK ->
      let id = Sqlite3.last_insert_rowid db in
      Printf.printf "Row inserted with id %Ld\n" id
    | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)


let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf "INSERT INTO GroupsInfo VALUES('%s', %d)"
      group_name host_id in
    match exec db sql with
    | Rc.OK ->
      let id = Sqlite3.last_insert_rowid db in
      Printf.printf "Row inserted with id %Ld\n" id
    | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)

let add_groups group_id member_id = 
  let sql =
    Printf.sprintf "INSERT INTO Groups VALUES(%d, %d)"
      group_id member_id in
    match exec db sql with
    | Rc.OK ->
      let id = Sqlite3.last_insert_rowid db in
      Printf.printf "Row inserted with id %Ld\n" id
    | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)