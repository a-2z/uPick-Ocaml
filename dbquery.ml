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
  restrictions_id : int;
  restriction : string;
}

type groups = {
  host_id : int;
  member_id : int;
}

let create_user user_id username password name =
{
  id = user_id;
  username = username;
  password = password;
  name = name;
}

let create_friends id_1 id_2 = 
{
  friend1 = id_1;
  friend2 = id_2;
}

let create_restrictions id rstrct = 
{
    restrictions_id = id;
    restriction = rstrct;
}

let create_groups host_id member_id = 
{
  host_id = host_id;
  member_id = member_id;
}

let add_user_data username password name =
      let sql =
        Printf.sprintf "INSERT INTO Users VALUES('%s','%s','%s')"
          username password name in
      match exec db sql with 
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)
      
let add_friends_data friends = 
  match friends with
    | {friend1; friend2} ->
      let sql =
        Printf.sprintf "INSERT INTO Friends VALUES(%d, %d)"
          friend1 friend2 in
      match exec db sql with
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)


let add_restrictions_data restrictions = 
  match restrictions with
    | {restrictions_id; restriction} ->
      let sql =
        Printf.sprintf "INSERT INTO Restrictions VALUES(%d,'%s')"
          restrictions_id restriction in
      match exec db sql with
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)


let add_groups_data groups = 
  match groups with
    | {host_id; member_id} ->
      let sql =
        Printf.sprintf "INSERT INTO Groups VALUES(%d, %d)"
          host_id member_id in
        match exec db sql with
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)
