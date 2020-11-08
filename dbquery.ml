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
}

type groups = {
  visited_id : int
}


let create_user username password name =
{
  id = 0;
  username = username;
  password = password;
  name = name;
  
}


let add_user_data user =
  match user with
    | {id; username; password; name} ->
      let sql =
        Printf.sprintf "INSERT INTO Users VALUES(%d,'%s','%s','%s')"
          id username password name
      in
      let () = begin match exec db sql with
        | Rc.OK ->
          let id = Sqlite3.last_insert_rowid db in
          Printf.printf "Row inserted with id %Ld\n" id
        | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db)
      end