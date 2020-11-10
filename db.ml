open Sqlite3
open Dbquery

let db = db_open "test.db"

let gracefully_exist error message =
  let () = prerr_endline (Rc.to_string error) in
  let () = prerr_endline (errmsg db) in
  let () = prerr_endline message in
  let _closed = db_close db in
  let () = prerr_endline "Exiting ..." in
  exit 1

let create_users_table () =
  let create_usertable =  {|
  CREATE TABLE IF NOT EXISTS Users (  
  username TEXT PRIMARY KEY NOT NULL, 
  password TEXT NOT NULL, 
  name TEXT NOT NULL);
  |}
  in match exec db create_usertable with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table users." in
    gracefully_exist r message

let create_friends_table () =
  let create_friends = {|
  CREATE TABLE IF NOT EXISTS Friends ( 
    friend_1 INTEGER NOT NULL, 
    friend_2 INTEGER NOT NULL, 
    FOREIGN KEY(friend_1) REFERENCES Users(rowid)
    ON DELETE SET NULL, 
    FOREIGN KEY(friend_2) REFERENCES Users(rowid)
    ON DELETE SET NULL);
  |}
  in match exec db create_friends with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table friends." in
    gracefully_exist r message
 
let create_restrictions_table () =
  let create_restrictions = {|
  CREATE TABLE IF NOT EXISTS Restrictions ( 
    user_id INTEGER NOT NULL, 
    restrictions TEXT NOT NULL, 
    FOREIGN KEY(restriction_id) REFERENCES Users(rowid)
    ON DELETE SET NULL);
    |}
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table restrictions." in
    gracefully_exist r message

let create_groups_table () =
  let create_groups_table = {|
  CREATE TABLE IF NOT EXISTS Groups ( 
    group_id INTEGER PRIMARY KEY, 
    host_id INTEGER NOT NULL, 
    friend_id INTEGER NOT NULL, 
    FOREIGN KEY(host_id) REFERENCES Users(rowid)
          ON DELETE SET NULL, 
    FOREIGN KEY(friend_id) REFERENCES Users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table groups." in
    gracefully_exist r message

let create_tables () = 
  () 
  |> create_friends_table
  |> create_groups_table
  |> create_restrictions_table
  |> create_users_table


  (* let create_visited_table () =
    let create_visited = " CREATE TABLE IF NOT EXISTS Visited ( \
                                visited_id INTEGER NOT NULL, \
                                visited INTEGER NOT NULL, \
                                FOREIGN KEY(visited_id) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                                );"
  in match exec db create_visited with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table visited." in
    gracefully_exist r message *)
