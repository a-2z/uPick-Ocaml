open Sqlite3
open Dbquery

let db = db_open "test.db"

let error error message =
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
    error r message

let create_friends_table () =
  let create_friends = {|
  CREATE TABLE IF NOT EXISTS Friends ( 
    friend_1 INTEGER NOT NULL, 
    friend_2 INTEGER NOT NULL, 
    PRIMARY KEY(friend_1, friend_2),
    FOREIGN KEY(friend_1) REFERENCES Users(rowid)
    ON DELETE SET NULL, 
    FOREIGN KEY(friend_2) REFERENCES Users(rowid)
    ON DELETE SET NULL);
  |}
  in match exec db create_friends with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table friends." in
    error r message
 
let create_restrictions_table () =
  let create_restrictions = {|
  CREATE TABLE IF NOT EXISTS Restrictions ( 
    user_id INTEGER KEY NOT NULL, 
    restriction TEXT NOT NULL, 
    PRIMARY KEY(user_id, restriction),
    FOREIGN KEY(user_id) REFERENCES Users(rowid)
    ON DELETE SET NULL);
    |}
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table restrictions." in
    error r message

let create_groups_info_table () =
  let create_groups_info_table = {|
  CREATE TABLE IF NOT EXISTS GroupsInfo ( 
    group_name TEXT NOT NULL,
    host_id INTEGER NOT NULL,
    PRIMARY KEY(group_name, host_id),
    FOREIGN KEY(host_id) REFERENCES Users(rowid)
          ON DELETE SET NULL, 
    FOREIGN KEY(host_id) REFERENCES Users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_info_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table groups_info." in
    error r message

let create_groups_table () =
  let create_groups_table = {|
  CREATE TABLE IF NOT EXISTS Groups ( 
    group_id INTEGER PRIMARY KEY,  
    friend_id INTEGER NOT NULL, 
    FOREIGN KEY(group_id) REFERENCES GroupsInfo(rowid)
          ON DELETE SET NULL
    FOREIGN KEY(friend_id) REFERENCES Users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table groups." in
    error r message

let create_tables () = 
  () 
  |> create_users_table
  |> create_friends_table
  |> create_restrictions_table
  |> create_groups_info_table
  |> create_groups_table


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
    error r message *)
