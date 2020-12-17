open Sqlite3

let db = db_open "upick.db"

let error error message =
  let () = prerr_endline (Rc.to_string error) in
  let () = prerr_endline (errmsg db) in
  let () = prerr_endline message in
  let _closed = db_close db in
  let () = prerr_endline "Exiting..." in
  exit 1

let create_users_table () =
  let create_usertable =  {|
  CREATE TABLE IF NOT EXISTS users (  
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
  CREATE TABLE IF NOT EXISTS friends ( 
    friend_1 INTEGER NOT NULL, 
    friend_2 INTEGER NOT NULL, 
    PRIMARY KEY(friend_1, friend_2),
    FOREIGN KEY(friend_1) REFERENCES users(rowid)
    ON DELETE SET NULL, 
    FOREIGN KEY(friend_2) REFERENCES users(rowid)
    ON DELETE SET NULL);
  |}
  in match exec db create_friends with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table friends." in
    error r message

let create_restrictions_table () =
  let create_restrictions = {|
  CREATE TABLE IF NOT EXISTS restrictions ( 
    user_id INT NOT NULL, 
    restriction INT NOT NULL, 
    PRIMARY KEY(user_id, restriction),
    FOREIGN KEY(restriction) REFERENCES restriction_index(rowid)
    FOREIGN KEY(user_id) REFERENCES users(rowid)
    ON DELETE SET NULL);
    |}
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table restrictions." in
    error r message

let create_restriction_index () =
  let create_restrictions = {|
  CREATE TABLE IF NOT EXISTS restriction_index ( 
    restriction TEXT UNIQUE NOT NULL); 
    |}
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table restriction." in
    error r message

let create_groups_info_table () =
  let create_groups_info_table = {|
  CREATE TABLE IF NOT EXISTS group_info (
    group_name TEXT NOT NULL,
    host_id INTEGER NOT NULL,
    num_members INTEGER DEFAULT 0,
    voting_allowed INTEGER DEFAULT 0,
    top_5 TEXT, --JSON
    top_pick TEXT, --JSON
    PRIMARY KEY(group_name, host_id),
    FOREIGN KEY(host_id) REFERENCES users(rowid)
          ON DELETE SET NULL, 
    FOREIGN KEY(host_id) REFERENCES users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_info_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table group_info." in
    error r message

let create_groups_table () =
  let create_groups_table = {|
  CREATE TABLE IF NOT EXISTS groups ( 
    group_id INTEGER,  
    member_id INTEGER NOT NULL, 
    loc_x FLOAT,
    loc_y FLOAT,
    target_price INT,
    cuisines TEXT,
    range INT,
    surveyed INTEGER DEFAULT 0,
    voted INTEGER DEFAULT 0,
    PRIMARY KEY(group_id, member_id),
    FOREIGN KEY(group_id) REFERENCES group_info(rowid)
          ON DELETE SET NULL
    FOREIGN KEY(member_id) REFERENCES users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table groups." in
    error r message

let create_votes_table () = 
  let create_vote_table = {|
    CREATE TABLE IF NOT EXISTS votes ( 
    group_id INTEGER NOT NULL,  
    user_id INTEGER NOT NULL, 
    ranking INTEGER NOT NULL,
    restaurant_id INTEGER NOT NULL,
    FOREIGN KEY(group_id) REFERENCES group_info(rowid)
          ON DELETE SET NULL
    FOREIGN KEY(user_id) REFERENCES groups(member_id)
          ON DELETE SET NULL);
    |}
  in match exec db create_vote_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table votes." in
    error r message

let create_tables _ = 
  () 
  |> create_users_table
  |> create_friends_table
  |> create_restrictions_table
  |> create_restriction_index
  |> create_groups_info_table
  |> create_groups_table
  |> create_votes_table 