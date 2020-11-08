open Sqlite3

let db = db_open "test.db"

let gracefully_exist error message =
  let () = prerr_endline (Rc.to_string error) in
  let () = prerr_endline (errmsg db) in
  let () = prerr_endline message in
  let _closed = db_close db in
  let () = prerr_endline "Exiting ..." in
  exit 1

(* creates user table with id, username, password, and name *)
let create_users_table () =
  let create_usertable = "CREATE TABLE IF NOT EXISTS Users ( \
                          id INTEGER PRIMARY KEY, \
                          username TEXT NOT NULL, \
                          password TEXT NOT NULL, \
                          name TEXT NOT NULL, \
                          );"
  in match exec db create_usertable with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table users." in
    gracefully_exist r message

(* creates friends table by using two friend ids, and references user id for 
foreign key*)
let create_friends_table () =
  let create_friends = " CREATE TABLE IF NOT EXISTS Friends ( \
                                friend_1 INTEGER NOT NULL, \
                                friend_2 INTEGER NOT NULL, \
                                FOREIGN KEY(friend_1) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                                FOREIGN KEY(friend_2) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                                );"
  in match exec db create_friends with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table friends." in
    gracefully_exist r message

(* makes the id of the usertable a foreign key that is liked to the id's in the
friends table *)
(* let link_friends_users () =
  let link = "ALTER TABLE Users
              ADD FOREIGN KEY(id)
              REFERENCES Friends(friend_1) AND REFERENCES Friends(friend_2) 
              ON DELETE SET NULL;"
  in match exec db link with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to link tables." in
    gracefully_exist r message *)
 
(* creates restriction table with an id and restriction and links the id   *)
let create_restrictions_table () =
  let create_restrictions = " CREATE TABLE IF NOT EXISTS Restrictions ( \
                                restriction_id INTEGER NOT NULL, \
                                restrictions TEXT NOT NULL, \
                                FOREIGN KEY(restriction_id) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                                );"
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table restrictinos." in
    gracefully_exist r message

(* makes the id of username of user table be a foreign key that is linked to 
restrictions table*)
(* let link_restrictions_users () =
  let link = "ALTER TABLE Users
              ADD FOREIGN KEY(id)
              REFERENCES Restrictions(id) AND REFERENCES Users(id) 
              ON DELETE SET NULL;"
  in match exec db link with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to link tables." in
    gracefully_exist r message *)

  let create_visited_table () =
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
    gracefully_exist r message

(* makes the id of the usertable a foreign key that is liked to the id's in the
visited table *)
(* let link_visited_users () =
  let link = "ALTER TABLE Users
              ADD FOREIGN KEY(id)
              REFERENCES Visited(id) AND REFERENCES Users(id) 
              ON DELETE SET NULL;"
  in match exec db link with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to link visited." in
    gracefully_exist r message *)

let create_groups_table () =
  let create_groups_table = "CREATE TABLE IF NOT EXISTS Groups ( \
                          group_id INTEGER PRIMARY KEY, \
                          host_id INTEGER NOT NULL, \
                          friend_id INTEGER NOT NULL, \
                          FOREIGN KEY(host_id) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                          FOREIGN KEY(friend_id) REFERENCES Users(id)
                                ON DELETE SET NULL, \
                          );"
  in match exec db create_groups_table with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table groups." in
    gracefully_exist r message

(* makes the id of the usertable a foreign key that is liked to the id's in the
groups table *)
(* let link_groups_users () =
  let link = "ALTER TABLE Users
              ADD FOREIGN KEY(id)
              REFERENCES Groups(host_id) AND REFERENCES Groups(friend_id) 
              ON DELETE SET NULL;"
  in match exec db link with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to link tables group." in
    gracefully_exist r message *)


(* Query that should returns a result if a table contacts exists *)
let check_table_sql =
  "SELECT COUNT(name) FROM sqlite_master WHERE type='table' AND name='users'"

(* Callback that checks if there is already a table users and if not creates
   it *)
let check_table_cb row = match row.(0) with
  | Some a ->
    if a = "0" then begin
      let () = print_endline "Creating the table Users" in
      create_users_table ()
    end else print_endline "The table Users already exists"
  | None -> ()

let ensure_table_users_exists () =
  match exec_no_headers db ~cb:check_table_cb check_table_sql with
  | Rc.OK -> ()
  | r ->
    let message =  "Unable to check if the table Users exists." in
    gracefully_exist r message

(* data to add to user table 
let user_data = [
  (1, "reetuparikh", "reetu123", "Reetu");
  (2, "andrewosorio", "andrew123", "Andrew");
] *)

(* script to add stuff 
let () =
  let () = ensure_table_users_exists () in 
  add_data user_data *)