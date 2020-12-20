open Sqlite3

let db = db_open "upick.db"

let error err message =
  let () = prerr_endline (Rc.to_string err) in
  let () = prerr_endline (errmsg db) in
  let () = prerr_endline message in
  let _closed = db_close db in
  let () = prerr_endline "exit" in
  exit 1

let create_users_table () =
  let create_usertable =  {|
  CREATE TABLE IF NOT EXISTS users (  
  username TEXT PRIMARY KEY NOT NULL, 
  password TEXT NOT NULL, 
  name TEXT NOT NULL,
  is_admin INTEGER DEFAULT 0);
  |}
  in match exec db create_usertable with
  | Rc.OK -> ()
  | err -> 
    let message = "Can't create table users (malformed sql/already exists)" in
    error err message

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
  | err -> let message = 
             "Can't create table friends (malformed sql/already exists)" in
    error err message

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
  | err -> let message = "Can't 
              create table restrictions (malformed sql/already exists)" in
    error err message

let create_restriction_index () =
  let create_restrictions = {|
  CREATE TABLE IF NOT EXISTS restriction_index ( 
    restriction TEXT UNIQUE NOT NULL); 
    |}
  in match exec db create_restrictions with
  | Rc.OK -> ()
  | err -> let message = 
             "Can't create table restriction (malformed sql/already exists)" in
    error err message

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
          ON DELETE SET NULL);
  |}
  in match exec db create_groups_info_table with
  | Rc.OK -> ()
  | err -> let message = 
             "Can't create table group_info (malformed sql/already exists)" in
    error err message

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
    preferences TEXT,
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
  | err ->
    let message = "Can't create table groups (malformed sql/already exists)" in
    error err message

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
  | err ->
    let message = "Can't create table votes (malformed sql/already exists)" in
    error err message

let create_group_invites_table () = 
  let create_group_invite_table = {|
    CREATE TABLE IF NOT EXISTS group_invites ( 
    group_id INTEGER NOT NULL,  
    user_id INTEGER NOT NULL, 
    FOREIGN KEY(group_id) REFERENCES group_info(rowid)
          ON DELETE SET NULL
    FOREIGN KEY(user_id) REFERENCES user(rowid)
          ON DELETE SET NULL);
    |}
  in match exec db create_group_invite_table with
  | Rc.OK -> ()
  | err -> let message = "Can't 
      create table group_invites (malformed sql/already exists)" in
    error err message

let create_cuisines_table () = 
  let create_cuisine_table = {|
    CREATE TABLE IF NOT EXISTS cuisines ( 
    cuisine_id INTEGER NOT NULL,  
    cuisine TEXT NOT NULL);
    |}
  in match exec db create_cuisine_table with
  | Rc.OK -> ()
  | err -> let message = 
             "Can't create table cuisines (malformed sql/already exists)" in
    error err message

let create_preferences_table () = 
  let create_preference_table = {|
    CREATE TABLE IF NOT EXISTS preferences ( 
    preference TEXT NOT NULL);
    |}
  in match exec db create_preference_table with
  | Rc.OK -> ()
  | err -> let message = 
             "Can't create table preferences (malformed sql/already exists)" in
    error err message

let create_visited_restaurant_table () = 
  let create_visit_table = {|
  CREATE TABLE IF NOT EXISTS visited_restaurants (
    user_id INTEGER NOT NULL,
    restaurant TEXT NOT NULL, --JSON
    FOREIGN KEY(user_id) REFERENCES users(rowid)
          ON DELETE SET NULL);
  |}
  in match exec db create_visit_table with
  | Rc.OK -> ()
  | err -> let message = "Can't 
      create table visited_restaurants (malformed sql/already exists)" in
    error err message

let create_feedbacks_table () = 
  let create_feedback_table = {|
  CREATE TABLE IF NOT EXISTS feedback (
    rating FLOAT NOT NULL,
    comments TEXT DEFAULT NULL);
  |}
  in match exec db create_feedback_table with
  | Rc.OK -> ()
  | err -> let message = "Can't 
      create table feedback (malformed sql/already exists)" in
    error err message


let set_admins () = 
  let env_field fld = List.assoc fld (Dotenv.parse ())
                      |> String.split_on_char ',' in
  let usernames = env_field "ADMINS" in
  let passwords = List.map (fun pw -> pw |> Bcrypt.hash 
                                      |> Bcrypt.string_of_hash)
      (env_field "PASSWORDS") in 
  let names = env_field "NAMES" in 
  try begin
    assert (List.length usernames = List.length passwords && 
            List.length usernames = List.length names);
    for i = 0 to List.length usernames do 
      let sql = Printf.sprintf 
          "INSERT INTO users (username, password, name, is_admin) 
    VALUES ('%s','%s','%s', 1); "
          (List.nth usernames i) (List.nth passwords i) (List.nth names i ) in
      ignore (exec db sql);
    done 
  end with _ -> ()

let create_tables _ = 
  () 
  |> create_users_table
  |> create_friends_table
  |> create_restriction_index
  |> create_restrictions_table
  |> create_groups_info_table
  |> create_groups_table
  |> create_votes_table 
  |> create_group_invites_table
  |> create_cuisines_table
  |> create_preferences_table
  |> create_visited_restaurant_table
  |> create_feedbacks_table
  |> set_admins

