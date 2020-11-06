open Sqlite3

let db = print_endline "hi"; db_open "test.db"

let gracefully_exist error message =
  let () = prerr_endline (Rc.to_string error) in
  let () = prerr_endline (errmsg db) in
  let () = prerr_endline message in
  let _closed = db_close db in
  let () = prerr_endline "Exiting ..." in
  exit 1

let create_contacts_table () =
  let create_table_sql = "CREATE TABLE contacts ( \
                          contact_id INTEGER PRIMARY KEY, \
                          first_name TEXT NOT NULL, \
                          last_name TEXT NOT NULL, \
                          email text NOT NULL UNIQUE, \
                          phone text NOT NULL UNIQUE \
                          );"
  in match exec db create_table_sql with
  | Rc.OK -> ()
  | r ->
    let message = "Unable to create table contacts." in
    gracefully_exist r message


