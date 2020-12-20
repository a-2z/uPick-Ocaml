open Lwt.Infix
open Sqlite3
open Yojson.Basic
open Yojson.Basic.Util

let db = db_open "upick.db"

type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  groups : int list;
  visited: string list
}

type group = {
  id : int;
  name : string;
  host_id : int;
  members : int list;
  voting_allowed : bool;
  top_5 : string option;
  top_pick : string option;
}

type restriction = {
  id : int;
  name : string;
}

(**Escapes all single quotes in a json string with an additional single quote
   for SQL compliance*)
let sanitize sql = Str.global_replace (Str.regexp "'") "''" sql

let make_stmt sql = prepare db sql 

(**[single_row_query sql_col sql_tbl sql_where] is an array of strings 
   representing the columns [sql_select] in a single row matching [sql_where]*)
let single_row_query
    (sql_select : string) 
    (sql_tbl : string) 
    (sql_where : string) = 
  let sql = Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s;
  |} sql_select sql_tbl sql_where in
  (* print_endline sql; *)
  let stmnt = 
    make_stmt sql in 
  ignore (step stmnt);
  Array.map Data.to_string_coerce (row_data stmnt)

(**[  sql_col sql_tbl sql_where f] is a list of lists containing 
   the values of [sql_col] in [sql_tbl] satisfying [sql_where], converted into 
   their primitive types from a string representation with [f]
   Returns: a list of lists of values for a query
   Requires: [sql_col] contains only one column
   [sql_col], [sql_tbl], abd [sql_where] are defined in the schema.*)
let lst_from_col 
    ?unique:(u = true)
    ?voting:(v = false)
    (sql_col : string) 
    (sql_tbl : string) 
    (sql_where : string) 
    (f : string -> 'a) = 
  let arr = ref [||] in
  let sql = (Printf.sprintf {|
  SELECT %s
  FROM %s
  WHERE %s;
  |} sql_col sql_tbl sql_where) in 
  let stmnt = make_stmt sql in 
  while (step stmnt) = ROW do 
    let value = (row_data stmnt).(0) |> Data.to_string_coerce |> f in
    arr := Array.append !arr [|value|] done;
  if v then Array.to_list !arr else 
  if u then List.sort_uniq compare (Array.to_list !arr)
  else List.sort compare (Array.to_list !arr)

(**Returns the number of occurrences of rows satisfying [sql_where] in
   [sql_tbl*)
let count sql_tbl sql_where = 
  single_row_query "COUNT (*)" sql_tbl sql_where 
  |> fun arr -> int_of_string (arr.(0))

(**[make_response] returns [Some last_id] if an insertion operation succeeded
   and [None] otherwise.*)
let make_response = function 
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id; Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

let delete_sql (sql_tbl : string) (sql_where : string) =
  Printf.sprintf {|DELETE FROM %s WHERE %s; |} sql_tbl sql_where

let is_host str_hid str_gid = 
  count "group_info" ("host_id = " ^ str_hid ^ " AND rowid = " ^ str_gid) > 0

let is_admin user_id = 
  count "users" ("is_admin = 1 AND rowid = " ^ (string_of_int user_id)) > 0

(*insertion functions *)
let add_user username password name =
  if name = "" || username = ""
  then None  
  else
    let sql =
      Printf.sprintf 
        "INSERT INTO users (username, password, name) VALUES('%s','%s','%s'); "
        username password name in
    make_response (exec db sql)

(**[add_friends friend1 friend2 inserts a pairing of two friends]
   Requires: friend1 is not friend2
   Raises: Invalid_arg*)
let add_friends friend1 friend2 = 
  try 
    assert (friend1 <> friend2);
    if count "users" ("rowid = " ^ string_of_int friend2) > 0 then
      if friend1 < friend2 then 
        let sql =
          Printf.sprintf "INSERT INTO friends VALUES(%d, %d); "
            friend1 friend2 in
        make_response (exec db sql)
      else 
        let sql =
          Printf.sprintf "INSERT INTO friends VALUES(%d, %d); "
            friend2 friend1 in
        make_response (exec db sql)
    else None
  with e -> 
    print_endline (Printexc.to_string e);
    print_endline "Cannot friend yourself";
    None

let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restrictions VALUES(%d, %d); "
      user_id restriction in
  make_response (exec db sql)

let add_rest_pref_helper user_id sql_text = 
  if is_admin user_id
  then
    let sql = sql_text in
    make_response (exec db sql)
  else None

let add_restrictions_index user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restriction_index VALUES('%s'); "
      restriction in
  add_rest_pref_helper user_id sql

let add_preferences_index user_id preference = 
  let sql =
    Printf.sprintf "INSERT INTO preferences VALUES('%s'); "
      preference in
  add_rest_pref_helper user_id sql

let rm_rest_pref_helper user_id sql_text = 
  if is_admin user_id
  then
    let sql = sql_text in
    make_response (exec db sql)
  else None

let remove_restrictions_index user_id restriction_id =
  if count "restriction_index" ("rowid = " ^ string_of_int restriction_id) > 0 
  then
    let sql_index = delete_sql "restriction_index" 
        ("rowid = " ^ string_of_int restriction_id) in
    let sql_restr = delete_sql "restrictions" 
        ("restriction = " ^ string_of_int restriction_id) in
    rm_rest_pref_helper user_id (sql_restr ^ sql_index)
  else None

let remove_preferences_index user_id preference_id =
  if count "preference_index" ("rowid = " ^ string_of_int preference_id) > 0 
  then
    let sql = delete_sql "preferences" 
        ("rowid = " ^ string_of_int preference_id) in
    rm_rest_pref_helper user_id sql
  else None

let add_cuisine user_id cuisine_id cuisine = 
  if is_admin user_id
  then
    let sql =
      Printf.sprintf "INSERT INTO cuisines VALUES(%d, '%s'); "
        cuisine_id cuisine in
    make_response (exec db sql)
  else None

let remove_cuisine user_id cuisine_id =
  if is_admin user_id && 
     count "cuisines" ("cuisine_id = " ^ string_of_int cuisine_id) > 0 
  then
    let sql = delete_sql "cuisines" 
        ("cuisine_id = " ^ string_of_int cuisine_id) in
    make_response (exec db sql)
  else None

let join_group group_id member_id = 
  if count "group_invites" ("group_id = " ^ string_of_int group_id ^
                            " AND user_id = " ^ string_of_int member_id) > 0 
  then begin 
    let sql = Printf.sprintf {|
  INSERT INTO groups (group_id, member_id) VALUES(%d, %d); |} 
        group_id member_id in 
    let resp = make_response (exec db sql) in 
    if resp = None then None else  
      let update_sql = Printf.sprintf {|UPDATE group_info 
  SET num_members = num_members + 1 
  WHERE rowid = %d; |} group_id in 
      make_response (exec db update_sql) end 
  else None

let add_group_invites group_id user_id host_id = 
  if is_host (string_of_int host_id) (string_of_int group_id) && 
     count "group_info" ("rowid = " ^ string_of_int group_id) > 0 &&
     count "users" ("rowid = " ^ string_of_int user_id) > 0 
  then
    let sql = "INSERT INTO group_invites VALUES (" ^ (string_of_int group_id) ^ 
              ", " ^(string_of_int user_id)^ "); " in 
    make_response (exec db sql) 
  else None

let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf 
      "INSERT INTO group_info (group_name, host_id) VALUES('%s', %d); "
      group_name host_id in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id;
    ignore (add_group_invites (Int64.to_int id) host_id host_id);
    ignore (join_group (Int64.to_int id) host_id); Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None 

let remove_a_user str_did = 
  let lst = lst_from_col "group_id" "groups" ("member_id = " ^ str_did) 
      (fun x -> x) in 
  let sql_lst = List.map (fun x -> "UPDATE group_info 
        SET num_members = num_members - 1 
        WHERE rowid = " ^ x ^ "; " ) lst in 
  List.fold_left (fun x y -> x ^ y) "" sql_lst

let delete_user user_id delete_id = 
  let str_did = string_of_int delete_id in
  let are_users = count "users" ("rowid = " ^ string_of_int user_id) > 0 &&
                  count "users" ("rowid = " ^ str_did) > 0 in
  let is_a_host = count "group_info" 
      ("host_id = " ^ str_did) > 0 in 
  if are_users && (user_id = delete_id || is_admin user_id) 
     && is_admin delete_id = false && is_a_host = false
  then 
    let user = delete_sql "users" ("rowid = " ^ str_did) in
    let groups = delete_sql "groups" ("member_id = " ^ str_did) in
    let group_invites = delete_sql "group_invites" ("user_id = " ^ str_did) in
    let votes = delete_sql "votes" ("user_id = " ^ str_did) in
    let restrictions = delete_sql "restrictions" ("user_id = " ^ str_did) in
    let group_members_update = remove_a_user str_did in 
    let friends = delete_sql "friends" 
        ("friend_1 = " ^ str_did ^ " OR friend_2 = " ^ str_did) in 
    make_response (exec db (user ^ groups ^ group_invites ^ votes 
                            ^ restrictions ^ friends ^ group_members_update))
  else None

let delete_group user_id group_id = 
  let str_uid = string_of_int user_id in
  let str_gid = string_of_int group_id in
  let is_user = count "users" ("rowid = " ^ str_uid) > 0 in
  let is_group = count "group_info" ("rowid = " ^ str_gid) > 0 in
  let is_host_or_admin = is_host (str_uid) (str_gid) || is_admin user_id in
  if is_user && is_group && is_host_or_admin 
  then 
    let group_info = delete_sql "group_info" ("rowid = " ^ str_gid) in
    let groups = delete_sql "groups" ("group_id = " ^ str_gid) in
    let group_invites = delete_sql "group_invites" ("group_id = " ^ str_gid) in
    let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
    make_response (exec db (votes ^ group_invites ^ groups ^ group_info))
  else None

(* if searches appear off maybe we need hostid? *)
let num_rests str_gid =
  let json_str = List.hd (
      lst_from_col "top_5" "group_info" ("rowid = " ^ str_gid) (fun x -> x)) in 
  let json = from_string json_str in 
  json |> member "restaurants" |> to_list |> List.length

let rec add_user_votes group_id user_id count acc lst =
  match lst with
  | [] -> acc
  | hd :: tl ->
    let sql = Printf.sprintf 
        "INSERT INTO votes VALUES(%d, %d, %d, %d); "
        group_id user_id count hd in
    add_user_votes group_id user_id (count + 1) (acc ^ sql) tl

(**Inserts a [user_id]'s votes in [group_id] with [ballot] representing
   the positions of restaurants in the list ranked in order*)
let add_votes group_id user_id ballot = 
  let str_gid = string_of_int group_id in
  let str_uid = string_of_int user_id in
  if count "group_info" ("voting_allowed = 1 AND rowid = " ^ str_gid) = 1
  then begin 
    let rest_size = num_rests str_gid in 
    let rec valid_ballot counter lst = 
      if counter = 0 then true else 
      if List.mem (counter-1) lst 
      then valid_ballot (counter-1) lst 
      else false in 
    if List.length ballot = rest_size && valid_ballot rest_size ballot
    then let new_votes = add_user_votes group_id user_id 1 "" ballot in 
      if count "votes" 
          ("group_id = " ^ str_gid ^ " AND user_id = " ^ str_uid) > 0
      then let drop_sql = 
             delete_sql "votes" ("group_id = " ^ str_gid ^ " AND user_id = "
                                 ^ str_uid) in 
        make_response (exec db (drop_sql ^ new_votes))
      else make_response (exec db new_votes)
    else None end else None

let login username = 
  try
    Some (single_row_query "password" "users" 
            ("username = '" ^ username ^ "'")).(0)
  with e -> ignore(e); None

(**[id_by_usr usr] is the id of the user with unique username [usr]*)
let id_by_usr usr =
  (single_row_query "rowid" "users" ("username = '" ^ usr ^ "'")).(0)
  |> int_of_string

(** [get_user userid] returns a representation of a single user from the 
    database in type user.  
    Requires: A valid userid is inputted, valid [username], [password] 
    inputted, valid [name] inputted, valid [friends] inputted, [restricitons] 
    inputted, [groups] inputted definined in the same user *)
let get_user userid = 
  let arr1 = single_row_query "username, password, name" "users" 
      ("rowid = " ^ string_of_int userid) in
  let friends1 = lst_from_col "friend_2" "friends" 
      ("friend_1 = " ^ string_of_int userid) int_of_string in
  let friends2 = lst_from_col "friend_1" "friends" 
      ("friend_2 = " ^ string_of_int userid) int_of_string in
  let friends = List.sort_uniq compare (friends1 @ friends2) in
  let restrictions = lst_from_col "restriction" "restrictions" 
      ("user_id = " ^ string_of_int userid) int_of_string in
  let groups = lst_from_col "group_id" "groups" 
      ("member_id = " ^ string_of_int userid) int_of_string in
  let visited = lst_from_col "restaurant" "visited_restaurants" 
    ("user_id = " ^ string_of_int userid) (fun x -> x) in
      {
        id = userid;
        username = arr1.(0);
        password = arr1.(1);
        name = arr1.(2); 
        friends = friends;
        restrictions = restrictions;
        groups = groups;
        visited = visited;
      }

let get_group group_id = 
  let arr1 = single_row_query 
      "group_name, host_id, voting_allowed, top_5, top_pick" "group_info" 
      ("rowid = " ^ string_of_int group_id) in 
  let mem_lst = lst_from_col 
      "member_id" "groups" 
      ("group_id = " ^ string_of_int group_id) int_of_string in 
  {
    id = group_id;
    name = arr1.(0);
    host_id = arr1.(1) |> int_of_string; 
    voting_allowed = arr1.(2) = "1";
    members = mem_lst;
    top_5 = if arr1.(3) = "" then None else Some arr1.(3);
    top_pick = if arr1.(4) = "" then None else Some arr1.(4)
  }

let get_restrictions () = 
  lst_from_col "restriction" "restriction_index" "1 = 1" (fun x -> x)

let get_restriction_by_id rest_id = 
  let rest = single_row_query "restriction" "restriction_index" 
      ("rowid = " ^ string_of_int rest_id) in
  rest.(0)

let get_preferences () = 
  lst_from_col "preference" "preferences" "1 = 1" (fun x -> x)

let get_preference_by_id pref_id = 
  let pref = single_row_query "preference" "preferences" 
      ("rowid = " ^ string_of_int pref_id) in
  pref.(0)

let get_cuisines () = 
  let cuisine_id_lst = lst_from_col ~voting:true "cuisine_id" 
      "cuisines" "1 = 1" (fun x -> int_of_string x) in 
  let cuisine_lst = lst_from_col ~voting:true "cuisine" 
      "cuisines" "1 = 1" (fun x -> x) in 
  (cuisine_id_lst, cuisine_lst)

let get_cuisine_by_id cuisine_id = 
  let cuisine = single_row_query "cuisine" "cuisines" 
      ("cuisine_id = " ^ string_of_int cuisine_id) in
  cuisine.(0)

(* let get_visited_restaurants user_id request_user_id = 
   let str_uid = string_of_int user_id in 
   let str_ruid = string_of_int request_user_id in 
   let count_int = if user_id < request_user_id then count "friends" 
   ("friend_1 = " ^ str_uid ^ " AND friend_2 = " ^ str_ruid) else
   count "friends" ("friend_1 = " ^ str_ruid ^ " AND friend_2 = " ^ str_uid) in
   if count_int > 0 || user_id = request_user_id then 
   lst_from_col "restaurant" "visited_restaurants" ("user_id = " ^ str_ruid)
   (fun x -> x)
   else [] *)

let deletion_updates str_gid str_uid = 
  let sql_grouprm = delete_sql 
      "groups" ("group_id = " ^ str_gid ^ " AND member_id = " ^ str_uid) in 
  let update_sql = Printf.sprintf {|
    UPDATE group_info 
    SET num_members = num_members - 1 
    WHERE rowid = %d; |} (int_of_string str_gid) in 
  let voting_updated = delete_sql "votes" 
      ("group_id = " ^ str_gid ^ " AND user_id = " ^ str_uid) in
  voting_updated ^ sql_grouprm ^ update_sql

let delete_from_group group_id member_id host_id = 
  let str_gid = string_of_int group_id in
  let str_hid = string_of_int host_id in
  let str_uid = string_of_int member_id in
  let num_members = count "groups" ("group_id = " ^ str_gid) in
  let is_host_bool = is_host str_hid str_gid in
  let mem_not_host = member_id <> host_id in
  let is_group_mem = count "groups" ("member_id = " ^ str_uid ^ 
                                     " AND group_id = " ^ str_gid) > 0 in
  if is_group_mem && (is_host_bool && (mem_not_host || num_members = 1) 
                      || (is_host_bool = false && mem_not_host = false)) then
    let response = deletion_updates str_gid str_uid in 
    if is_host_bool then 
      let sql_invite = delete_sql "group_invites" ("group_id = " ^ str_gid ^ 
                                                   " AND user_id = " ^ str_uid) 
      in 
      print_endline (response ^ sql_invite);
      make_response (exec db (response ^ sql_invite))
    else make_response (exec db response)
  else None

let reassign_host group_id user_id host_id = 
  let str_gid = string_of_int group_id in
  let str_hid = string_of_int host_id in
  let str_uid = string_of_int host_id in
  let is_group_mem = count "groups" ("member_id = " ^ str_uid ^ 
                                     " AND group_id = " ^ str_gid) > 0 in
  if is_host str_hid str_gid && is_group_mem then
    let update_sql = Printf.sprintf {|
    UPDATE group_info 
    SET host_id = %d 
    WHERE rowid = %d; |} user_id group_id in 
    make_response (exec db update_sql)
  else None

let ans_survey user_id group_id loc_x loc_y cuisine price range preferences = 
  let sql = Printf.sprintf {|
  UPDATE groups 
  SET loc_x = %f, loc_y = %f, target_price = %d, 
  cuisines = '%s', range = %d, preferences = '%s', surveyed = 1 
  WHERE member_id = %d AND group_id = %d; |} 
      loc_x loc_y price cuisine range preferences user_id group_id in
  make_response (exec db sql) 

let avg_flt col n g_id =
  let n = float_of_int n in
  let g = string_of_int g_id in 
  lst_from_col ~unique:false col "groups" ("group_id = " ^ g) float_of_string
  |> List.fold_left ( +. ) 0.
  |> fun x -> x /. n
              |> string_of_float 
              |> fun x -> if String.length x > 7 then 
                float_of_string (String.sub x 0 6)
              else float_of_string x

let avg_int col n g_id =  
  let g = string_of_int g_id in 
  lst_from_col ~unique:false col "groups" ("group_id = " ^ g) int_of_string 
  |> List.fold_left ( + ) 0
  |> fun x -> x / n

let ranks str_gid = 
  let rank_lst = lst_from_col ~voting:true "ranking" "votes" 
      ("group_id = " ^ str_gid) int_of_string in
  let rest_lst = lst_from_col ~voting:true "restaurant_id" "votes" 
      ("group_id = " ^ str_gid) int_of_string in
  let matched_ranks = List.combine rest_lst rank_lst in 
  let rec ranked_lst acc = function
    | [] -> acc 
    | (rest, rank) :: t -> if List.mem_assoc rest acc 
      then begin 
        let current_vote = rank + (List.assoc rest acc) in 
        let new_acc = acc |> List.remove_assoc rest 
                      |> List.cons (rest, current_vote) in 
        ranked_lst new_acc t
      end else 
        let new_acc = (rest, rank) :: acc in
        ranked_lst new_acc t in 
  ranked_lst [] matched_ranks

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  

let empty_survey_votes str_gid = 
  let votes = delete_sql "votes" ("group_id = " ^ str_gid) in
  let updated_surveys = "UPDATE groups SET loc_x = NULL, loc_y = NULL, 
  target_price = NULL, cuisines = NULL, range = NULL, preferences = NULL, 
  surveyed = 0 WHERE group_id = " ^ str_gid ^ "; " in
  votes ^ updated_surveys

let visited_entries str_gid top = 
  let member_lst = lst_from_col "member_id" "groups" ("group_id = " ^ str_gid)
      (fun x -> x) in 
  let sql_lst = List.map 
      (fun x -> "INSERT INTO visited_restaurants (user_id, restaurant) 
VALUES(" ^ x ^ ", '" ^ top ^ "'); ") member_lst in 
  List.fold_right (fun x y -> x ^ y) sql_lst ""

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if is_host str_hid str_gid 
  && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0 then 
    let ranks_final = ranks str_gid in 
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
      then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks_final in 
    let top_pick = fst (List.hd ordered_ranks) in
    let top = single_row_query "top_5" "group_info" ("rowid = " ^ str_gid) 
              |> fun row -> row.(0) 
                            |> sanitize 
                            |> Search.get_winner top_pick in 
    let sql = Printf.sprintf {|UPDATE group_info SET top_pick = '%s',
     voting_allowed = 0 WHERE rowid = %d; |}
        (sanitize top) g_id in 
    let sql_visited = visited_entries str_gid (sanitize top) in 
    let empty_tables = empty_survey_votes str_gid in
    make_response (exec db (sql ^ sql_visited ^ empty_tables)) else None

let format_cuisines group_id = 
  lst_from_col "cuisines" "groups" ("group_id = " ^ (string_of_int group_id)) 
    (fun x -> x)
  |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
              |> String.split_on_char ','
              |> List.filter (fun s -> s <> "")

let gather_restrictions g_id = 
  let mems = lst_from_col "member_id" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in 
  let rec restr_lst acc = function
    | [] -> acc
    | h :: t -> restr_lst 
                  ((lst_from_col "restriction" "restrictions" 
                      ("user_id = " ^ h) (fun x -> x)) :: acc) t in
  let restr = List.flatten (restr_lst [] mems) in 
  let f x = lst_from_col "restriction" "restriction_index" ("rowid = " ^ x) 
      (fun x -> x) in List.flatten (List.map f restr)

let calculate_survey cuisines x y range price g_id = 
  let pref = lst_from_col ~unique:false "preferences" "groups" 
      ("group_id = " ^ string_of_int g_id) (fun x -> x) in
  let pref_str = List.fold_left (fun x y -> x ^ "," ^ y) "" pref in
  let pref_list = String.split_on_char ',' pref_str in
  let pref_restr = pref_list @ (gather_restrictions g_id) in
  Search.get_rests ~cuisine:cuisines x y range price pref_restr >>= 
  fun res -> let sql = Printf.sprintf 
                 {|UPDATE group_info SET top_5 = '%s', voting_allowed = 1 
                 WHERE rowid = %d;|}
                 (sanitize res) g_id in
  Lwt.return (make_response (exec db sql))

let adjust_group str_gid = 
  let count_drop = 
    count "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in 
  let to_drop = 
    delete_sql "groups" ("surveyed = 0 AND group_id = " ^ str_gid) in
  ignore (make_response (exec db to_drop));
  let update_num_members = Printf.sprintf
      {|UPDATE group_info SET num_members = num_members - %d 
          WHERE rowid = %d; |}
      count_drop (int_of_string str_gid) in 
  ignore (make_response (exec db update_num_members))

let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (*Ensure that the user making the request is the host of the group*)
    is_host str_hid str_gid && 
    count "groups" ("surveyed = 1 AND member_id = " ^ str_hid ^ 
                    " AND group_id = " ^ str_gid) > 0 
  end then begin
    adjust_group str_gid;
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = format_cuisines g_id in 
    ignore(calculate_survey cuisines x y range price g_id);
    Some (Int64.zero)
  end else None

let add_feedback rating comments = 
  let str_rating = string_of_float rating in
  if comments = "" then 
    let sql = "INSERT INTO feedback (rating) VALUES(" ^ str_rating ^ "); " in 
    make_response (exec db sql)
  else
    let sql = "INSERT INTO feedback VALUES(" ^ str_rating ^ 
              ", '" ^ comments ^ "'); " in 
    make_response (exec db sql)

let top_visited () =
(* let rest_list = lst_from_col "restaurant" "visited_restaurants" 
  "1 = 1" (fun x -> x) in  *)
  let sql = "SELECT restaurant, 
  COUNT (restaurant) AS 'value_occurrance' 
  FROM visited_restaurants
  GROUP BY restaurant
  ORDER BY 'value_occurrence' DESC; " in
  ignore(make_response (exec db sql));
  ["apple"]
  
let create_tables () = Db.create_tables ()