open Sqlite3

let db = db_open "upick.db"

type user = {
  id : int;
  username : string;
  password : string;
  name : string;
  friends : int list; 
  restrictions : int list; 
  groups : int list
}

type group = {
  id : int;
  name : string;
  host_id : int;
  members : int list;
}

type restriction = {
  id : int;
  name : string;
}

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

(**[lst_from_col sql_col sql_tbl sql_where f] is a list of lists containing 
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
    let value = (row_data stmnt).(0)
                |> Data.to_string_coerce 
                |> f in
    arr := Array.append !arr [|value|]
  done;
  if v then Array.to_list !arr
  else 
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

(*insertion functions *)
let add_user username password name =
  let sql =
    Printf.sprintf "INSERT INTO users VALUES('%s','%s','%s')"
      username password name in
  make_response (exec db sql)

(**[add_friends friend1 friend2 inserts a pairing of two friends]
   Requires: friend1 is not friend2
   Raises: Invalid_arg*)
let add_friends friend1 friend2 = 
  try 
    assert (friend1 <> friend2);
    let sql =
      Printf.sprintf "INSERT INTO friends VALUES(%d, %d)"
        friend1 friend2 in
    make_response (exec db sql)
  with e -> 
    print_endline (Printexc.to_string e);
    print_endline "Cannot friend yourself";
    None

let add_restrictions user_id restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restrictions VALUES(%d, %d)"
      user_id restriction in
  make_response (exec db sql)

let add_restrictions_index restriction = 
  let sql =
    Printf.sprintf "INSERT INTO restriction_index VALUES('%s')"
      restriction in
  make_response (exec db sql)

(* FIX INCREMENTING NUM_MEMBERS BC IT INCREMENTS BEFORE THE UNIQUE CONSTRAINT ERROR *)
let add_groups group_id member_id = 
  let sql =
    Printf.sprintf "INSERT INTO groups (group_id, member_id) VALUES(%d, %d)"
      group_id member_id in
  let resp = make_response (exec db sql) in 
  let sql = {|
  UPDATE group_info 
    SET num_members = num_members + 1 
  WHERE rowid = |} ^ string_of_int group_id in
  ignore (exec db sql); resp (*Ignore return code of the update operation *)

let add_group_info group_name host_id = 
  let sql =
    Printf.sprintf 
      "INSERT INTO group_info (group_name, host_id) VALUES('%s', %d)"
      group_name host_id in
  match exec db sql with
  | Rc.OK ->
    let id = Sqlite3.last_insert_rowid db in
    Printf.printf "Row inserted with id %Ld\n" id;
    ignore (add_groups (Int64.to_int id) host_id); Some id
  | r -> prerr_endline (Rc.to_string r); prerr_endline (errmsg db); None

(* ACCOUNT FOR NO VOTES IN ACC *)
let add_votes group_id user_id restaurant_id_lst = 
  let str_gr = string_of_int group_id in
  print_endline str_gr;
  let check = 
    (* print_endline ("count val: " ^ string_of_int ((count "group_info" 
                                                     ("voting_allowed = 1 AND rowid = " ^ str_gr)))); *)
    (count "group_info" ("voting_allowed = 1 AND rowid = " ^ str_gr)) = 1 in 
  if check 
  then 
    let rec add_user_votes group_id user_id count acc lst = begin 
      match lst with
      | [] -> make_response (exec db acc)
      | hd :: tl ->
        let sql = Printf.sprintf 
            "INSERT INTO votes VALUES(%d, %d, %d, %d); "
            group_id user_id count hd in
        add_user_votes group_id user_id (count+1) (acc ^ sql) tl end in
    add_user_votes group_id user_id 1 "" restaurant_id_lst
  else None
  (* (fun x -> print_endline "/ready was not true?"; x) *)

(* delete user *)

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
    Requires: A valid userid is inputted, valid [username], [password] inputted, 
    valid [name] inputted, valid [friends] inputted, [restricitons] inputted,
    [groups] inputted definined in the same user *)
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
  {
    id = userid;
    username = arr1.(0);
    password = arr1.(1);
    name = arr1.(2); 
    friends = friends;
    restrictions = restrictions;
    groups = groups;
  }

let get_group groupid = 
  let arr1 = single_row_query 
      "group_name, host_id" "group_info" 
      ("rowid = " ^ string_of_int groupid) in 
  let mem_lst = lst_from_col 
      "member_id" "groups" 
      ("group_id = " ^ string_of_int groupid) int_of_string in 
  {
    id = groupid;
    name = arr1.(0);
    host_id = arr1.(1) |> int_of_string; 
    members = mem_lst
  }

let get_restrictions () = 
  lst_from_col "restriction" "restriction_index" "1 = 1" (fun x -> x)

let get_restriction_by_id rest_id = 
  let rest = single_row_query 
      "restriction" "restriction_index" 
      ("rowid = " ^ string_of_int rest_id) in
  rest.(0)

(* INSERT VOTING INFO FROM BALLOT INTO GROUPS TABLE *)
let ans_survey user_id group_id loc_x loc_y cuisine price range = 
  let sql = Printf.sprintf {|
  UPDATE groups 
  SET loc_x = %f, loc_y = %f, 
  target_price = %d, cuisines = %s, range = %d, surveyed = 1 
  WHERE member_id = %d AND group_id = %d|} 
      loc_x loc_y price cuisine range user_id group_id in
  make_response (exec db sql) 

let avg_flt col n g_id =
  let n = float_of_int n in
  let g = string_of_int g_id in 
  lst_from_col ~unique:false col "groups" ("group_id = " ^ g) float_of_string
  |> List.fold_left ( +. ) 0.
  |> fun x -> x /. n

let avg_int col n g_id = 
  let g = string_of_int g_id in 
  lst_from_col ~unique:false col "groups" ("group_id = " ^ g) int_of_string 
  |> List.fold_left ( + ) 0
  |> fun x -> x / n

(* change voting_allowed from 1 to 0 when we update database with results*)

let string_of_tuplst lst =
  let rec helper acc = function
    | [] -> acc
    | (hs, hv) :: t -> begin 
        let elm = "(" ^ string_of_int hs ^ ", " ^ string_of_int hv ^ ") " in 
        helper (acc ^ elm) t end in 
  "{" ^ (helper "" lst) ^ "}" 

let calculate_votes g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (count "group_info" ("host_id = " ^ str_hid ^ " AND rowid = " ^ str_gid) 
     > 0 && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0) 
  end then 
    let rank_lst = lst_from_col ~voting:true "ranking" "votes" 
        ("group_id = " ^ str_gid) int_of_string in
    let rest_lst = lst_from_col ~voting:true "restaurant_id" "votes" 
        ("group_id = " ^ str_gid) int_of_string in
    let matched_ranks = List.combine rest_lst rank_lst in 
    print_endline (string_of_tuplst matched_ranks); 
    let rec ranked_lst acc = function
      | [] -> acc
      | (rest, rank) :: t -> if List.mem_assoc rest acc then 
          let current_vote = rank + (List.assoc rest acc) in 
          let new_acc = acc |> List.remove_assoc rest 
                        |> List.cons (rest, current_vote) in 
          ranked_lst new_acc t
        else 
          let new_acc = (rest, rank) :: acc in
          ranked_lst new_acc t in 
    let ranks = ranked_lst [] matched_ranks in 
    print_endline (string_of_tuplst ranks); 
    (* let rec top_pick_tuple result ranks = match ranks with
    | [] -> result
    | (rest, rank) :: t -> if rank < snd result || snd result < 0
    then top_pick_tuple (rest, rank) t
    else top_pick_tuple result t in  *)
    let compare_op = fun x y -> if snd x > snd y then 1 else if snd x < snd y 
    then -1 else 0 in 
    let ordered_ranks = List.sort compare_op ranks in 
    print_endline (string_of_tuplst ordered_ranks); 
    let top_pick = fst (List.hd ordered_ranks) in
    (* let top_pick = fst ((top_pick_tuple (-1,-1) ranks)) in  *)
    let sql = Printf.sprintf 
        "UPDATE group_info SET top_pick = %d WHERE rowid = %d;
         UPDATE group_info SET voting_allowed = 0 WHERE rowid = %d" 
        top_pick g_id g_id in 
        make_response (exec db sql)
  else None

(*  ^ "AND group_id = " ^ str_gid) *)
let process_survey g_id h_id = 
  let str_gid = string_of_int g_id in 
  let str_hid = string_of_int h_id in
  if begin
    (count "group_info" ("host_id = " ^ str_hid ^ " AND rowid = " ^ str_gid) 
     > 0 && count "groups" ("surveyed = 1 AND member_id = " ^ str_hid) > 0) 
  end then begin
    let to_drop = {|
  DELETE FROM groups
  WHERE surveyed = 0 AND group_id = |} ^ str_gid in
    ignore (make_response (exec db to_drop));
    let num_votes = count "groups" ("group_id = " ^ str_gid) in 
    let x = avg_flt "loc_x" num_votes g_id in 
    let y = avg_flt "loc_y" num_votes g_id in 
    let price = avg_int "target_price" num_votes g_id in 
    let range = avg_int "range" num_votes g_id in 
    let cuisines = 
      lst_from_col "cuisines" "groups" ("group_id = " ^ str_gid)
        (fun x -> x)
      |> fun l -> List.fold_right (fun x y -> x ^ "," ^ y) l ""
                  |> String.split_on_char ','
                  |> List.filter (fun s -> s <> "") in
    let sql = Printf.sprintf 
        "UPDATE group_info SET top_5 = '%s' WHERE rowid = %d;
        UPDATE group_info SET voting_allowed = 1 WHERE rowid = %d" 
        (Search.get_rests ~cuisine:cuisines x y range price) g_id g_id in 
    make_response (exec db sql)
  end else None

let create_tables () = Db.create_tables ()