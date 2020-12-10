(* open Sqlite3 *)

(*
starting with wanting to start voting
wait for an accepted /ready post request. 
-/ready -> call process_survey -> process_survey will return top 5 restaurants 
and sets top_5 to null. /ready will just return true.

/vote -> checks if top_5 is null, if so then accepts vote, voting is uploaded
/calculate -> calculate and return the order in which restaurants were 
elected. 
*)

(* let num_surveyed group_id = 
   Dbquery.lst_from_col "surveyed" "groups" "group_id= " ^ string_of_int group_id *)