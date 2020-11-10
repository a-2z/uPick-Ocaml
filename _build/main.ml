open Db 
open Dbquery

let _ = create_tables (); add_user "reetuparikh" "reetu123" "Reetu";
add_user "andrewzeng" "andrew123" "Andrew";
add_restrictions 1 "egg"; add_restrictions 1 "meat"; add_restrictions 1 "meat";
add_friends 1 2; add_group_info "party" 1; add_groups 1 2