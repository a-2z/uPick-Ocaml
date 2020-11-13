open Dbquery

let _ = create_tables (); ignore(add_user "reetuparikh" "reetu123" "Reetu");
ignore(add_user "andrewzeng" "andrew123" "Andrew");
ignore(add_restrictions 1 3); ignore(add_restrictions 1 1); ignore(add_restrictions 1 1);
ignore(add_friends 1 2); add_group_info "party" 1; add_groups 1 3