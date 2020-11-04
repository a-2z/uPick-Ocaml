open App_state

let script () =
  let instance = load "state.json" in
  add_restaurant instance 
    "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
  add_restaurant instance 
    "mcDonalds" 33.12 92.15 "fast food" 4.27 ["dairy"] 5 20;
  add_restaurant instance  
    "pizza hut" 98.12 94.15 "fast food" 4.17 ["eggs"; "dairy"] 3 10;
  add_restaurant instance 
    "outback steakhouse" 31.12 49.15 "fast food" 4.7 ["eggs"; "dairy"] 6 2;
  add_user instance "andrew123" "xxandrew1234xx" "Andrew";
  add_user instance "andrew12" "xxandrew1234xx" "Andrew";
  add_user instance "reetu123" "xxreetu1234xx" "Reetu";
  add_user instance "zach123" "xxzach1234xx" "Zach";
  add_group instance "birthday party" "andrew123";
  add_group instance "pizza party" "andrew12";
  add_group instance "party" "reetu123";
  add_group instance "christmas party" "zach123";
  make_friends instance (get_user instance "andrew123") 
    (get_user instance "andrew12");
  (*Save the file*)
  save instance

let () = script ()