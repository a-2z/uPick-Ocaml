open App_state

let run_script () = fun _ ->
  load "state.json";
  add_restaurant "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_restaurant "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_restaurant "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_restaurant "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_restaurant "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_restaurant "mickeyds" 30.1123 44.15 "fast food" 4.67 ["eggs", "dairy"] 15 30;
  add_user "andrew123" "1234" "andrew";
  add_user "andrew124" "1234" "andrew";
  add_user "andrew123" "1234" "andrew";
  add_user "andrew123" "1234" "andrew";
  add_user "andrew123" "1234" "andrew";
  make_friends (get_user "andrew123") (get_user "andrew1234");
  make_group "andrew123" "Martha's tea party";
  join_group (get_user "andrew1234") (get_group "andrew123" "Martha's tea party");
  join_group (get_user "andrew123") (get_group "andrew1234" "Martha's tea party");
  save (); 


