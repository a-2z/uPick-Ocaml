open App_state

let run_script () = fun _ ->
  let instance = load "state.json" in
  add_restaurant instance 
    "taco bell" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
  add_restaurant instance 
    "mcDonalds" 33.12 92.15 "fast food" 4.67 ["dairy"] 5 20;
  add_restaurant instance  
    "pizza hut" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
  add_restaurant instance 
    "Outback Steakhouse" 30.12 44.15 "fast food" 4.67 ["eggs"; "dairy"] 15 30;
  save instance;



