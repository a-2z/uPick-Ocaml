open Opium.Std

let _ = get "/restrictions"
    (fun _ -> let restriction = Dbquery.get_restrictions () in 
      `Json (Ezjsonm.list Ezjsonm.string restriction) |>  
      respond')