module Json_help = struct  
  (**Returns a string representing [lst] in a JSON format.
     Note: the return is not a valid JSON itself.*)
  let json_string_lst (lst : string list) = 
    let rec aux in_lst acc = 
      match in_lst with 
      | [] -> acc ^ "]"
      | last :: [] -> acc ^ "\"" ^ last ^ "\"" ^ "]"
      | h :: t -> aux t (acc ^ "\"" ^ h ^ "\"" ^ ", ") in
    aux lst "["
end
