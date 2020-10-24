module type Json_helper = sig 
  val json_string_lst : string list -> string
  val json_dict_lst : string list -> string
end

module Json_helper = struct  
  (**Returns a string representing [lst] in a JSON format.
     Note: the return is not a valid JSON itself.*)
  let json_string_lst (lst : string list) = 
    let rec aux in_lst acc = 
      match in_lst with 
      | [] -> acc ^ "]"
      | last :: [] -> acc ^ "\"" ^ last ^ "\"" ^ "]"
      | h :: t -> aux t (acc ^ "\"" ^ h ^ "\"" ^ ", ") in
    aux lst "["

  let json_dict_lst (lst : string list) =
    let rec aux in_lst acc = 
      match in_lst with 
      | [] -> acc ^ "]"
      | last :: [] -> acc ^ last ^ "]"
      | h :: t -> aux t (acc ^ h ^ ", ") in
    aux lst "["
end
