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

let json_int_lst (lst : int list) = 
  let rec aux in_lst acc = 
    match in_lst with 
    | [] -> acc ^ "]"
    | last :: [] -> acc ^ string_of_int last ^ "]"
    | h :: t -> aux t (acc ^ (string_of_int h) ^ ", ") in
  aux lst "["

let json_int_lst_opt (lst : int list option) = 
  let rec aux in_lst acc = 
    match in_lst with 
    | [] -> acc ^ "]"
    | last :: [] -> acc ^ string_of_int last ^ "]"
    | h :: t -> aux t (acc ^ (string_of_int h) ^ ", ") in
  match lst with
  | None -> ""
  | Some v -> aux v "["
