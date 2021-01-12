let to_char_lst s = List.init (String.length s) (String.get s)

let ascii_range min max c = 
	Char.code c >= min && Char.code c <= max

let has_digit = List.exists (ascii_range 48 57)

let has_upper = List.exists (ascii_range 65 90)

let has_lower = List.exists (ascii_range 97 122)

let is_long pw = List.length pw >= 7

let is_valid password = 
let c = to_char_lst password in 
has_digit c && has_upper c && has_lower c && is_long c
