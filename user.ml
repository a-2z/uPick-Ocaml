type restrictions = Dairy | Shellfish
(** *)
type foods = Dairy | Shellfish | Cake | Mexican 
type preference = 
{
	time : int;
	dist : int;
	price : int;
	openness : bool;
	restrictions : restrictions;
}

type t = 
  {
	id : int; 
	username : string;
	password : string;
	name : string; 
	friends : int list;
	preferences : preference;
	visited : int list;
	}
