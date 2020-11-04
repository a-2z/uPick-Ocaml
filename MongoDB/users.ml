(* "127.0.0.1" *)

let myMongoDB = 
  let open Mongo in
  let url = "mongodb+srv://reetu-parikh:uPickMongoDBtemp@upick.wrnnr.mongodb.net/test" in
  let port = 27017 in (* int *)
  let dbName = "uPick" in
  let collectionName = "collection-users" in
  try 
    create url port dbName collectionName
  with 
    Mongo_failed ex -> Pervasives.print_endline ex


type t = {
  id : int; 
  username : string;
  mutable password : string;
  name : string; 
  mutable friends : int list;
  mutable restrictions : int list;
  mutable visited : int list;
  mutable groups : int list;
}

let person_to_bson t = 
  let friend_list = List.map (Bson.create_int64) t.friends in
  let restrictions_list = List.map (Bson.create_int64) t.friends in
  let visited_list = List.map (Bson.create_int64) t.visited in
  let groups_list = List.map (Bson.create_int64) t.groups in
  Bson.empty 
    |> Bson.add_element "id" (Bson.create_string t.id)
    |> Bson.add_element "username" (Bson.create_string t.username)
    |> Bson.add_element "password" (Bson.create_string t.password)
    |> Bson.add_element "name" (Bson.create_string t.name)
    |> Bson.add_element "friends" (Bson.create_list friend_list)
    |> Bson.add_element "restrictions" (Bson.create_list restrictions_list)
    |> Bson.add_element "visited" (Bson.create_list visited_list)
    |> Bson.add_element "groups" (Bson.create_list groups_list)

let bson_to_person bson = 
  let id = 
    Bson.get_element "id" bson
      |> Bson.get_int64 in
  let username = 
     Bson.get_element "username" bson
      |> Bson.get_string in
  let password = 
     Bson.get_element "password" bson
      |> Bson.get_string in
  let name = 
    Bson.get_element "name" bson
      |> Bson.get_string in
  let friends =
    Bson.get_element "friends" bson
      |> Bson.get_list 
      |> List.map (Bson.get_int64) in
  let restrictions =
    Bson.get_element "restrictions" bson
      |> Bson.get_list 
      |> List.map (Bson.get_int64) in
  let visited =
    Bson.get_element "visited" bson
      |> Bson.get_list 
      |> List.map (Bson.get_int64) in
  let groups =
    Bson.get_element "groups" bson
      |> Bson.get_list 
      |> List.map (Bson.get_int64) in
  {id; username; password; name; friends; restrictions; visited; groups}

(** Main: Insert 5 documents into mongodb and print any exceptions that are thrown *)
let () =
  let people = [
      {
      id = 1;
      username = "reetuparikh";
      password = "reetu123";
      name = "Reetu";
      friends = [2;3;4];
      restrictions = [1;2];
      visited = [11];
      groups = [20]
    };
    {
      id = 2;
      username = "zachtegtmeier";
      password = "zach123";
      name = "Zach";
      friends = [1;3;4];
      restrictions = [1;3];
      visited = [11;33];
      groups = [20]
    };
    {
      id = 3;
      username = "andrewzeng";
      password = "andrewz123";
      name = "Andrew";
      friends = [1;2;4];
      restrictions = [];
      visited = [33];
      groups = [20]
    };
    {
      id = 4;
      username = "andrewosorio";
      password = "andrewo123";
      name = "Andrew";
      friends = [1;2;3];
      restrictions = [2];
      visited = [11;33];
      groups = [20]
    };
    {
    id = 5;
      username = "john smith";
      password = "johnsmith123";
      name = "John";
      friends = [1;2];
      restrictions = [1;2];
      visited = [11;33];
      groups = [4]
    }
      ] 
  in 
  try 
      (* val insert : t -> Bson.t list -> unit *)
    List.map person_to_bson people
      |> Mongo.insert myMongoDB

  with 
    Mongo.Mongo_failed ex -> (
      Pervasives.print_endline ex;
      Mongo.destory myMongoDB
  )