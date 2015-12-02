open Yojson.Basic.Util

(* First take the json file right as its compiled (This is for command line)*)
(* let listcommands = Array.to_list Sys.argv
let command = List.nth listcommands 1
let json = Yojson.Basic.from_file command *)

(* Gets the json file (This is for testing in utop)*)
let json = Yojson.Basic.from_file "trivia.json"


(* Extracts sets id or description (pass in option as "key") *)
let get_id_desc key json =
  member "sets" json
  |> map (member key)
  |> to_list
  |> filter_string

(* Extracts the  questions or answers and returns a bytes list*)
let get_q_a key json=
 member "sets" json
  |> map (member key)
  |> to_list
  |> filter_list
  |> List.map filter_string
  |> List.map (fun x -> ref x)

let get_points json =
  member "sets" json
  |> map (member "points")
  |> to_list
  |> filter_list
  |> List.map filter_int
  |> List.map (fun x -> ref x)


(*A type representing a single question*)
type question = {
  question: bytes;
  answer: bytes;
  point: int;
  }

(*Represents the entire library of questions at a given difficulty level*)
type question_db = {
  ids: bytes list;
  descriptions: bytes list;
  questions: bytes list ref list ref;
  answers: bytes list ref list ref;
  points: int list ref list ref;
}

let complete_db = {ids=get_id_desc "id" json;
descriptions=get_id_desc "description" json;
questions=ref (get_q_a "questions" json); answers=ref (get_q_a "answers" json);
points=ref (get_points json)}


(* Produces a random question based on difficulty level
 * removes from the db
val rand_question: unit -> question *)
let rand_question difficulty =
  let rand = Random.int (List.length !(List.nth !(complete_db.questions) difficulty)) in
  let q = List.nth !(List.nth !(complete_db.questions) difficulty) rand in
  let a = List.nth !(List.nth !(complete_db.answers) difficulty) rand in
  let p = List.nth !(List.nth !(complete_db.points) difficulty) rand in
  (* Removes question from db, along with points and answers *)
  List.nth !(complete_db.questions) difficulty :=
   List.filter (fun x -> if x = q then false else true)
   !(List.nth !(complete_db.questions) difficulty);
  List.nth !(complete_db.answers) difficulty :=
   List.filter (fun x -> if x = a then false else true)
   !(List.nth !(complete_db.answers) difficulty);
  List.nth !(complete_db.points) difficulty :=
   List.filter (fun x -> if x = p then false else true)
   !(List.nth !(complete_db.points) difficulty);

	{
		question = q;
		answer = a;
		point = p;
	}
