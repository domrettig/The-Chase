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

let get_points json =
  member "sets" json
  |> map (member "points")
  |> to_list
  |> filter_list
  |> List.map filter_int


(*A type representing a single question*)
type question = {
  question: bytes;
  answer: bytes list;
  point: int;
  }

(*Represents the entire library of questions at a given difficulty level*)
type question_db = {
  id: bytes;
  description: bytes;
  questions: bytes list;
  answers: bytes list;
  points: int list;
}

(* Represents the data base of all difficulty levels *)
type whole_db = {
  all_id: bytes list;
  all_description: bytes list;
  all_questions: bytes list list;
  all_answers: bytes list list;
  all_points: int list list;
}

let complete_db = {all_id=get_id_desc "id" json;
all_description=get_id_desc "description" json;
all_questions=get_q_a "questions" json; all_answers=get_q_a "answers" json;
all_points=get_points json}


(*Builds question db based on the requested difficulty level*)
let parse_questions difficulty =
  if difficulty > 2 then
    failwith "Thats an invalid difficulty level!"
  else
    {id = List.nth complete_db.all_id difficulty;
    description = List.nth complete_db.all_description difficulty;
    questions = List.nth complete_db.all_questions difficulty;
    answers = List.nth complete_db.all_answers difficulty;
    points = List.nth complete_db.all_points difficulty}

(* Produces a random question based on difficulty level
 * removes from the db
val rand_question: unit -> question *)
let rand_question () =
	{
		question = List.nth (List.nth complete_db.all_questions 0) 0;
		answer = [List.nth (List.nth complete_db.all_answers 0) 0];
		point = 0;
	}
