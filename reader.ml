open Yojson.Basic.Util

(* First take the json file right as its compiled (This is for command line)*)
(* let listcommands = Array.to_list Sys.argv
let command = List.nth listcommands 1
let json = Yojson.Basic.from_file command *)

(* Gets the json file (This is for testing in utop)*)
let json = Yojson.Basic.from_file "trivia.json"


(* Extracts sets id or description (pass in option as "key") based on difficuty*)
let get_desc key difficulty json =
  member difficulty json
  |> map (member key)
  |> to_list
  |> filter_string

(* Extracts the questions or answers based of diff and returns a bytes list*)
let get_q_a key difficulty json=
 member difficulty json
  |> map (member key)
  |> to_list
  |> filter_list
  |> List.map filter_string
  |> List.map (fun x -> ref x)
(* Extracts the points based of diff and returns a bytes list*)
let get_points difficulty json =
  member difficulty json
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
  descriptions: bytes list;
  questions: bytes list ref list ref;
  answers: bytes list ref list ref;
  points: int list ref list ref;
}

(* Creates a db of the easy questions (all types) *)
let easy_db = {descriptions=get_desc "description" "easy" json;
questions=ref (get_q_a "questions" "easy" json);
answers=ref (get_q_a "answers" "easy" json);
points=ref (get_points "easy" json)}
(* Creates a db of the medium questions (all types) *)
let medium_db = {descriptions=get_desc "description" "medium" json;
questions=ref (get_q_a "questions" "medium" json);
answers=ref (get_q_a "answers" "medium" json);
 points=ref (get_points "medium" json)}
(* Creates a db of the hard questions (all types) *)
let hard_db = {descriptions=get_desc "description" "hard" json;
questions=ref (get_q_a "questions" "hard" json);
answers=ref (get_q_a "answers" "hard" json);
points=ref (get_points "hard" json)}


(* Removed item at given index of a list *)
let rec remove l index counter=
  match l with
  |[] -> failwith "Invalid index to remove"
  |h::t -> if index = !counter then t else (counter := !counter+1; h::(
  remove t index counter))

(* Produces a random question based on difficulty level
 * removes from the db
val rand_question: unit -> question *)
let rand_question difficulty category =
  (* Sets the db we will work with based on difficulty *)
  let complete_db = (match difficulty with
  | 0 -> easy_db
  | 1 -> medium_db
  | 2 -> hard_db
  | _ -> failwith "invalid difficulty") in
  (* Gets a questions from that difficulty in that category *NEED TO PASS IN A
  CATEGORY SO I INDEX CORRECTLY RIGHT NOW ITS DUMB * *)
  Random.init(int_of_float(Unix.gettimeofday ()));
  let rand = Random.int (List.length !(List.nth !(complete_db.questions) category)) in
  let q = List.nth !(List.nth !(complete_db.questions) category) rand in
  let a = List.nth !(List.nth !(complete_db.answers) category) rand in
  let p = List.nth !(List.nth !(complete_db.points) category) rand in
  (* Removes question from db, along with points and answers *)
  List.nth !(complete_db.questions) category :=
   remove (!(List.nth !(complete_db.questions) category)) rand (ref 0);
  List.nth !(complete_db.answers) category :=
   remove (!(List.nth !(complete_db.answers) category)) rand (ref 0);
  List.nth !(complete_db.points) category :=
   remove (!(List.nth !(complete_db.points) category)) rand (ref 0);
  (*Returns a random question*)
	{
		question = q;
		answer = a;
		point = p;
	}
