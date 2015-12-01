(*Reads in a json question file*)

open Yojson.Basic.Util

(* First take the json file right as its compiled *)
let listcommands = Array.to_list Sys.argv
let command = List.nth listcommands 1
let json = Yojson.Basic.from_file command












(*A type representing a single question*)
type question = {
  question: bytes;
  answer: bytes list;
  point: int
  }

(*Represents the entire library of questions at a given difficulty level*)
type question_db = {
  id: bytes;
  description: bytes;
  questions: bytes list;
  answers: bytes list;
  points: int list
}

(*Builds question db, only retrieves questions based on
 * difficulty level *)
val parse_questions : int -> question_db

(*Produces a random question based on difficulty level
 * removes from the db *)
val rand_question: unit -> question