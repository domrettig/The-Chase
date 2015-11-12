(*Reads in a json question file*)

open Yojson.Basic.Util

(*A type representing a single question*)
type question

(*Represents the entire library of questions at a given difficulty level*)
type question_db

(*Builds question db, only retrieves questions based on 
 * difficulty level *)
val parse_questions : int -> question_db

(*Produces a random question based on difficulty leve
 * removes from the db *)
val rand_question: unit -> question