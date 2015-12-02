(*Reads in a json question file*)

open Yojson.Basic.Util

(*A type representing a single question*)
type question = {
	question: bytes;
	answer: bytes;
	point: int;
}

(*Represents the entire library of questions at a given difficulty level*)
type question_db

(*Produces a random question based on difficulty leve
 * removes from the db *)
val rand_question: int -> question