open Yojson.Basic.util

(* Takes in the players question, and answer as well as the
difficulty of said question and returns *)
val parse_input : string -> string -> int -> Yojson.Basic.json

(* Appends the new question to the set of trivia questions *)
val write_to_file : Yojson.Basic.json -> unit