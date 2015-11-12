open Gameplay

(* For a timed question, timed_question receives an empty Ivar from Gameplay
 * After x seconds, Ivar will be filled, and the player will not be allowed
 * to enter an answer. *)
val timed_question : 'a Deferred.t -> string

(* Returns player input for answer to a trivia question *)
val get_input : unit -> string

(* Removes all punctuation and capitalization from player's input *)
val strip : string -> string

(* If the question has a one word answer, ensure that the answer is in the
 * player's response. *)
val one_word_ans : Reader.question -> string -> bool

(* Breaks down correct answer into single words and ensures that each word is
 * in the player's answer. Will call one_word_ans *)
val long_ans : Reader.question -> string -> bool

(* Given a player's response and a question, determines if they were correct. 
 * First bool represents whether or not player answered quetsion correctly
 * Second bool represents whether or not a timeout occurred *)
val is_correct : string -> Reader.question -> bool * bool
