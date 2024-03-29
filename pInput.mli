val timeout : bool ref

(* Removes all punctuation and capitalization from player's input *)
val strip : string -> string

(* If the question has a one word answer, ensure that the answer is in the
 * player's response. *)
val one_word_ans : Reader.question -> string -> bool

(* Breaks down correct answer into single words and ensures that each word is
 * in the player's answer.
 * i.e., the correct answer is a subset of the player's answer  *)
val long_ans : Reader.question -> string -> bool

(* Given a player's response and a question, determines if they were correct. 
 * First bool represents whether or not player answered quetsion correctly
 * Second bool represents whether or not a timeout occurred *)
val is_correct : string -> Reader.question -> bool * bool
