(* open Async.Std *)

(*Holds all game metadata, including player and ai and current question, question pool*)
type metadata

(*Holds data about the player, score etc.*)
type player

(*Holds data about the ai, difficulty etc.*)
type ai

(*Holds data about round 2 gameboard*)
type gameboard

(*Defines the types of actor in the game, the ai and the player*)
type actor

(*Returns the current question served to the player*)
val serve_question : unit -> unit

(*Returns a bool if the passed in question has a single word as an answer*)
val is_one_word_ans : Reader.question -> bool

(*Returns a bool if the passed in question has multiple words as an answer*)
val is_mult_word_ans : Reader.question -> bool

(*Returns a deffered that will be determined when time runs out*)
(* val get_timer : float -> 'a Deferred.t *)

(*Produces a response string, (a,b) 
 * [a] is the correctness of the answer
 * [b] is true if timeout
 *)
val respond_to_answer : bool * bool -> unit

(*Updates the amount of money in the players wallet*)
val update_wallet : float -> unit

(*Handles running phase one of the game, the one minute round/two minute round*)
val phase_one : int -> unit

(*Updates the amount of money in the bank*)
val update_bank : float -> unit

(*Moves the actor one position forward on the game board*)
val update_position: actor -> unit

(*Determines whether the player has been caught by the chaser*)
val caught : gameboard -> bool

(*Handles the chase round, 8 questions*)
val phase_two : unit -> unit

(*Handles optional round 3, a hybrid of the previous rounds*)
val phase_three : unit -> unit

(*The main game loop, takes in data from multiple sources and updates gui, ai, and game state appropriately*)
val game_loop : unit -> unit

