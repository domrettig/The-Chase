(* open Async.Std *)

(*Holds all game metadata, including player and ai and current question, question pool*)
type metadata

(*Holds data about the player, score etc.*)
type player

(*Holds data about the ai, difficulty etc.*)
type ai

(*Holds data about round 2 gameboard*)
type gameboard

(*#############################################*)

val send_question : unit -> bytes

val receive_answer : bytes -> (bytes * bytes)

(*#############################################*)
(*Returns the current question served to the player*)
val serve_question : unit -> unit

(*Returns a deffered that will be determined when time runs out*)
(* val get_timer : float -> 'a Deferred.t *)

(*Produces a response string, (a,b) 
 * [a] is the correctness of the answer
 * [b] is true if timeout
 *)
val respond_to_answer : bool * bool -> string

val receive_diff : string -> bool

val receive_cat : string -> bool

(*Updates the amount of money in the players wallet*)
val update_wallet : float -> unit

(*Handles running phase one of the game, the one minute round/two minute round*)
val phase_one : unit -> unit

(*Updates the amount of money in the bank*)
val update_bank : float -> unit

(*Moves the player and AI one position forward if they got the answer right*)
val update_gameboard: bool -> bool -> unit

(*Determines whether the player has been caught by the chaser*)
val caught : gameboard -> bool

val init_gameboard : unit -> unit

(*Handles the chase round, 8 questions*)
val phase_two : unit -> unit

(*Handles optional round 3, a hybrid of the previous rounds*)
val phase_three : unit -> unit

(*The main game loop, takes in data from multiple sources and updates gui, ai, and game state appropriately*)
val game_loop : unit -> unit

