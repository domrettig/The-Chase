(*Holds all game metadata, including player and ai and current question, question pool*)
type metadata

(*Holds data about the player, score etc.*)
type player

(*Holds data about the ai, difficulty etc.*)
type ai

(*Holds data about round 2 gameboard*)
type gameboard

val send_question : unit -> bytes

(* Takes a player's input from the GUI and responds with a correct/incorrect message and the players new wallet *)
val receive_answer : bytes -> (bytes * bytes)

(*Produces a response string, (a,b) 
 * [a] is the correctness of the answer
 * [b] is true if timeout
 *)
val respond_to_answer : bool * bool -> string

(* Called by the GUI to get the player's wallet amount *)
val send_wallet : unit -> string

(* Returns the amount the player will win at the end of phase three *)
val send_total : unit -> float

(* Sets the game difficulty after player enters value into GUI *)
val receive_diff : string -> bool

(* Takes player's input, sees if category is a valid, unused category of questoin *)
val receive_cat : string -> bool

(* Called by the GUI to validate a player's input for starting position of phase two
 * Sets locations of player and chaser *)
val receive_board : string -> bool

(*Updates the amount of money in the players wallet*)
val update_wallet : float -> unit

(*Updates the amount of money in the bank*)
val update_bank : float -> unit

(*Moves the player and AI one position forward if they got the answer right*)
val update_gameboard: bool -> bool -> unit

(* Determines whether the player has been caught by the chaser *)
val caught : unit -> bool

(* Determines whether the player has reached the bank *)
val at_bank : unit -> bool

(* Takes in the player's answer
 * Returns (a,b,c,d) where a is GUI's response to answer
 * b is updated bank
 * c is whether the ai answered correctly
 * d is what the ai answered *)
val receive_head_to_head : string -> string * string * bool * string

(* Alters game data based on whether or not player completed phase two
 * Returns a string to display to the player *)
val phase_two_end : bool -> string

(* Returns a bool list, where ture represents a question the AI answered correctly
 * and false represents a question answered incorrectly *)
val send_ai_list : unit -> bool list

(* Returns string of player/chaser/bank positions *)
val receive_positions : unit -> string

