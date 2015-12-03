include Reader
include PInput
include Ai

type player = {
	mutable wallet: float;
	mutable bank: float;
	mutable wager: float;
	mutable num_right: int
}

type ai = {
	difficulty:int;
}

type gameboard = {
	size:int;
	mutable player_pos:int;
	mutable chaser_pos:int;
}

type actor = P of player | A of ai

type metadata = {
	mutable player: player;
	mutable ai: ai;
	mutable difficulty: int;
	mutable curr_cat: int;
	mutable used_cat: int list;
	mutable gameboard: gameboard;
}

let player = {
	wallet = 0.;
	bank = 0.;
	wager = 0.;
	num_right = 0
}

let ai = {
	difficulty = 0;
}

let gameboard = {
	size = 14;
	player_pos = 0;
	chaser_pos = 0;
}

let metadata = {
	player=player;
	ai = ai;
	difficulty = 0;
	curr_cat = 0;
	used_cat = [];
	gameboard = gameboard;
}

(*##############################################################*)

let test_num = ref 0
let test_bank = ref 0

let send_question () = 
	test_num := (!test_num + 1);
	"Sample Question " ^ (string_of_int !test_num) ^ "\n"

let receive_answer a = 
	Printf.printf "Answer receieved: %s\n" a;
	test_bank := (!test_bank + 10);
	("Thats correct!", string_of_int(!test_bank))


(*##############################################################*)
let categories = [(0, "Science");(1, "History");(2, "Math")]

let display_question (q:Reader.question) : unit = 
	Printf.printf "Question: %s\n" q.question

let respond_to_answer p =
  match p with
  | (correct, timeout) -> 
    if timeout then
      Printf.printf "You timed out!\n"
    else (if correct then
           Printf.printf "That is correct!\n"
         else Printf.printf "That is incorrect!\n")

let update_wallet n = 
	let new_val = n +. metadata.player.wallet in
	metadata.player.wallet <- new_val

let serve_question () =
  let q = Reader.rand_question metadata.difficulty metadata.curr_cat in
  display_question q;
  let ans = PInput.get_input () in 
  let (correct, timeout) = PInput.is_correct ans q in
  respond_to_answer (correct, timeout);
  (if correct && not timeout then begin
  	metadata.player.num_right <- metadata.player.num_right + 1;
    update_wallet (float_of_int q.point)
  end
  else ());
  Printf.printf "Your Wallet: %f\n" metadata.player.wallet

let rec get_difficulty () =
	Printf.printf "Do you want to play on easy, medium, or hard? ";
	let diff = read_line () in
	match (String.lowercase diff) with
	| "easy" 		-> metadata.difficulty <- 0
	| "medium" 	-> metadata.difficulty <- 1
	| "hard" 		-> metadata.difficulty <- 2
	| _ 				-> Printf.printf "Not a difficulty.\n"; get_difficulty ()

let rec get_available_cat (left: (int * bytes) list) =
	match left with
	| [] -> ""
	| h::t -> (match h with
				| (id, name) -> if not (List.mem id metadata.used_cat) then
								  name ^ "\n" ^ (get_available_cat t)
								else (get_available_cat t))

let rec get_category () =
	Printf.printf "What category of questions do you want to answer?\n";
	Printf.printf "%s" (get_available_cat categories);
	let cat = read_line () in
	match (String.lowercase cat) with
	| "science" -> 
	  if List.mem 0 metadata.used_cat then begin
	    Printf.printf "You have already used the category Science.\n"; get_category ()
	  end
	  else 
	    metadata.used_cat <- (metadata.used_cat @ [0]);
	    metadata.curr_cat <- 0
	| "history" ->
	  if List.mem 1 metadata.used_cat then begin
	    Printf.printf "You have already used the category History.\n"; get_category ()
	  end
	  else 
	    metadata.used_cat <- (metadata.used_cat @ [1]);
	    metadata.curr_cat <- 1
	| "math" 		-> 
	  if List.mem 2 metadata.used_cat then begin 
	    Printf.printf "You have already used the category Math.\n"; get_category ()
	  end
	  else 
	    metadata.used_cat <- (metadata.used_cat @ [2]);
	    metadata.curr_cat <- 2
	| _ 				-> Printf.printf "Not a valid category.\n"; get_category ()

let phase_one () =
	get_difficulty ();
	get_category ();
	let start_time = Unix.gettimeofday () in
	let rec body () = 
		let curr_time = Unix.gettimeofday () in
		if (curr_time -. start_time) <= 10. then begin
			serve_question ();
			body ()
		end
		else
			Printf.printf "Time's up!\n" in
	body ()

let update_bank n =
	let new_val = metadata.player.bank +. n in
	metadata.player.bank <- new_val

let update_gameboard player_right ai_right =
	match player_right, ai_right with
	| true, true   -> metadata.gameboard.player_pos <- metadata.gameboard.player_pos + 1;
					  				metadata.gameboard.chaser_pos <- metadata.gameboard.chaser_pos + 1
	| true, false  -> metadata.gameboard.player_pos <- metadata.gameboard.player_pos + 1
	| false, true  -> metadata.gameboard.chaser_pos <- metadata.gameboard.chaser_pos + 1
	| false, false -> ()

let caught g = 
	failwith "Unimplemented"

let rec init_gameboard () = 
  Printf.printf "Please select an option to begin Round 2\n";
  Printf.printf "(A) Start 4 spots away from the bank and wager 1/2 of the money in your wallet\n";
  Printf.printf "(B) Start 5 spots away from the bank and wager 2/3 of the money in your wallet\n";
  Printf.printf "(C) Start 6 spots away from the bank and wager all of the money in your wallet\n";
  let ans = PInput.get_input () in
  let curr_wallet = metadata.player.wallet in
  match ans with
  | "a" -> 
    metadata.gameboard.player_pos <- 10;
    metadata.gameboard.chaser_pos <- 2;
    metadata.player.wager <- (curr_wallet *. 0.5);
  | "b" ->
    metadata.gameboard.player_pos <- 9;
    metadata.gameboard.chaser_pos <- 1;
    metadata.player.wager <- (curr_wallet *. (2./.3.));
  | "c" ->
  	metadata.gameboard.player_pos <- 8;
    metadata.gameboard.chaser_pos <- 0;
    metadata.player.wager <- (curr_wallet);
  |  _  -> Printf.printf "That is not a valid selection."; init_gameboard ()

let rec head_to_head_question () =
	(* Get question, display, and update metadata *)
	let q = Reader.rand_question metadata.difficulty metadata.curr_cat in
	display_question q;

	(* Get the player's and ai's answers *)
	let ans = PInput.timed_question 10. in
	let ai_correct = Ai.ai_is_correct metadata.difficulty in
	let ai_ans = Ai.ai_answer ai_correct q in
	Printf.printf "The AI answered %s\n" ai_ans;
	Printf.printf "The correct answer is %s\n" q.answer;

	let (p_correct,p_timeout) = PInput.is_correct ans q in
	respond_to_answer (p_correct,p_timeout);
	update_gameboard p_correct ai_correct;
	finished ()

and finished () =
	(* If the player is at the bank *)
	if metadata.gameboard.player_pos = metadata.gameboard.size then begin
		Printf.printf "YOU WIN!\n";
		Printf.printf "You sucessfully banked $%f!\n" metadata.player.wager;
		update_bank metadata.player.wager
	end
	(* If the Chaser caught the player *)
	else if metadata.gameboard.player_pos = metadata.gameboard.chaser_pos then begin
		Printf.printf "Sorry, the chaser caught you\n";
		metadata.player.wallet <- (metadata.player.wallet -. metadata.player.wager);
		metadata.player.wager <- 0.
	end
	else
		head_to_head_question ()

let phase_two () = 
	init_gameboard ();
	get_category ();
	Printf.printf "These are timed questions. You have 10 seconds to answer.\n";
	head_to_head_question ();
	if metadata.player.wallet = 0. then
		Printf.printf "You're out of money! Game over\n"
	else
		Printf.printf "On to Phase Three!\n"

let ai_answer_questions () =
	let num_seen = 90 / ((Random.int 6) + 5) in
	let rec create_list l i =
		if i = 0 then []
		else (Ai.ai_is_correct metadata.difficulty)::(create_list l (i-1)) in
	create_list [] num_seen

let rec show_answers count = function
	| []        -> ()
	| true::tl  -> Printf.printf "The Chaser answered a question correctly.\n";
								 count := !count + 1;
								 show_answers count tl
	| false::tl -> Printf.printf "The Chaser answered incorrectly. Your turn to try and answer.\n";
								 let q = Reader.rand_question metadata.difficulty metadata.curr_cat in
							   display_question q;
							   let ans = PInput.get_input () in 
							   let (correct, timeout) = PInput.is_correct ans q in
							   respond_to_answer (correct, timeout);
							   (if correct then metadata.player.num_right <- metadata.player.num_right + 1
							 	 else ());
								 show_answers count tl

let phase_three () =
	metadata.player.num_right <- 0;
	phase_one ();
	Printf.printf "%d\n" metadata.player.num_right;
	let counter = ref 0 in
	show_answers counter (ai_answer_questions ());
	if metadata.player.num_right > !counter then
	  Printf.printf "YOU WIN MOTHERFUCKER\n"
	else
		Printf.printf "YOU LOSE MOTHERFUCKER\n"

let game_loop () =
	Printf.printf "Welcome to The Chase!\n";
	Printf.printf "For this phase, answer as many questions as you can in 90 seconds.\n";
	phase_one ();
	phase_two ();
	phase_three ()
