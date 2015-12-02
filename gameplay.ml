include Reader
include PInput
include Ai

type player = {
	mutable wallet: float;
	mutable bank: float;
	mutable wager: float;	
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
	difficulty: int;
	mutable curr_question: Reader.question option;
	mutable gameboard: gameboard;
}

let player = {
	wallet = 0.;
	bank = 0.;
	wager = 0.;
}

let ai = {
	difficulty = 0;
}

let gameboard = {
	size = 14;
	player_pos = 0;
	chaser_pos = 0;
}

let test_metadata = {
	player=player;
	ai = ai;
	difficulty = 0;
	curr_question = None;
	gameboard = gameboard;
}

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
	let new_val = n +. test_metadata.player.wallet in
	test_metadata.player.wallet <- new_val

let serve_question () =
  let q = Reader.rand_question test_metadata.difficulty in
  test_metadata.curr_question <- Some q;
  display_question q;
  let ans = PInput.get_input () in 
  let (correct, timeout) = PInput.is_correct ans q in
  respond_to_answer (correct, timeout);
  (if correct && not timeout then
    update_wallet (float_of_int q.point) 
  else ());
  Printf.printf "Your Wallet: %f\n" test_metadata.player.wallet

let phase_one () =
	Printf.printf "Welcome to The Chase!\nFor this phase, answer as many questions as you can in 90 seconds. Press enter when you're ready.";
	let _ = read_line () in
	let start_time = Unix.gettimeofday () in
	let rec body () = 
		let curr_time = Unix.gettimeofday () in
		if (curr_time -. start_time) <= 90. then begin
			serve_question ();
			body ()
		end
		else
			Printf.printf "Time's up!\n" in
	body ()

let update_bank n =
	failwith "Unimplemented"

let update_position a =
	failwith "Unimplemented"

let caught g = 
	failwith "Unimplemented"

let rec init_gameboard () = 
  Printf.printf "Please select an option to begin Round 2\n";
  Printf.printf "(A) Start 4 spots away from the bank and wager 1/2 of the money in your wallet\n";
  Printf.printf "(B) Start 5 spots away from the bank and wager 2/3 of the money in your wallet\n";
  Printf.printf "(C) Start 6 spots away from the bank and wager all of the money in your wallet\n";
  let ans = PInput.get_input () in
  let curr_wallet = test_metadata.player.wallet in
  match ans with
  | "a" -> 
    test_metadata.gameboard.player_pos <- 10;
    test_metadata.gameboard.chaser_pos <- 2;
    test_metadata.player.wager <- (curr_wallet *. 0.5);
    test_metadata.player.wallet <- (curr_wallet -. test_metadata.player.wager)
  | "b" ->
    test_metadata.gameboard.player_pos <- 9;
    test_metadata.gameboard.chaser_pos <- 1;
    test_metadata.player.wager <- (curr_wallet *. (2./.3.));
    test_metadata.player.wallet <- (curr_wallet -. test_metadata.player.wager)
  | "c" ->
  	test_metadata.gameboard.player_pos <- 8;
    test_metadata.gameboard.chaser_pos <- 0;
    test_metadata.player.wager <- (curr_wallet);
    test_metadata.player.wallet <- 0.
  |  _  -> Printf.printf "That is not a valid selection."; init_gameboard ()

let phase_two () = 
	failwith "Unimplemented"

let phase_three () =
	failwith "Unimplemented"

let game_loop () =
	failwith "Unimplemented"

