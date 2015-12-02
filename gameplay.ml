include Reader
include PInput

type player = {
	mutable wallet: int;
	mutable bank: int	
}

type ai = {
	difficulty:int;
}

type gameboard = {
	mutable pos:int;
}

type actor = P of player | A of ai

type metadata = {
	mutable player: player;
	mutable ai: ai;
	difficulty: int;
	mutable curr_question: Reader.question option;
}

let player = {
	wallet = 0;
	bank = 0;
}

let ai = {
	difficulty = 0;
}

let test_metadata = {
	player=player;
	ai = ai;
	difficulty = 0;
	curr_question = None;
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
	let new_val = n + test_metadata.player.wallet in
	test_metadata.player.wallet <- new_val

let serve_question () =
  let q = Reader.rand_question test_metadata.difficulty in
  test_metadata.curr_question <- Some q;
  display_question q;
  let ans = PInput.get_input () in 
  let (correct, timeout) = PInput.is_correct ans q in
  respond_to_answer (correct, timeout);
  (if correct && not timeout then
    update_wallet q.point 
  else ());
  Printf.printf "Your Wallet: %d\n" test_metadata.player.wallet

let phase_one () =
	Printf.printf "Welcome to The Chase!\nFor this phase, answer as many questions as you can in 90 seconds. Press enter when you're ready.\n";
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

let phase_two () = 
	failwith "Unimplemented"

let phase_three () =
	failwith "Unimplemented"

let game_loop () =
	failwith "Unimplemented"

