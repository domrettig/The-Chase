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
    else if correct then
           Printf.printf "That is correct!\n"
         else Printf.printf "That is incorrect!\n"

let serve_question () =
  let q = Reader.rand_question () in
  test_metadata.curr_question <- Some q;
  display_question q;
  let ans = PInput.get_input () in 
  let (correct, timeout) = PInput.is_correct ans q in
  respond_to_answer (correct, timeout)

let update_wallet n = 
	player.wallet <- n

let phase_one i =
	failwith "Unimplemented"

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

