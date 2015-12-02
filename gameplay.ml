include Reader
include PInput

type player = {
	score:int;
	wallet: int;	
}

type ai = {
	difficulty:int;
}

type gameboard = {
	pos:int;
}

type actor = P of player | A of ai

type metadata = {
	mutable player: player;
	mutable ai: ai;
	difficulty: int;
	mutable curr_question: Reader.question option;
}

let test_metadata = {
	player={score=0; wallet=0;};
	ai = {difficulty=0;};
	difficulty = 0;
	curr_question = None;
}

let display_question (q:Reader.question) : unit = 
	Printf.printf "Question: %s\n" q.question


let is_one_word_ans q =
	failwith "Unimplemented"

let is_mult_word_ans q =
	failwith "Unimplemented"

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
	failwith "Unimplemented"

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

