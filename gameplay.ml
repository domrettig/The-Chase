include Reader

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


let get_served_question () : Reader.question =
	let q = Reader.rand_question () in
	test_metadata.curr_question <- Some q;
	q

let display_question (q:Reader.question option) : unit = 
	match q with
	| None -> failwith "No question in metadata"
	| Some x -> Printf.printf "Question: %s\n" x.question

let is_one_word_ans q =
	failwith "Unimplemented"

let is_mult_word_ans q =
	failwith "Unimplemented"

let respond_to_answer p =
	failwith "Unimplemented"

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

