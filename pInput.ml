let get_input () = 
	Printf.printf "Answer: ";
	read_line()

let one_word_ans q a =
	let helper question ans =
		match ans with
		| [] -> false
		| hd::tl -> 
			(if hd = (List.hd question.answer) then
	    	true
	    else
		    one_word_ans question tl) in
	helper q (Str.split (Str.regexp " ") a)