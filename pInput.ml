include Reader

let remove_non_alpha = Str.global_replace (Str.regexp "[^a-z0-9 ]+") ""

let explode = Str.split (Str.regexp " ") 

let strip (ans:string) : string =
  let exp_lower = explode (String.trim (String.lowercase ans)) in
  let clean a x = if x<>"" then a ^ (x ^ " ") else a in
	String.trim (List.fold_left clean "" (List.map remove_non_alpha exp_lower))

let timeout = ref false

let one_word_ans q a =
	let rec helper question ans =
		match ans with
		| [] -> false
		| hd::tl -> 
			(if hd = (strip question.answer) then
	    	true
	    else
		    helper question tl) in
	helper q (explode a)

(* long_ans ensures that all words in the correct answer are in the player's answer
 * i.e., the correct answer is a subset of the player's answer *)
let long_ans q a =
	let correct_ans = explode (q.answer) in
	let ans = Str.split (Str.regexp " ") a in
	List.fold_left (fun acc x -> acc && (List.mem (strip x) ans)) true correct_ans

let is_correct answer question =
	let correct_ans = explode (question.answer) in
	let correct = if (List.length correct_ans > 1) then
								  long_ans question answer
								else
									one_word_ans question answer in
	let timed_out = !timeout in
	timeout := false;
	(correct,timed_out)

