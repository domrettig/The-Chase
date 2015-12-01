let get_input () = 
	Printf.printf "Answer: ";
	read_line()

let timed_question (time:float) : string =
  let start_time = Unix.gettimeofday () in 
  let input = get_input () in
  let end_time = Unix.gettimeofday () in
  if (end_time-.start_time) > time then
    ""
  else input


let remove_non_alpha = Str.global_replace (Str.regexp "[^a-z ]+") ""

let explode = Str.split (Str.regexp " ") 

let strip (ans:string) : string =
  let exp_lower = explode (String.trim (String.lowercase ans)) in
  let clean a x = if x<>"" then a ^ (x ^ " ") else a in
    String.trim (List.fold_left clean "" (List.map remove_non_alpha exp_lower))

let one_word_ans q a =
	let rec helper question ans =
		match ans with
		| [] -> false
		| hd::tl -> 
			(if hd = (List.hd question.answer) then
	    	true
	    else
		    helper question tl) in
	helper q (Str.split (Str.regexp " ") a)

(* long_ans ensures that all words in the correct answer are in the player's answer
 * i.e., the correct answer is a subset of the player's answer *)
let long_ans q a =
	let ans = Str.split (Str.regexp " ") a in
	List.fold_left (fun acc x -> acc && (List.mem x ans)) true (q.answer)

