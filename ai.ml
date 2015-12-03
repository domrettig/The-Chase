include Reader

type probability = float list

type dictionary = string list

(* Get a new seed *)
let seed () =
	Random.self_init();;

let gen_dict () : dictionary =
	(* FROM: http://camltastic.blogspot.com/2008/09/tip-read-all-lines-from-file-most.html *)
	let lines = ref [] in
	let chan = open_in "/usr/share/dict/web2" in
	try
  		while true; do
			lines := input_line chan :: !lines
  		done; []
	with End_of_file ->
  		close_in chan;
  		List.rev !lines

let dict : dictionary = gen_dict ()

(* [difficult i] converts the input to a probability range
 * 0<=i<=2 for easy, medium, hard difficulties *)
let difficulty (i:int) : probability =
	if i = 0 then
		[0.4;0.6]
	else if i = 1 then
		[0.6;0.8]
	else
		[0.8;1.0]

let ai_is_correct i =
	seed ();
	let range = difficulty i in
	let prob = (Random.float 0.2) +. (List.hd range) in
	(Random.float 1.0) < prob

let ai_answer correct q =
	seed ();
	if correct then
		q.answer
	else
		List.nth dict (Random.int (List.length dict))