include Reader

type probability = float list

let difficulty i =
	if i == 0 then
		[0.4;0.6]
	else if i == 1 then
		[0.6;0.8]
	else
		[0.8;1.0]

let ai_is_correct p q =
	let prob = (Random.float 0.2) +. (List.hd p) in
	(Random.float 1.0) < prob

let ai_answer correct q =
	if correct then
		q.answer
	else
		""