type probability

type dictionary

(** Taked a difficulty as input (int 1, int 2,int 3 respectively)
returns the range of probability of making a correct guess*)
(* val difficulty : int -> probability *)

(* This determines the correctness of he AI's response *)
val ai_is_correct : int -> bool

(* Returns the ai's answer *)
val ai_answer : bool -> Reader.question -> string
