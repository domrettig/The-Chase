type probability

(** Taked a difficulty as input (int 1, int 2,int 3 respectively)
returns the range of probability of making a correct guess*)
val difficulty : int -> probability

(* This determines the correctness of he AI's response *)
val is_correct : probability -> Reader.question -> bool
