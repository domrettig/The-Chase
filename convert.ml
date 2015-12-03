open Extlib
open Yojson.Basic
open Yojson.Basic.Util

(*Reads a file into a list, each line is an element*)
let read_file filename =
  let chan = open_in filename in
  Std.input_list chan

let check_for_start (lines: bytes list) : bool = 
	match lines with
	| [] -> false
	| h::t -> h="##START##"

let handle_diff (line:bytes) : bytes = 
	match line with
	| "DIFF: EASY"   -> "easy"
	| "DIFF: MEDIUM" -> "medium"
	| "DIFF: HARD"   -> "hard"
	| _			     -> failwith "Invalid difficulty"

let handle_cat (line:bytes) : bytes = 
	match line with
	| "CAT: HISTORY" -> "History"
	| "CAT: SCIENCE" -> "Science"
	| "CAT: MATH"    -> "Math"
	| _              -> failwith "Invalid category"

let handle_q_or_a (line:bytes) : bytes =
	let colon = String.index line ':' in
	String.sub line (colon + 2) ((String.length line) - (colon + 2))

let handle_pts (line:bytes) : bytes =
	let pt_str = handle_q_or_a line in
	pt_str

let rec single_question (lines:bytes list) (num:int) : bytes list = 
	match lines with
	| [] -> []
	| h::t -> 
		(match num with
		| 0 -> (handle_diff h) :: (single_question t (num + 1))
		| 1 -> (handle_cat h) :: (single_question t (num + 1))
		| 2 -> (handle_q_or_a h) :: (single_question t (num + 1))
		| 3 -> (handle_q_or_a h) :: (single_question t (num + 1))
		| 4 -> (handle_pts h) :: []
		| _ -> failwith "Uh oh")

let append_str_json (l:json) (n:bytes) = 
  let json_list = to_list l in
  (`List(json_list @ [`String(n)]))

let append_int_json (l:json) (n:int) =
  let json_list = to_list l in
  (`List(json_list @ [`Int(n)]))

let parse_input q a d =
  failwith "Unimplemented"

let write_to_file json = 
  failwith "Unimplemented"