include Gameplay_mock
open Lwt_react
open Lwt
open LTerm_widget

let time = ref 91
let answer_ref = ref ""

let get_time () =
  time := (!time - 1);
  Printf.sprintf "%d" !time


let main () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let clock = new label (get_time ()) in
  let bank = new label "$" in
  let question = new label (Gameplay_mock.serve_question ()) in
  let answer = new label ("Answer: ") in
  let game_response = new label "" in
  let button = new button "Press esc to exit" in
  let submit = new button "Press enter to submit" in
  
  let update_label () = 
  	let (response, new_bank) = Gameplay_mock.receive_answer (answer#text) in
  	game_response#set_text response;
  	bank#set_text ("$" ^ new_bank);
  	answer#set_text "Answer: ";
  	question#set_text (Gameplay_mock.serve_question ()) in

  let show_answer s = s >|= Zed_utf8.singleton >>= fun x -> (answer#set_text (answer#text ^ x); return ()) in 

  let backspace () =
    let curr_text = answer#text in
    let to_remove = String.get curr_text ((String.length curr_text) - 1) in
    if to_remove<>':' then
      answer#set_text (String.sub curr_text 0 ((String.length curr_text) -1))
    else () in



  let handle_evt evt = 
  	match evt with
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape } -> wakeup wakener (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter }  -> update_label (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch} -> ignore (show_answer (return ch)); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Backspace} -> backspace (); true
    | _ -> false in

  vbox#add clock;
  vbox#add button;
  vbox#add submit;
  vbox#add bank;
  vbox#add question;
  vbox#add answer;
  vbox#add game_response;

  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> clock#set_text (get_time ())));
  (* ignore (Lwt_engine.on_timer 0.2 true (fun _ -> show_answer ())); *)
  (* Quit when the exit button is clicked. *)
  (* button#on_click (fun () -> wakeup_exn wakener Exit); *)


  button#on_event handle_evt;
  submit#on_event handle_evt;

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let () = Lwt_main.run (main ())