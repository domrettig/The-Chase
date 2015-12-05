include Gameplay
open Lwt_react
open Lwt
open LTerm_widget

let time = ref 11
let answer_ref = ref ""

let get_time () =
  time := (!time - 1);
  if !time>0 then
    Printf.sprintf "%d" !time
  else Printf.sprintf "%s" "Times up!"

let round_three () =
  let waiter, wakener = wait () in

  let r3_time = ref 11 in

  let ai_round = ref false in

  let ai_wrong = ref false in 

  let unlock_answer = ref false in

  let count = ref 0 in

  let player_right = ref 0 in

  let ai_list = ref (Gameplay.send_ai_list ()) in 

  let get_time_three () = 
    r3_time := (!r3_time - 1);
    if !r3_time>0 then
      Printf.sprintf "%d" !r3_time
    else Printf.sprintf "%s" "Times up!" in

  let vbox = new vbox in
  let clock = new label (get_time_three ()) in
  let bank = new label (Gameplay.send_wallet ()) in
  let question = new label (Gameplay.send_question ()) in
  let answer = new label ("Answer:") in
  let game_response = new label "" in
  let button = new button "Press esc to exit" in
  let submit = new button "Press enter to submit" in

  let end_game () = 
    let end_text = "You answered " ^ (string_of_int !player_right) ^
    " correctly\n The Chaser answered " ^ (string_of_int !count) ^ " correctly" in
    question#set_text end_text;
    if (!player_right > !count) then begin
      answer#set_text ("You beat The Chaser!\n You won " ^ (string_of_float (Gameplay.send_total ())));
      exit 0
    end
    else 
      answer#set_text "The Chaser caught you! You LOSE";
      exit 0 in



  let rec ai_answers () = 
    (match !ai_list with
    | []        -> end_game ()
    | true::tl  -> question#set_text "The Chaser answered a question correctly. Press Enter to continue";
                   count := !count + 1;
                   ai_list := tl
    | false::tl -> ai_wrong := true; 
                  question#set_text "The Chaser answered incorrectly. Your turn to try and answer. Press Enter to see the question";
                  ai_list := tl ) in

  let update_label () =
    if !ai_round then begin answer#set_text "Answer:"; ai_round := false; (ai_answers ()) end else
    if !r3_time <= 0 && !ai_wrong then begin
      question#set_text (Gameplay.send_question ());
      ai_wrong := false
    end
    else if !r3_time <= 0 && not !ai_wrong then
      let (response, new_bank) = Gameplay.receive_answer (answer#text) in
      game_response#set_text response;
      (if response="That is correct!\n" then
        player_right := !player_right + 1
      else ());
      bank#set_text ("$" ^ new_bank);
      answer#set_text "Answer:";
      clock#set_text "Times up!\nThe Chaser is now answering questions";
      ai_answers ()
    else
      let (response, new_bank) = Gameplay.receive_answer (answer#text) in
      game_response#set_text response;
      (if response="That is correct!\n" then
        player_right := !player_right + 1
      else ());
      bank#set_text ("$" ^ new_bank);
      answer#set_text "Answer:";
      question#set_text (Gameplay.send_question ()) in

  let show_answer s =
    s >|= Zed_utf8.singleton >>=
    fun x ->
      (* (answer#set_text (answer#text ^ x); return ()) in  *)
      (let new_text = if !r3_time<=0 && not !unlock_answer then begin 
        unlock_answer := true; 
        ai_round := true;
        vbox#remove clock;
        r3_time := 1;
        "Out of time! Press Enter to start The Chaser round" 
        end 
        else (answer#text ^ x) in
        answer#set_text new_text; return ()) in

  let backspace () =
    if !r3_time<= 0 then
      ()
    else
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
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> clock#set_text (get_time_three ())));
  (* ignore (Lwt_engine.on_timer 0.2 true (fun _ -> show_answer ())); *)
  (* Quit when the exit button is clicked. *)
  (* button#on_click (fun () -> wakeup_exn wakener Exit); *)


  button#on_event handle_evt;
  submit#on_event handle_evt;

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter



let round_three_prescreen () = 
  let waiter, wakener = wait () in

  let expl_text = "In this round you will answer questions from your selected category.\n You will have two minutes " ^
  "to answer as many as you possibly can.\n Each correct answer will move you one space away from The Chaser\n " ^
  "After two minutes, The Chaser will have two minutes to answer questions from a different category\n" ^
  "Each correct answer moves him one space closer, but an incorrect answer will give you the opportunity\n to answer " ^
  "and possibly advance by one extra space.\n If time runs out, you keep all the money in your wallet, but if he catches " ^
  "you, its GAME OVER" in 

  let vbox = new vbox in
  let title = new label "Round 3" in
  let expl = new label expl_text in
  let ins = new label "Press Enter" in
  let input = new label "" in

  let cat_set = ref false in

  let update_info () =
    if input#text <> "" && not !cat_set then begin
      cat_set := Gameplay.receive_cat (input#text);
      (if not !cat_set then
        ins#set_text ("That is not a valid category or has already been used.\n Please select from Science, History, Music or Geography")
      else begin
        ins#set_text ("Ready to begin. Press Enter.");
        input#set_text "" end)
    end
    else (if not !cat_set then
      ins#set_text ("Please choose a category: Science, History, Music, or Geography")
    else ignore(Lwt_main.run (round_three ()))) in


  let show_input s =
    s >|= Zed_utf8.singleton >>=
    fun x -> input#set_text (input#text ^ x); return () in


  let backspace () =
    let curr_text = input#text in
    if curr_text <> "" then
      input#set_text (String.sub curr_text 0 ((String.length curr_text) -1))
    else () in

  let handle_evt evt =
    match evt with
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape } -> wakeup wakener (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter }  -> update_info (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch} -> ignore (show_input (return ch)); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Backspace} -> backspace (); true
    | _ -> false in

  vbox#add title;
  vbox#add expl;
  vbox#add ins;
  vbox#add input;

  vbox#on_event handle_evt;

  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter




let round_two () =
  let waiter, wakener = wait () in

  let time = ref 11 in

  let reset_time () =
    time := 11 in

  let new_get_time () =
    time := (!time - 1);
    if !time>0 then
      Printf.sprintf "%d" !time
    else Printf.sprintf "%s" "Times up!" in

  let vbox = new vbox in
  let clock = new label (new_get_time ()) in
  let question = new label (Gameplay.send_question ()) in
  let positions = new label (Gameplay.receive_positions()) in
  let answer = new label ("Answer: ") in
  let game_response = new label "" in
  let button = new button "Press esc to exit" in
  let submit = new button "Press enter to submit" in

  let update_label () =
    let places = Gameplay.receive_positions () in
    positions#set_text places;
    if Gameplay.caught () then
      let response = Gameplay.phase_two_end false in
      answer#set_text ("You've been caught by the chaser! " ^ response);
      vbox#remove clock;
      ignore(Lwt_main.run (round_three_prescreen ()))
    else if Gameplay.at_bank () then
      let response = Gameplay.phase_two_end true in
      answer#set_text ("You've made it safely to the bank! " ^ response);
      vbox#remove clock;
      ignore(Lwt_main.run (round_three_prescreen ()))
    else (
      let (response, new_bank, ai_correct, ai_ans) = Gameplay.receive_head_to_head (answer#text) in
      let correct = if ai_correct then " correctly " else " incorrectly " in
      let new_response = response ^ " The Chaser answered" ^ correct ^ "with " ^ ai_ans in
      game_response#set_text new_response;
      answer#set_text "Answer: ";
      question#set_text (Gameplay.send_question ());
      reset_time ()) in

  let show_answer s =
    s >|= Zed_utf8.singleton >>=
    fun x ->
      (* (answer#set_text (answer#text ^ x); return ()) in  *)
      (let new_text = if !time<=0 then "Out of time! Press Enter to continue to next question" else (answer#text ^ x) in
        answer#set_text new_text; return ()) in

  let backspace () =
    if !time<= 0 then
      (* ignore(Lwt_main.run (round_two_prescreen ())) *)
      ignore(Printf.sprintf "%s" "TESTING")
    else
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
  vbox#add positions;
  vbox#add question;
  vbox#add answer;
  vbox#add game_response;

  (* Update the time every second. *)
  ignore (Lwt_engine.on_timer 1.0 true (fun _ -> clock#set_text (new_get_time ())));
  (* ignore (Lwt_engine.on_timer 0.2 true (fun _ -> show_answer ())); *)
  (* Quit when the exit button is clicked. *)
  (* button#on_click (fun () -> wakeup_exn wakener Exit); *)


  button#on_event handle_evt;
  submit#on_event handle_evt;

  (* Run in the standard terminal. *)
  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter

let round_two_prescreen () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let title = new label "Round 2" in
  let ins = new label "Press Enter" in
  let input = new label "" in

  let cat_set = ref false in
  let board_set = ref false in

  let update_info () =
    if input#text<>"" && not !board_set then begin
      board_set := Gameplay.receive_board (input#text);
      (* board_set := true; *)
      (if not !board_set then
        ins#set_text ("That is not a valid selection. Please chose A, B or C")
      else begin
        ins#set_text ("Please choose a category: Science, History, Music, or Geography");
        input#set_text "" end)
    end
    else (if input#text <> "" && not !cat_set then begin
      cat_set := Gameplay.receive_cat (input#text);
      (if not !cat_set then
        ins#set_text ("That is not a valid category or has already been used.\n Please select from Science, History, Music or Geography")
      else begin
        ins#set_text ("Ready to begin. Press Enter.");
        input#set_text "" end)
    end
    else (if not !board_set then
      let a = "(A) Start 4 spots away from the bank and wager 1/2 of the money in your wallet\n" in
      let b = "(B) Start 5 spots away from the bank and wager 2/3 of the money in your wallet\n" in
      let c = "(C) Start 6 spots away from the bank and wager all of the money in your wallet\n" in
      ins#set_text ("Please select an option to begin Round 2\n"^a^"\n"^b^"\n"^c)
    else (if not !cat_set then
      ins#set_text ("Please choose a category: Science, History, Music, or Geography")
    else ignore(Lwt_main.run (round_two ()))))) in


  let show_input s =
    s >|= Zed_utf8.singleton >>=
    fun x -> input#set_text (input#text ^ x); return () in


  let backspace () =
    let curr_text = input#text in
    if curr_text <> "" then
      input#set_text (String.sub curr_text 0 ((String.length curr_text) -1))
    else () in

  let handle_evt evt =
    match evt with
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape } -> wakeup wakener (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter }  -> update_info (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch} -> ignore (show_input (return ch)); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Backspace} -> backspace (); true
    | _ -> false in

  vbox#add title;
  vbox#add ins;
  vbox#add input;

  vbox#on_event handle_evt;

  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter



let round_one () =
  let waiter, wakener = wait () in

  let vbox = new vbox in
  let clock = new label (get_time ()) in
  let bank = new label "$0.00" in
  let question = new label (Gameplay.send_question ()) in
  let answer = new label ("Answer:") in
  let game_response = new label "" in
  let button = new button "Press esc to exit" in
  let submit = new button "Press enter to submit" in

  let update_label () =
    if !time <= 0 then
      ignore(Lwt_main.run (round_two_prescreen ()))
    else
    let (response, new_bank) = Gameplay.receive_answer (answer#text) in
    game_response#set_text response;
    bank#set_text ("$" ^ new_bank);
    answer#set_text "Answer:";
    question#set_text (Gameplay.send_question ()) in

  let show_answer s =
    s >|= Zed_utf8.singleton >>=
    fun x ->
      (* (answer#set_text (answer#text ^ x); return ()) in  *)
      (let new_text = if !time<=0 then "Out of time! Press Enter to continue to Round 2" else (answer#text ^ x) in
        answer#set_text new_text; return ()) in

  let backspace () =
    if !time<= 0 then
      ignore(Lwt_main.run (round_two_prescreen ()))
    else
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

let main_menu () =
  let waiter, wakener = wait () in

  let diff_set = ref false in
  let cat_set = ref false in

  let vbox = new vbox in
  let title = new label "The Chase" in
  let ins = new label "Press enter to begin" in
  let input = new label "" in
  let setup_question = new label "" in




  let update_info () =
    if input#text<>"" && not !diff_set then begin
      diff_set := Gameplay.receive_diff (input#text);
      (if not !diff_set then
        setup_question#set_text ("That is not a valid difficulty. Please select easy, medium or hard")
      else begin
        setup_question#set_text ("Please choose a category: Science, History, Music, or Geography");
        input#set_text "" end)
    end
    else (if input#text <> "" && not !cat_set then begin
      cat_set := Gameplay.receive_cat (input#text);
      (if not !cat_set then
        setup_question#set_text ("That is not a valid category or has already been used.\n Please select from Science, History, Music, or Geography")
      else begin
        setup_question#set_text ("Ready to begin. Press Enter.");
        input#set_text "" end)
    end
    else (if not !diff_set then
      setup_question#set_text ("Do you want to play on easy, medium, or hard? ")
    else (if not !cat_set then
      setup_question#set_text ("Please choose a category: Science, History, Music, or Geography")
    else ignore(Lwt_main.run (round_one ()))))) in


  let show_input s =
    s >|= Zed_utf8.singleton >>=
    fun x -> input#set_text (input#text ^ x); return () in


  let backspace () =
    let curr_text = input#text in
    if curr_text <> "" then
      input#set_text (String.sub curr_text 0 ((String.length curr_text) -1))
    else () in

  let handle_evt evt =
    match evt with
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Escape } -> wakeup wakener (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Enter }  -> update_info (); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Char ch} -> ignore (show_input (return ch)); true
    | LTerm_event.Key { LTerm_key.code = LTerm_key.Backspace} -> backspace (); true
    | _ -> false in

  vbox#add title;
  vbox#add ins;
  vbox#add setup_question;
  vbox#add input;


  vbox#on_event handle_evt;

  Lazy.force LTerm.stdout
  >>= fun term ->
  run term vbox waiter



let () = try Lwt_main.run (main_menu ()) with
         | Failure f -> exit 0