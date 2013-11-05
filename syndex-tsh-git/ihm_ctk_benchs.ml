(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                          Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

open Camltk
open Types
open Ihmcommon_ctk
open Adequation_core

(* Here we choose to display the progress of the adequation in a tk mode *)
module Latency_adequation = Latency_adequation.Make (Progress_box.Tk)

module Reliability_adequation = Reliability_adequation.Make (Progress_box.Tk)

module Aaatp_adequation_reliability = Aaatp_adequation_reliability.Make (Progress_box.Tk)

let read_error_message_box frame filename line message =
  ok_message_box frame
    ((Filename.basename filename) ^": " ^ (string_of_int line) ^ ":\n" ^
     message ^ "\n")

let adequation_graph = ref (None, None)

let main_window_title win filename changed =
  let file = match filename with
  | None -> ""
  | Some filename -> " : " ^ (Filename.basename filename)
  and changed = match changed with
  | true -> " **"
  | false -> "" in
  let title = syndex_version_msg ^ file ^ changed in
  match is_initialised_tk () with
  | true -> Wm.title_set win title
  | false -> ()

let top_win_create () =
  let binary_directory = ref "" in
  let file_changed = ref false in
  let adequation_in_progress = ref false in
  let save_adequation = ref false in

  let win = openTk () in
  let keep_info_tv = Textvariable.create () in
  let save_adequation_tv = Textvariable.create () in
  let generate_chronos_tv =  Textvariable.create () in
  main_window_title win None false;

  let frame = frame_create win in
  let menu_bar = frame_create frame in
  let infobar_frame = frame_create frame in
  let info_bar = Text.create infobar_frame [Background White] in
  let info_scrollbar = Scrollbar.create infobar_frame [Orient Vertical] in
  Text.configure info_bar [YScrollCommand (Scrollbar.set info_scrollbar)];
  Scrollbar.configure info_scrollbar [ScrollCommand (Text.yview info_bar)];
  let msgbar_frame = frame_create frame in
  let msg_bar = Text.create msgbar_frame [Background background_color;
					  TextHeight 8] in
  let msg_scrollbar = Scrollbar.create msgbar_frame [Orient Vertical] in
  Text.configure msg_bar [YScrollCommand (Scrollbar.set msg_scrollbar)];
  Scrollbar.configure msg_scrollbar [ScrollCommand (Text.yview msg_bar)];

  let top = {
    tw_frame = frame;
    tw_info_bar = info_bar;
    tw_msg_bar = msg_bar;
    tw_current_directory = "";
    tw_current_file = None;
    tw_chrono = false;
    tw_algos = [];
    tw_archis = [];
    tw_schedules = [];
    tw_codes = [];
    tw_windows = [];
    tw_file_changed = (function () -> ());
    tw_alg_clip = [],[];
    tw_arc_clip = [],[];
    tw_alg_graphclip = Graph_ctk.create_clipboard ();
    tw_arc_graphclip = Graph_ctk.create_clipboard ()
  } in
  let changed () =
    file_changed := true;
    main_window_title win top.tw_current_file true in
  top.tw_file_changed <- changed;

  let file_button = menubutton_create menu_bar "File"
  and options_button = menubutton_create menu_bar "Options"
  and algo_button = menubutton_create menu_bar "Algorithm"
  and archi_button = menubutton_create menu_bar "Architecture"
  and constraints_button = menubutton_create menu_bar "Constraints"
  and adeq_button = menubutton_create menu_bar "Adequation"
  and code_button = menubutton_create menu_bar "Code"
  and help_button = menubutton_create menu_bar "Help"
  and debug_button = menubutton_create menu_bar "Debug" in

  let file_menu = Menu.create file_button [TearOff false]
  and options_menu = Menu.create options_button [TearOff false]
  and algo_menu = Menu.create algo_button [TearOff false]
  and archi_menu = Menu.create archi_button [TearOff false]
  and constraints_menu = Menu.create constraints_button [TearOff false]
  and adeq_menu = Menu.create adeq_button [TearOff false]
  and code_menu = Menu.create code_button [TearOff false]
  and help_menu = Menu.create help_button [TearOff false]
  and debug_menu = Menu.create debug_button [TearOff false] in

  let includelib_submenu = Menu.create file_menu [TearOff false] 
  and delete_definition_submenu = Menu.create algo_menu [TearOff false]
  and delete_xsc_submenu = Menu.create algo_menu [TearOff false]
  and delete_operator_submenu = Menu.create archi_menu [TearOff false]
  and delete_media_submenu = Menu.create archi_menu [TearOff false]
  and delete_architecture_submenu = Menu.create archi_menu [TearOff false] 

   (************* Fault Tolerance Menu *****************************)
   and adeq_fault_submenu = Menu.create adeq_menu [TearOff false]
   and adeq_nofault_submenu = Menu.create adeq_menu [TearOff false]  in
   (****************************************************************)


  let rec set_global_bindings wgt = 
    let bindings = [
      [Control],"o", (function _ -> open_file ());
      [Control],"s", (function _ -> save_file ());
      [Control],"A", (function _ -> archi_main_open ());
      [Control],"D", (function _ -> algo_main_open ());
      [Control],"k", (function _ -> change_clean_infobar ());
      [],"F1", (function _ -> help_html ());
      [],"F2", (function _ -> tutorial_html ());
      [],"F3", (function _ -> latency_adequation No_Flatten);
      [],"F4", (function _ -> latency_adequation Flatten);
(*       [],"F7", (function _ -> fault_tolerance_adequation No_Flatten); *)
      [],"F5", (function _ -> generate_code top.tw_chrono);
      [],"F6", (function _ -> view_code ());
      [],"F7", (function _ -> latency_adequation_schedule_view ())] in
    List.iter (function m,k,c -> bind wgt [m,KeyPressDetail k] (BindSet ([], c))) bindings

  and save_changed_file () =
    match !file_changed with
    | true ->
	(match dialog_create syndex_version_msg "Current file has changed.\nDo you want to save it ?\n" 0 ["Save";"Discard changes";"Cancel"] with
	| 0 -> save_file ();true
	| 1 -> true
	| _ -> false)
    | false -> true

  and close_application () =
    List.iter (function (algo,_) -> Algorithm_ctk.algo_close top algo.alg_path)
      top.tw_algos;
    List.iter (function (archi,_) -> (Architecture_ctk.archi_close top
					(archi.arc_library,archi.arc_name)))
      top.tw_archis;
    List.iter (function s,_ -> Schedule_ctk.schedule_view_close s)
      top.tw_schedules;
    code_close_all ();
    List.iter (function id,_ -> generic_window_close top id) top.tw_windows;
    Hashtbl.clear Adequation_core.schedules;
    adequation_graph := None, None;
    Application.close ()

  and update_menus () =
    update_lib_menu ();
    update_definition_delete_menu ();
    update_operator_delete_menu ();
    update_media_delete_menu ();
    update_architecture_delete_menu ();
    update_xsc_delete_menu ()

  and close_file () =
    text_clean top.tw_msg_bar;
    match save_changed_file () with
    | true ->
	close_application ();
	top.tw_current_file <- None;
	file_changed := false;
	main_window_title win None false;
	update_menus ();
	true
    | false -> false

(********************************)
(******   OPEN SIMULATION   *****)
(********************************)

and open_file_name_simulation name =
    match close_file () with
    | true ->
	(match Sys.file_exists name with
	| true ->
	    (try adequation_graph := (Read.open_syndex_file name), None
	    with Read.Error (filename, line, message) ->
	      read_error_message_box top.tw_frame filename line message);
	    update_menus ();
	    top.tw_current_file <- Some name;
	    main_window_title win top.tw_current_file false;
            (*algo_main_open ();
	    archi_main_open ()*)
	| false -> 
	    ok_message_box top.tw_frame ("Cannot open file "^name))
    | false -> ()

(**********************************)
(****** END OPEN SIMULATION *******)
(**********************************)

  and open_file_name name =
    match close_file () with
    | true ->
	(match Sys.file_exists name with
	| true ->
	    (try adequation_graph := (Read.open_syndex_file name), None
	    with Read.Error (filename, line, message) ->
	      read_error_message_box top.tw_frame filename line message);
	    update_menus ();
	    top.tw_current_file <- Some name;
	    main_window_title win top.tw_current_file false;
	    algo_main_open ();
	    archi_main_open ()
	| false ->
	    ok_message_box top.tw_frame ("Cannot open file "^name))
    | false -> ()

  and open_file () =
    match Tk.getOpenFile [FileTypes [{typename="SynDEx files";extensions=[".sdx"];mactypes=[]}];InitialDir top.tw_current_directory] with
    | "" -> ()
    | name ->
	top.tw_current_directory <- Filename.dirname name;
	open_file_name name

  and save_as_file () =
    let wgt = match is_initialised_tk () with
    | true -> None
    | false -> Some (openTk()) in
    (match Tk.getSaveFile [DefaultExtension ".sdx";FileTypes [{typename="SynDEx files";extensions=[".sdx"];mactypes=[]}];InitialDir top.tw_current_directory] with
    | "" -> ()
    | filename -> 
	let len = String.length filename in
  	let name = match (String.sub filename (len-4) 4) with
  	| ".sdx" -> filename
  	| _ -> filename^".sdx" in
	top.tw_current_file <- Some name;
	top.tw_current_directory <- Filename.dirname name;
	save_file ());
    match wgt with
    | Some wgt -> Tk.destroy wgt
    | None -> ()

  and save_file () =
    match top.tw_current_file with
    | None -> save_as_file();
    | Some name ->
	file_changed := false;
	main_window_title win top.tw_current_file false;
	Write.application_save name (!save_adequation,!adequation_graph)

  and choose_lib_dir () =
    ()

  and change_text_variable tv =
    let newval = match Textvariable.get tv with
      | "1" -> "0"
      | _ -> "1" in
      Textvariable.set tv newval

  and change_clean_infobar () =
    change_text_variable keep_info_tv
      
  and update_option_from_text_variable tv f_affect =
    let rec f () =
      let tval = Textvariable.get tv in
	let tval_b = match tval with
	  | "1" -> true
	  | _ -> false in
	  f_affect tval_b;
	  Textvariable.set tv tval;
	  Textvariable.handle tv f in
      f ()
	
  and algo_main_open () =
    match Application.algo_main_get () with
    | "","",_ -> error_message top "No main algorithm, chose a definition to edit.";
	Algorithm_ctk.operation_definition_edit top (Some set_global_bindings)
    | libm,algom,_ -> Algorithm_ctk.algo_open top (Some set_global_bindings) libm algom []

  and archi_main_open () =
    match Application.archi_main_get () with
    | "","" -> error_message top "No main architecture, chose a definition to edit.";
	Architecture_ctk.architecture_definition_edit top (Some set_global_bindings)
    | libm,archim -> Architecture_ctk.archi_open top (Some set_global_bindings) libm archim

  and adequation adeqtype adeq_function =
    let (algomain_lib,algomain_name,_),(archimain_lib,archimain_name) = Application.algo_main_get (), Application.archi_main_get () in
    match !adequation_in_progress with
    | true -> ()
    | false ->
	adequation_in_progress := true;
	text_clean top.tw_msg_bar;
	match ((algomain_lib,algomain_name),(archimain_lib,archimain_name)) with
	| ("",""),_ -> 	adequation_in_progress := false;
	    error_message top "No main algorithm defined"
	| _,("","") -> 	adequation_in_progress := false;
	    error_message top "No main architecture defined"
	| (algmain_lib,algmain_name),_ ->
	    try
	      let graph = Transformation.transform adeqtype algmain_lib algmain_name in
	      let warnings = Transformation.consistancy_check graph in
	      let warning_str = string_of_string_list warnings "\n\n" in
		(match warning_str with
		| "" -> ()
		| _ -> error_message top ("WARNING:\n"^warning_str));
		message_info top.tw_msg_bar "Adequation";
		let graph,schedules = adeq_function graph in                
		  adequation_graph := (Some graph,Some schedules); 
		  adequation_in_progress := false;
		  message_info top.tw_msg_bar "Adequation done";
	    with Failure msg ->
	      adequation_in_progress := false;
	      error_message top ("ABORTING:\n"^msg)(*;
	    Adequation.check_schedule graph*)
	      
  and schedule_view schedule_info_function =
    match !adequation_graph,!adequation_in_progress with
    | (Some graph,_),false ->
	let (algomain_lib,algomain_name,_) = Application.algo_main_get () in
	let (archimain_lib,archimain_name) = Application.archi_main_get () in
	  (try                 
	    let schedule_info = schedule_info_function graph in               
	    let title = "Adequation "^(string_of_ref_type algomain_lib algomain_name)^" onto "^
			(string_of_ref_type archimain_lib archimain_name) in                 
	      Schedule_ctk.schedule_view top title schedule_info set_global_bindings
	   with Failure msg -> error_message top msg)
    | _ -> ()

  and latency_adequation adeqtype =
    adequation adeqtype Latency_adequation.adequation

  and latency_adequation_schedule_view () =
     schedule_view Adequation_core.pretty_conv_graph

  (************************************)
  (******** FAULT TOLERANCE ***********)
  (************************************)
 
  and fault_tolerance_adequation adeqtype = () (*
    (*match Xsc_ctk.faults_data_choose top with
       | false,_,_,_    -> ()
       | true,npf,nmf,check -> *)
    let check,npf,nmf = true,1,0 in                     
             Aaatp_adequation_reliability.split_data := check;
             Aaatp_adequation_reliability.failures := {Aaatp_adequation_reliability.npf=npf; Aaatp_adequation_reliability.nmf=nmf};
             Fault_tolerance_genexec.npf := npf;
             Fault_tolerance_genexec.nmf := nmf;    
             adequation adeqtype (Aaatp_adequation_reliability.fault_tolerance_adequation);                             
	     schedule_view Aaatp_adequation_reliability.pretty_conv_graph*)

  and fault_tolerance_schedule_view () =
    schedule_view Adequation_core.pretty_conv_graph

(*  and fault_tolerance_generate_code chrono =
    text_clean top.tw_msg_bar;
    message_info top.tw_msg_bar "Fault Tolerant Executive Generation";
    match !adequation_graph with
    | None,_ -> error_message top "Do the Fault Tolerance Adequation first"
    | (Some graph),_ ->
	let archilib,archiname = Application.archi_main_get () in
	match Architecture.operator_main_get archilib archiname with
	| "" -> error_message top "No main operator defined"
	| _ ->
	    let application_name = (match top.tw_current_file with
	    | None -> ""
	    | Some name -> name) in
	      try
		Fault_tolerance_genexec.generate_code application_name graph chrono
	      with Failure s -> error_message top s

  *******************************************)
  (********** End Fault Tolerance  ***********)
  (*******************************************)


  (************************************)
  (******** RELIABILITY     ***********)
  (************************************)
  and reliability_adequation adeqtype file_name =
    let level_max = 1 in                     
       (*Reliability_adequation.level_max := level_max;*)    
       Reliability_adequation.file_name := file_name;     
       adequation adeqtype (Reliability_adequation.reliability_adequation);  
       schedule_view Reliability_adequation.pretty_conv_graph

  and reliability_schedule_view () =
    schedule_view Reliability_adequation.pretty_conv_graph


and pss s =
  print_string (s^"\n");
  flush stdout

(********************************************************************) 
(**************               SIMULATION               **************) 
(********************************************************************) 
  and benchs x = 
  let dir = "/scratch/kalla/simulations/" in  
  let readme = dir^"Simulation" in 
  let out_readme = open_out readme in
    output_string out_readme ("\n\n\n Comparaison :\n\n\n"); 
    close_out out_readme;  
    let p = ref 0 in 
    let tab_p = [|5;6;7|] in 
    for ind_p = 0 to 0 do (* architecture size = tab_p[ind_p] *) 
      p := tab_p.(ind_p); 
      let archi_name = ("archi_p"^(string_of_int !p)) in 
      let rates_fail = [| 3.0 ; 4.0 ; 5.0 ; 6.0|] in       
	for ind_rate = 0 to 3 do  
	  let rate = ref rates_fail.(ind_rate) in

	  let aaa_tp_len = (dir^"results/len_aaa_tp_rate_"^(string_of_float !rate)) in  
          let aaa_f_len  = (dir^"results/len_aaa_f_rate_"^(string_of_float !rate)) in 
          let out_aaa_tp_len = open_out aaa_tp_len in   
          let out_aaa_f_len  = open_out aaa_f_len in   

	  let aaa_tp_rel = (dir^"results/rel_aaa_tp_rate_"^(string_of_float !rate)) in  
          let aaa_f_rel  = (dir^"results/rel_aaa_f_rate_"^(string_of_float !rate)) in  
          let out_aaa_tp_rel = open_out aaa_tp_rel in     
          let out_aaa_f_rel = open_out aaa_f_rel in   

          let aaa_f_rep= (dir^"results/rep_aaa_f_rate_"^(string_of_float !rate)) in 
          let out_aaa_f_rep = open_out aaa_f_rep in     

          let size = [|40;80;120|] in 
	    for i = 0 to 0 do  (* task graphes size = i*10 *) 
 	      let nb = size.(i) in
                output_string out_aaa_tp_len (string_of_int nb);  
                output_string out_aaa_f_len (string_of_int nb);  
                output_string out_aaa_tp_rel (string_of_int nb);  
                output_string out_aaa_f_rel (string_of_int nb);  
                output_string out_aaa_f_rep (string_of_int nb);  
                let dir_opr=(dir^"graphs/p"^(string_of_int !p)^"/t"^(string_of_int nb)^"/")in
                for j = 1 to 50 do (*nb red graphs*) 
	  	  let algo_name = ("alg"^(string_of_int nb)^"_red"^(string_of_int j)) in
                  pss "-----------------------------------------------";
		  let name = (dir_opr^algo_name^"_ccr"^(string_of_float 1.0)^"_"^archi_name^".sdx") in
                  let name_rel = (dir_opr^algo_name^"_ccr"^(string_of_float 1.0)^"_rate"^
                                 (string_of_float !rate)^"_"^archi_name^".rel") in
                    pss (name^" :: rate = "^(string_of_float !rate)); 
		    open_file_name_simulation name;                                                                      

                  for level = 1 to 1 do  (* level = npf *)

                    Aaatp_adequation_reliability.level_max := level;  
                    Aaatp_adequation_reliability.file_name := name_rel;         
                    adequation No_Flatten (Aaatp_adequation_reliability.fault_tolerance_adequation);
                    output_string out_aaa_tp_len (" "^(string_of_float !Aaatp_adequation_reliability.old_len));
                    output_string out_aaa_tp_rel (" "^(string_of_float !Aaatp_adequation_reliability.old_rel));
                    pss (" AAA-TP (1,0)  = "^(string_of_float  !Aaatp_adequation_reliability.old_len)^
                         "        ,  "^(string_of_float  !Aaatp_adequation_reliability.old_rel));

                    let thetas = [|22.5 ; 45.0 ; 67.5|] in

                    for theta = 0 to 2 do
                      
                      Reliability_adequation.obj_rel := !Aaatp_adequation_reliability.old_rel;
                      Reliability_adequation.obj_len := !Aaatp_adequation_reliability.old_len;
                      Reliability_adequation.level_max := level;  
                      Reliability_adequation.theta := thetas.(theta);  
                      Reliability_adequation.file_name := name_rel;         

                      adequation No_Flatten (Reliability_adequation.reliability_adequation);        
                      output_string out_aaa_f_len (" "^(string_of_float !Reliability_adequation.old_len));
                      output_string out_aaa_f_rel (" "^(string_of_float !Reliability_adequation.old_rel));
                      let rate_rep = ((float_of_int !Reliability_adequation.rate_replication) /. (float_of_int nb)) in
                      output_string out_aaa_f_rep (" "^(string_of_float rate_rep));                    
                       pss (" AAA-F ("^(string_of_float thetas.(theta))^") = "^(string_of_float  !Reliability_adequation.old_len)^
                            "        ,  "^(string_of_float  !Reliability_adequation.old_rel)^
                            " replication rate = "^(string_of_float  rate_rep)^"%"); 
                    done; pss "";

                  done;                   
                  flush out_aaa_tp_len;flush out_aaa_f_len;
                  flush out_aaa_tp_rel;flush out_aaa_f_rel;flush out_aaa_f_rep;
                  pss "";
                  close_file (); 
                done;
	        output_string out_aaa_tp_len "\n";output_string out_aaa_f_len "\n";  
	        output_string out_aaa_tp_rel "\n";output_string out_aaa_f_rel "\n";  
	        output_string out_aaa_f_rep "\n"; 
	    done;    
	    close_out out_aaa_tp_len;close_out out_aaa_f_len;
            close_out out_aaa_tp_rel;close_out out_aaa_f_rel;
            close_out out_aaa_f_rep;
	done;
    done

      
(*********************************************)   
(**************  END  SIMULATION **************) 
(**********************************************) 

  and synchronized () =
    match !adequation_graph with
    | None,_ -> error_message top "Do the Adequation first"
    | (Some graph),_ -> ignore(Genexec.synchronize graph)

  and generate_code chrono =
    text_clean top.tw_msg_bar;
    message_info top.tw_msg_bar "Executive Generation";
    match !adequation_graph with
    | None,_ -> error_message top "Do the Adequation first"
    | (Some graph),_ ->
	let archilib,archiname = Application.archi_main_get () in
	match Architecture.operator_main_get archilib archiname with
	| "" -> error_message top "No main operator defined"
	| _ ->
	    let application_name = (match top.tw_current_file with
	    | None -> ""
	    | Some name -> name) in
	      try
		Genexec.generate_code application_name graph chrono
	      with Failure s -> error_message top s

  and code_close code =
    let toclose,codes = List.partition ((=) code) top.tw_codes in
    top.tw_codes <- codes;  
    List.iter Tk.destroy toclose

  and code_close_others code =
    let toclose, codes = List.partition ((<>) code) top.tw_codes in
    top.tw_codes <- codes;
    List.iter Tk.destroy toclose

  and code_close_all () =
    List.iter Tk.destroy top.tw_codes;
    top.tw_codes <- []

  and code_view win filename =
    let window = Toplevel.create win [] in
    Wm.title_set window ("File "^(Filename.basename filename));
    let menu_bar = Ihmcommon_ctk.frame_create window in
    let window_menu =
      let btn = Menubutton.create menu_bar [Text "Window"] in
      let mnu = Menu.create btn [TearOff false] in
      Menubutton.configure btn [Menu mnu];
      pack [btn] [Side Side_Left];
      mnu in
    Menu.add_command window_menu [Label "Close"; Accelerator "Ctrl-W"; Command (function () -> code_close window)];
    bind window [[Control], KeyPressDetail "w"] (BindSet ([], function _ -> code_close window));
    Menu.add_command window_menu [Label "Close Others"; Command (function () -> code_close_others window)];
    Menu.add_command window_menu [Label "Close All"; Command (function () -> code_close_all ())];
    pack [menu_bar] [Fill Fill_X];
    let txt = Text.create window [Background background_color] in
    let ysb = Scrollbar.create window [Orient Vertical] in
    Text.configure txt [YScrollCommand (Scrollbar.set ysb)];
    Scrollbar.configure ysb [ScrollCommand (Text.yview txt)];
    pack [ysb] [Side Side_Right; Fill Fill_Y];
    pack [txt] [Expand true; Fill Fill_Both];
    let f = open_in filename in
    (try
      while true do
	Text.insert txt (TextIndex (End,[])) ((input_line f)^"\n") [];
      done
    with _ -> ());
    window

  and view_code () =
    let view file =
      let win = code_view top.tw_frame file in
	set_global_bindings win;
      bind win [[], Destroy] (BindSet ([],function _ -> top.tw_codes <- List.filter ((<>) win) top.tw_codes));
      top.tw_codes <- win::top.tw_codes in
    List.iter code_close top.tw_codes;
    top.tw_codes <- [];
    let application_name = (match top.tw_current_file with
    | None -> ""
    | Some name -> name) in
    List.iter view (Genexec.generated_files application_name)

  and help_html () = ignore (html_view top.tw_frame ((!binary_directory)^"/../manual/manual.html"))

  and tutorial_html () = ignore (html_view top.tw_frame ((!binary_directory)^"/../tutorial/tutorial.html"))

  and help_txt () = ignore(file_view top.tw_frame ((!binary_directory)^"/../HELP.txt"))

  and copyright () = ignore(file_view top.tw_frame ((!binary_directory)^"/../COPYRIGHT.txt"))

  and about () =
    let window = Toplevel.create Widget.default_toplevel [] in
    Wm.title_set window syndex_version_msg;
    
    let f (txt,web) =
      let lbl = Label.create window [Text txt; Justify Justify_Left; Font font] in
      let web_view url = ignore (html_view top.tw_frame url) in
      (match web with
      |	Some url ->
	  Label.configure lbl [Font underlined_font];
	  bind lbl [[],Enter] (BindSet ([],(function _ -> Label.configure lbl [Foreground Red])));
	  bind lbl [[],Leave] (BindSet ([],(function _ -> Label.configure lbl [Foreground Black])));	  
	  bind lbl [[],ButtonPress] (BindSet ([],(function _ -> web_view url)))
      |	_ -> ());
      pack [lbl] [Side Side_Top; Anchor W] in
    
    List.iter f [(syndex_version_msg^"\n"^web_msg^"\n"),(Some syndex_web);
		 inria_msg,(Some inria_rocquencourt_web);
		 (team_msg^"\n"),(Some team_web);
		 ("Authors : "^author_msg^"\n"),None];

    let ok_action _ = Tk.destroy window in
    let ok = Button.create window [Text "ok"; Command ok_action] in
    bind window [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
    bind window [[],KeyPressDetail "Escape"] (BindSet ([],ok_action));
    pack [ok] [Side Side_Bottom; Expand true]


(** update the menu related to the libraries. Libraries can either be included or "un-included" *)
  and update_lib_menu () =
    Menu.delete includelib_submenu (Number 0) Last;
    let dir = Unix.opendir !Read.library_directory in
    let libfiles = ref [] in
      (try
	 while true do
	   let name = Unix.readdir dir in
	     if Filename.check_suffix name ".sdx" then libfiles:= name :: !libfiles;
	 done
       with End_of_file -> ());
      let included = Application.included_libs () in
	List.iter (function l -> 
		     
		     let is_included = List.mem (Filename.chop_extension l) included in
		     let my_label,my_command =
		       (match is_included with
			| true -> (
			    Label ((Filename.chop_extension l) ^"(included)"),
			    (* command associated with the lib button, can be used to include or un-include a lib.
			       Can't be un-included if one of the definitions belonging to the lib has been referenced *)
			    
			    Command (function _ ->
				       match Application.library_referenced (Filename.chop_extension l) with
				       | true -> 
					  error_message top  
					     ("Library '"^ Filename.chop_extension l ^"' currently used, cannot un-include")
				       | false -> (
					   Menu.configure_command includelib_submenu (
					     Pattern ((Filename.chop_extension l) ^"(included)")) [State Normal];
					   (
					     (* generic remove function: removes elements in "listtoremove"
						which belong to library "lib" with function "removefun" *)
					     let rec remove_defs listtoremove lib removefun =
					       match listtoremove with
					       | (current_lib,current_name) :: tl ->
						   if current_lib = (Filename.chop_extension lib)
						   then 
						     removefun current_lib current_name
						   else 
						     (); 
						   remove_defs tl lib removefun
					       | [] -> () in 
					       (* removing algorithm, operator, medium and architecture definitions *)
					       remove_defs (Algorithm.algo_list ()) l Algorithm.algo_delete;
					       remove_defs (Architecture.operatortypes_list ()) l Architecture.operator_definition_delete;
					       remove_defs (Architecture.mediatypes_list ()) l Architecture.media_definition_delete;
					       remove_defs (Architecture.architectures_list ()) l Architecture.architecture_delete
					   );
					   
					   (* removing library name from the list and updating menu in the interface *)
					   Application.library_remove (Filename.chop_extension l);
					   update_lib_menu ();
					   
					   List.iter (function wg -> Algorithm_ctk.algo_menus_update wg) top.tw_algos;
					   List.iter (function wg -> Architecture_ctk.archi_menus_update wg) top.tw_archis)
				    ))
			| false ->
			    Label ((Filename.chop_extension l)),
			    Command (function _ -> 
				       Menu.configure_command includelib_submenu (
					 Pattern (Filename.chop_extension l)) [State Disabled]; 
			      (try Read.open_syndex_library l
			      with Read.Error (filename, line, message) ->
				read_error_message_box top.tw_frame filename line message);
			      update_lib_menu ();
			      List.iter (function wg -> Algorithm_ctk.algo_menus_update wg) top.tw_algos;
			      List.iter (function wg -> Architecture_ctk.archi_menus_update wg) top.tw_archis)) in 
		     (* adding button to include/un-include a lib to the menu *)
		     (Menu.add_command includelib_submenu [my_label;my_command;State Normal])) (List.sort compare !libfiles);

	  
  and update_submenu submenu submenu_name father_menu list_elements command =
    (* Keep only local definitions *)
    let deletable = List.sort (fun (_,name1) (_,name2) -> compare name1 name2)
	(List.filter (function (l,_) -> l="") (list_elements ())) in
    match deletable = [] with
      (* Should use something less specific for the index (widget, num) *)
    | true -> Menu.configure_cascade father_menu (Pattern submenu_name) [State Disabled]
    | false ->
 	Menu.configure_cascade father_menu (Pattern submenu_name) [State Active]; 
	Menu.delete submenu (Number 0) Last;
	let submenu_entries = List.map (function (_,element) ->  
	  [Label element; Command (function () -> command element;
	    update_submenu submenu submenu_name father_menu list_elements command)]) deletable in
	rec_cascade submenu submenu_entries

  and update_definition_delete_menu () =
    update_submenu delete_definition_submenu "Delete Local Definition" algo_menu (Algorithm.algo_list) (Algorithm_ctk.operation_definition_delete top)

  and update_xsc_delete_menu () =
    update_submenu delete_xsc_submenu "Delete Software Component" algo_menu (fun () -> List.map (fun x -> ("",x)) (Application.xsc_namelist ())) (Algorithm_ctk.xsc_delete top)

  and update_operator_delete_menu () =
    update_submenu delete_operator_submenu "Delete Local Operator" archi_menu (Architecture.operatortypes_list) (Architecture_ctk.operator_definition_delete top)

  and update_media_delete_menu () =
    update_submenu delete_media_submenu "Delete Local Medium" archi_menu (Architecture.mediatypes_list) (Architecture_ctk.media_definition_delete top)

  and update_architecture_delete_menu () =
    update_submenu delete_architecture_submenu "Delete Local Architecture" archi_menu (Architecture.architectures_list) (Architecture_ctk.architecture_definition_delete top) in

  let ask_save () =
    match dialog_create syndex_version_msg "Save current application ?" 0 ["Yes" ;"No"] with
    | 0 -> save_file ()
    | _ -> ()

  and quit () =
    match close_file () with
    | true ->
	closeTk ();
	exit 0
    | false -> () in

  set_global_bindings win;

  Menu.add_command file_menu [Label "Open"; Accelerator "Ctrl-O"; Command open_file];
  Menu.add_command file_menu [Label "Save"; Accelerator "Ctrl-S"; Command save_file];
  Menu.add_command file_menu [Label "Save as"; Command save_as_file];
  Menu.add_command file_menu [Label "Close"; Command (function _ -> ignore (close_file ()))];
  Menu.add_separator file_menu;
  Menu.add_cascade file_menu [Label "Include Library"; Menu includelib_submenu];
(*   Menu.add_command file_menu [Label "Choose Library Directory"; Command choose_lib_dir]; *)
  Menu.add_separator file_menu;
  Menu.add_command file_menu [Label "Quit"; Command quit];

  Textvariable.set keep_info_tv "0";
  Textvariable.handle keep_info_tv (function () ->
				      update_option_from_text_variable keep_info_tv (fun new_val -> clean_info_bar := (not new_val)));
  Textvariable.set save_adequation_tv "0";
  Menu.add_checkbutton options_menu [Label "Keep information in main window"; Accelerator "Ctrl-K";Variable keep_info_tv];
  Textvariable.handle save_adequation_tv (function () ->
					    update_option_from_text_variable save_adequation_tv (fun new_val -> save_adequation := new_val));
  Menu.add_checkbutton options_menu [Label "Save adequation with application"; Variable save_adequation_tv];

  let submenu_algo = Menu.create algo_menu [] in
  List.iter (function t,l -> Menu.add_command submenu_algo [Label l; Command (function _ -> Algorithm_ctk.operation_definition_create top (Some set_global_bindings) t;update_definition_delete_menu ())]) [Operation,"Function";Constant,"Constant";Sensor,"Sensor";Actuator,"Actuator";(Memory (Symbolic.Float 0.,Symbolic.Float 0.)),"Delay"];
  Menu.add_cascade algo_menu [Label "New Local Definition"; Menu submenu_algo];
  Menu.add_command algo_menu [Label "Edit Definition"; Command (function () -> Algorithm_ctk.operation_definition_edit top (Some set_global_bindings))];
  Menu.add_command algo_menu [Label "Edit Main Definition"; Accelerator "Ctrl-Shift-D"; Command algo_main_open];
  Menu.add_separator algo_menu;    
  Menu.add_cascade algo_menu [Label "Delete Local Definition"; Menu delete_definition_submenu];
  Menu.add_separator algo_menu;
  Menu.add_command algo_menu [Label "Create Software Component"; Command (function () -> Algorithm_ctk.xsc_define top;update_xsc_delete_menu ())];
  Menu.add_cascade algo_menu [Label "Delete Software Component"; Menu delete_xsc_submenu];
  Menu.add_separator algo_menu;
  Menu.add_command algo_menu [Label "Main definition port types list"; Command (function () -> Algorithm_ctk.port_types_list_view top)];

  Menu.add_command archi_menu [Label "New Local Operator Definition"; Command (function () -> Architecture_ctk.operator_definition_new top (Some set_global_bindings);update_operator_delete_menu ())];
  Menu.add_command archi_menu [Label "Edit Operator Definition"; Command (function () -> Architecture_ctk.operator_definition_edit top (Some set_global_bindings))];
  Menu.add_cascade archi_menu [Label "Delete Local Operator"; Menu delete_operator_submenu];
  Menu.add_separator archi_menu;
  Menu.add_command archi_menu [Label "New Local Medium Definition"; Command (function _ -> Architecture_ctk.media_definition_new top (Some set_global_bindings);update_media_delete_menu ())];
  Menu.add_command archi_menu [Label "Edit Medium Definition"; Command (function _ -> Architecture_ctk.media_definition_edit top (Some set_global_bindings))];
  Menu.add_cascade archi_menu [Label "Delete Local Medium"; Menu delete_media_submenu];
  Menu.add_separator archi_menu;
  Menu.add_command archi_menu [Label "New Local Architecture"; Command (function () -> Architecture_ctk.architecture_definition_create top (Some set_global_bindings);update_architecture_delete_menu ())];
  Menu.add_command archi_menu [Label "Edit Architecture"; Command (function () -> Architecture_ctk.architecture_definition_edit top (Some set_global_bindings))];
  Menu.add_command archi_menu [Label "Edit Main Architecture"; Accelerator "Ctrl-Shift-A"; Command archi_main_open];
  Menu.add_cascade archi_menu [Label "Delete Local Architecture"; Menu delete_architecture_submenu];
  Menu.add_command constraints_menu [Label "Absolute Constraints"; Command (function () -> Xsc_ctk.absolute_constraints_choose top)];
(*   Menu.add_command constraints_menu [Label "Relative Constraints"; Command (function () -> Xsc_ctk.relative_constraints top)]; *)
  Menu.add_command constraints_menu [Label "Absolute Constraints (main architecture)"; Command (function () -> Xsc_ctk.absolute_constraints_main top)];

  Menu.add_command adeq_menu [Label "No Flatten"; Accelerator "F3"; Command (function _ -> latency_adequation No_Flatten)];
  Menu.add_command adeq_menu [Label "Flatten"; Accelerator "F4"; Command (function _ -> latency_adequation Flatten)];
  Menu.add_separator adeq_menu;

 (**************** FAULT TOLERANCE MENU ***************)
  Menu.add_command adeq_menu [Label "Fault-Tolerance";Accelerator "F7";Command(function _ -> fault_tolerance_adequation No_Flatten)];
  Menu.add_separator adeq_menu; 
 (************************ END ************************) 

  Menu.add_command adeq_menu [Label "View schedule"; Accelerator "F8"; Command (function _ -> latency_adequation_schedule_view ())];

(*  let flatten_menu = Menu.create adeq_menu [] in
    Menu.add_command flatten_menu [Label "Hierarchy and conditioning"; Command (function _ -> adequation No_Repetition_Flatten)];
    Menu.add_command flatten_menu [Label "Hierarchy, conditioning and repetition"; Accelerator "F4"; Command (function _ -> adequation Flatten)];
  Menu.add_cascade adeq_menu [Label "Flatten"; Menu flatten_menu];*)

  Menu.add_command code_menu [Label "Generate Executive"; Accelerator "F5"; Command (function _ -> generate_code top.tw_chrono)];
  Menu.add_command code_menu [Label "View Executive"; Accelerator "F6"; Command view_code];
  Textvariable.set generate_chronos_tv "0";
  Textvariable.handle generate_chronos_tv (function () ->
					     update_option_from_text_variable generate_chronos_tv
					     (fun new_val -> top.tw_chrono <- new_val));
  Menu.add_separator code_menu;
  Menu.add_checkbutton code_menu [Label "Generate Chronos"; Variable generate_chronos_tv];

  Menu.add_command debug_menu [Label "Synchronized"; Accelerator "F8"; Command (function _ -> synchronized ())];
  Menu.add_command debug_menu [Label "Transform Graph, Flatten (PostScript)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.ps top.tw_current_directory top.tw_msg_bar (Transformation.transform Flatten algolib algoname))];
  Menu.add_command debug_menu [Label "Transform Graph, Flatten (Jpeg)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.jpeg top.tw_current_directory top.tw_msg_bar (Transformation.transform Flatten algolib algoname))];
  Menu.add_command debug_menu [Label "Transform Graph, No Repetition Flatten (PostScript)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.ps top.tw_current_directory top.tw_msg_bar (Transformation.transform No_Repetition_Flatten algolib algoname))];
  Menu.add_command debug_menu [Label "Transform Graph, No Repetition Flatten (Jpeg)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.jpeg top.tw_current_directory top.tw_msg_bar (Transformation.transform No_Repetition_Flatten algolib algoname))];
  Menu.add_command debug_menu [Label "Transform Graph, No Flatten (PostScript)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.ps top.tw_current_directory top.tw_msg_bar (Transformation.transform No_Flatten algolib algoname))];
  Menu.add_command debug_menu [Label "Transform Graph, No Flatten (Jpeg)"; Command (function _ ->
    let algolib,algoname,_ = Application.algo_main_get () in
    Dot_ctk.jpeg top.tw_current_directory top.tw_msg_bar (Transformation.transform No_Flatten algolib algoname))];

  Menu.add_command help_menu [Label "User Manual (html)"; Accelerator "F1"; Command help_html];
  Menu.add_command help_menu [Label "Tutorial (html)"; Accelerator "F2"; Command tutorial_html];
  Menu.add_command help_menu [Label "User Manual (txt)"; Command help_txt];
  Menu.add_command help_menu [Label "Copyright"; Command copyright];
  Menu.add_command help_menu [Label "About"; Command about];

  Menubutton.configure file_button [Menu file_menu];
  Menubutton.configure options_button [Menu options_menu];
  Menubutton.configure algo_button [Menu algo_menu];
  Menubutton.configure archi_button [Menu archi_menu];
  Menubutton.configure constraints_button [Menu constraints_menu];
  Menubutton.configure adeq_button [Menu adeq_menu];
  Menubutton.configure code_button [Menu code_menu];
  Menubutton.configure debug_button [Menu debug_menu];
  Menubutton.configure help_button [Menu help_menu];

  pack [menu_bar] [Side Side_Top; Fill Fill_X];
  pack [infobar_frame] [Fill Fill_Both; Expand true];
  pack [msgbar_frame] [Fill Fill_X];
  pack [info_scrollbar] [Side Side_Right; Fill Fill_Y];
  pack [info_bar] [Side Side_Top; Fill Fill_Both; Expand true];
  pack [msg_scrollbar] [Side Side_Right; Fill Fill_Y];
  pack [msg_bar] [Side Side_Bottom; Fill Fill_X];
  pack [file_button] [Side Side_Left; Fill Fill_Y];
  pack [options_button] [Side Side_Left; Fill Fill_Y];
  pack [algo_button] [Side Side_Left; Fill Fill_Y];
  pack [archi_button] [Side Side_Left; Fill Fill_Y];
  pack [constraints_button] [Side Side_Left; Fill Fill_Y];
  pack [adeq_button] [Side Side_Left; Fill Fill_Y];
  pack [code_button] [Side Side_Left; Fill Fill_Y];
  pack [help_button] [Side Side_Left; Fill Fill_Y];
(*   pack [debug_button] [Side Side_Left; Fill Fill_Y];  *)
  pack [frame] [Fill Fill_Both; Expand true];

  bind frame [[],Destroy] (BindExtend ([], (function _ -> ignore(save_changed_file ());close_application ())));

  let current_directory_update d = top.tw_current_directory <- d in
  let binary_directory_update d = binary_directory := d; current_directory_update (Filename.concat d "..")
  and library_directory_update d = Read.library_directory := d
  and html_program_update d = Ihmcommon_ctk.html_program := d
  and dot_program_update d = Dot_ctk.dot_program := d
  and gs_program_update d = Dot_ctk.gs_program := d; Graph_ctk.gs_program := d
  and version () =
    print_string application_msg;
    quit () in
  let keywords = ["-app",Arg.String current_directory_update, " specifies the application directory";
		  "-bin", Arg.String binary_directory_update, "  specifies the bin directory";
		  "-libs", Arg.String library_directory_update, " specifies the libraries directory";
		  "-html", Arg.String html_program_update, " specifies the html browser";
		  "-dot", Arg.String dot_program_update, "  specifies the dot directory";
		  "-gs", Arg.String gs_program_update, "   specifies the gs directory";
		  "-v", Arg.Unit version, "    SynDEx version"] in
  Arg.parse keywords (function name -> current_directory_update (Filename.dirname name); open_file_name name) (application_msg^"\nUsage: syndex [options] file\nOptions:");
  update_menus ();

    (benchs 1)
   
let () =
  if (not !Sys.interactive)
  then
    try
      top_win_create ();
      mainLoop ()  
    with exn -> 
      print_string ("Error : "^(Printexc.to_string exn)^"\n");
      exit 0
  else
    print_string ("\t" ^ syndex_version_msg ^ " (Interactive)\n")
