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

module Dogan_adequation = Dogan_adequation.Make (Progress_box.Tk)

module Reliability_adequation = Reliability_adequation.Make (Progress_box.Tk)

module Reliability_adequation_no_routing = Reliability_adequation_no_routing.Make (Progress_box.Tk)

module Fault_tolerance_adequation = Fault_tolerance_adequation.Make (Progress_box.Tk)

let read_error_message_box frame filename line message =
  ok_message_box frame
    ((Filename.basename filename) ^": " ^ (string_of_int line) ^ ":\n" ^
     message ^ "\n")

let adequation_graph = ref (None, None)

let view = ref false

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
   (*and adeq_fault_submenu = Menu.create adeq_menu [TearOff false]
   and adeq_nofault_submenu = Menu.create adeq_menu [TearOff false] *) in 
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
      [],"F3", (function _ -> latency_adequation "" No_Flatten);
      [],"F4", (function _ -> latency_adequation "" Flatten); 
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

 (* and choose_lib_dir () =
    () *)

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
	      
  and schedule_view schedule_info_function heuristic_name =
    match !adequation_graph,!adequation_in_progress with
    | (Some graph,_),false ->
	let (algomain_lib,algomain_name,_) = Application.algo_main_get () in
	let (archimain_lib,archimain_name) = Application.archi_main_get () in
	  (try                 
	    let schedule_info = schedule_info_function graph in               
	    let title = "   "^heuristic_name^" :"^"     Adequation "^(string_of_ref_type algomain_lib algomain_name)^" onto "^
			(string_of_ref_type archimain_lib archimain_name) in                 
	      Schedule_ctk.schedule_view top title schedule_info set_global_bindings
	   with Failure msg -> error_message top msg)
    | _ -> ()

  and latency_adequation reliability_file_name adeqtype =
    adequation adeqtype (Latency_adequation.adequation reliability_file_name)

  and latency_adequation_schedule_view () =
     schedule_view Adequation_core.pretty_conv_graph "SynDEx"

  (************************************)
  (******** FAULT TOLERANCE ***********)
  (************************************)
 
  and fault_tolerance_adequation adeqtype = 
    (match Xsc_ctk.faults_data_choose top with
       | false,_,_,_    -> () 
       | true,npf,nmf,check ->  (
     Schedule_ctk.reliability_cost := "";
             Fault_tolerance_adequation.split_data := check;  
             Fault_tolerance_adequation.failures := 
             {Fault_tolerance_adequation.npf=npf; Fault_tolerance_adequation.nmf=nmf}; 
             adequation adeqtype (Fault_tolerance_adequation.fault_tolerance_adequation));)

(*  and fault_tolerance_schedule_view () =
    schedule_view Adequation_core.pretty_conv_graph

  and fault_tolerance_generate_code chrono =
    text_clean top.tw_msg_bar;
    message_info top.tw_msg_bar "Fault Tolerant Executive Generation";
    match !adequation_graph with
    | None,_ -> error_message top "Do the Fault Tolerance Adequation first"
    | (Some graph),_ ->
	let archilib,archiname = Application.archi_main_get () in
	match Architecture.operator_main_get archilib archiname with
	| "" -> error_message top "No main operator defined"
	| _ -> () *)
	    (*
               let application_name = (match top.tw_current_file with
	       | None -> ""
	       | Some name -> name) in
	       try ()
	       (*Fault_tolerance_genexec.generate_code application_name graph chrono*)
	       with Failure s -> error_message top s*)

  (*******************************************)
  (********** End Fault Tolerance  ***********)
  (*******************************************)


  (************************************)
  (******** RELIABILITY     ***********)
  (************************************)
(*  and reliability_adequation adeqtype file_name =
    (*let level_max = 1 in                     
       Reliability_adequation.level_max := level_max;*)    
       Reliability_adequation.file_name := file_name;     
       adequation adeqtype (Reliability_adequation.reliability_adequation);  
       schedule_view Reliability_adequation.pretty_conv_graph *)

(*  and reliability_schedule_view () =
    schedule_view Reliability_adequation.pretty_conv_graph*)


and pss s =
  print_string (s^"\n");
  flush stdout

(********************************************************************) 
(**************               SIMULATION               **************) 
(********************************************************************)
and simulation_ccr () = 
    let what_sim = "length_overhead_routing" in
    let dir = ("/scratch/kalla/Benchmarks/Results/"^what_sim^"/media-10e3/")  in   
    let dir_graphs = "/scratch/kalla/Benchmarks/" in 
    let dir_rel    = "/scratch/kalla/Benchmarks/SynDEx-no-routing/file-rel/" in  
    let readme = dir^"SimParameters" in 
    let out_readme = open_out readme in
    output_string out_readme ("\n\n AAA-F :\n\n");  
    output_string out_readme ("\n\n length overhead :\n\n");  
    output_string out_readme (" p = 4  \n ccr = [1.0] \n N = 10:50 (x100graphs) \n  rep max = 2 (donc 3)");  
    close_out out_readme;   
    let p = ref 0 in  
    let tab_p = [|4;6|] in  (* NUMBER OF PROCESSORS *) 
    for ind_p = 1 to 1 do  
      p := tab_p.(ind_p);        
      let archi_name = ("arc"^(string_of_int !p)) in  
      (* algorithm graphs CCR  *) 
      let ccr = ref 0. in  
      let tab_ccr = [| 0.1; 1.0; 10.0 |] in        
      for ind_ccr = 1 to 1 do  
	ccr := tab_ccr.(ind_ccr);      
	(* file for procs rate replication replication_process_proc *)
      	let aaaf_rate_proc = (dir^"rep_proc_aaaf_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in   
        let out_aaaf_rate_proc = open_out aaaf_rate_proc in     
	(* file for mean replication rate of each graph *)
      	let aaaf_rep = (dir^"rep_aaaf_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in   
        let out_aaaf_rep = open_out aaaf_rep in     
	(* files for schedule length *)
 	let aaa_len = (dir^"len_aaa_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in   
        let aaaf_len  = (dir^"len_aaaf_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in 
        let out_aaaf_len = open_out aaaf_len in    
        let out_aaa_len = open_out aaa_len in     
	(* files for schedule failure rate *)
        let aaaf_lam  = (dir^"lam_aaaf_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in 
        let aaa_lam   = (dir^"lam_aaa_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in 
        let out_aaaf_lam = open_out aaaf_lam in    
        let out_aaa_lam = open_out aaa_lam in    
	(* algorithm graphs size  *) 
	for i = 5 to 5 do  
 	  let size =  i * 10 in
	  (* file for mean replication rate of each operation of all redundant graphs *)
      	  let aaaf_rep_process = (dir^"rep_aaaf_opn-"^(string_of_int size)^"_ccr-"^(string_of_float !ccr)^"_p-"^(string_of_int !p)) in   
          let out_aaaf_rep_process = open_out aaaf_rep_process in     
	  output_string out_aaaf_rep_process (string_of_int size);
	  output_string out_aaaf_rate_proc (string_of_int size);	  
	  output_string out_aaaf_rep (string_of_int size);
	  output_string out_aaa_len (string_of_int size); output_string out_aaaf_len (string_of_int size);
	  output_string out_aaa_lam (string_of_int size); output_string out_aaaf_lam (string_of_int size);    
          let dir_opr=(dir_graphs^"Random_graphs/parallelisme-8/p"^(string_of_int !p)^"/t"^(string_of_int size)^"/")in                
	  let median_replication = Array.make size [] in
	  let rep_procs_median = Array.make 6 0. in
	  let nb_red = 50 in
          for j = 1 to nb_red do 
	    let algo_name = ("alg"^(string_of_int size)^"_red"^(string_of_int j)) in
            pss "-----------------------------------------------";
	    let name     = (dir_opr^algo_name^"_ccr"^(string_of_float !ccr)^"_"^archi_name^".sdx") in
	    let name_rel = (dir_rel^(string_of_int !p)^"-proc-heterogene-media-10e3.rel") in
	    pss name;                           
	    open_file_name_simulation name;              
            (***  AAA  ***)
            (*adequation No_Flatten (Latency_adequation.adequation name_rel);         
	    let len = !Latency_adequation.schedule_length in 
	    let lambda = !Latency_adequation.schedule_big_lambda in  
	    pss ("\n AAA :       L = "^(string_of_float  len)^"       /\\ = "^(string_of_float  lambda));*)
            (*** AAA-F ***)  
	    let rep =  3 (*!p-1*) in (* for p=6 we have fixed rep to 3 *)     
	     for index = 2 to 7 do   (* 2 to 7 *)             
	       let lambda =  (10. **(-. (float_of_int index)))  in                	                     	       
	       (*output_string out_aaa_len  (" "^(string_of_float !Latency_adequation.schedule_length));  
	         output_string out_aaa_lam  (" "^(string_of_float lambda));*)

               adequation No_Flatten (Reliability_adequation.adequation max_float lambda 45. rep false name_rel);
	       output_string out_aaaf_len  (" "^(string_of_float !Reliability_adequation.schedule_length)); 
	       output_string out_aaaf_lam  (" "^(string_of_float !Reliability_adequation.schedule_big_lambda)); 
	       output_string out_aaaf_rep  (" "^(string_of_float !Reliability_adequation.rate_replication));

	       pss ("\n AAA-F-ok-routing("^(string_of_float (!Reliability_adequation.rate_replication))
                    ^")      L     = "^(string_of_float  !Reliability_adequation.schedule_length)
		    ^"     /\\*    = "^(string_of_float  lambda)
                    ^"     /\\     = "^(string_of_float  !Reliability_adequation.schedule_big_lambda)^"%");

	       adequation No_Flatten (Reliability_adequation_no_routing.adequation max_float lambda 45. rep false name_rel);
	       output_string out_aaa_len  (" "^(string_of_float !Reliability_adequation_no_routing.schedule_length)); 
	       output_string out_aaa_lam  (" "^(string_of_float !Reliability_adequation_no_routing.schedule_big_lambda));

	       pss (" AAA-F-no-routing("^(string_of_float (!Reliability_adequation.rate_replication))
                    ^")      L     = "^(string_of_float  !Reliability_adequation_no_routing.schedule_length));
  	       
	       Hashtbl.iter (fun opr (ind,rate,nb_opn) ->   
			       rep_procs_median.(ind-1) <- rep_procs_median.(ind-1) +. ((float_of_int rate)/.(float_of_int nb_opn));                  
			    )  Adequationtypes.replication_process_proc;

	     
	       let index = ref 0 in
	       List.iter (fun rep -> median_replication.(!index) <-  rep::median_replication.(!index);index := !index+1;
			 ) !Reliability_adequation.replication_process;	        
	     done;
	    (*** write results to files ***)
	    flush out_aaaf_len;flush out_aaaf_lam; flush out_aaaf_rep; 
	    flush out_aaa_len; flush out_aaa_lam; 
	    ignore(close_file ());
          done;

	  Array.iter (fun rep_proc -> (*ps (string_of_float (rep_proc/.5.));*)output_string out_aaaf_rate_proc (" "^(string_of_float (rep_proc/.50.)))
		     ) rep_procs_median;
  
	  Array.iter (fun rep_values -> output_string out_aaaf_rep_process (" "^(string_of_float (Adequationtypes.get_average rep_values)))
		     ) median_replication;
	  output_string out_aaaf_rep_process "\n";flush out_aaaf_rep_process; close_out out_aaaf_rep_process;
	  output_string out_aaa_len "\n"; output_string out_aaaf_len "\n";	  
	  output_string out_aaa_lam "\n"; output_string out_aaaf_lam "\n";
	  output_string out_aaaf_rep "\n";  
	done;    
	close_out out_aaa_len; close_out out_aaa_lam; 
	close_out out_aaaf_len;close_out out_aaaf_lam; close_out out_aaaf_rep; 
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
      (* ............... *)
    Unix.closedir dir; 
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

(* let ask_save () =
    match dialog_create syndex_version_msg "Save current application ?" 0 ["Yes" ;"No"] with
    | 0 -> save_file ()
    | _ -> ()   

  and *) let  quit () =
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

  Menu.add_command adeq_menu [Label "No Flatten"; Accelerator "F3"; Command (function _ -> latency_adequation "" No_Flatten)];
  Menu.add_command adeq_menu [Label "Flatten"; Accelerator "F4"; Command (function _ -> latency_adequation "" Flatten)];
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

view := false;
(match true with
| true -> simulation_ccr ();
| false -> let name,name_rel = 
              match false with
	      | true ->  "/scratch/kalla/Benchmarks/Random_graphs/p4/t20/alg20_red1_ccr10._arc4.sdx",
 		         "/home/popart/kalla/SynDExprocs-heterogene.rel"; 
	      | false -> "/home/popart/kalla/SynDEx/file-sdx/sampp.sdx", 
                         "/home/popart/kalla/SynDEx/file-rel/4-proc-heterogene-media-10e3.rel" in
  ps name;
  open_file_name_simulation name;                                 
  (**********************************) 
  (************    AAA     **********) 
  adequation No_Flatten (Latency_adequation.adequation name_rel);        
  pss ("\n AAA : \n      L = "^(string_of_float  !Latency_adequation.schedule_length)       
       ^";\n      R = "^(string_of_float  !Latency_adequation.schedule_reliability)
       ^";\n     /\\ = "^(string_of_float  !Latency_adequation.schedule_big_lambda)); pss "";     
  if !view then schedule_view Reliability_adequation.pretty_conv_graph "AAA";

  (************************************)
  (************    AAA-F     **********)
  let r = 3  in       
  let schedule_length      =  !Latency_adequation.schedule_length  in   
  let schedule_big_lambda  = (!Latency_adequation.schedule_big_lambda) in 
                             
  adequation No_Flatten (Reliability_adequation.adequation schedule_length schedule_big_lambda 45. r false name_rel);
  pss ("\n AAA-F ("^(string_of_float !Reliability_adequation.theta)^","^(string_of_float !Reliability_adequation.rate_replication) 
    ^")\n      L = "^(string_of_float  !Reliability_adequation.schedule_length)   
    ^"\n      R = "^(string_of_float   !Reliability_adequation.schedule_reliability)     
    ^"\n     /\\ = "^(string_of_float  !Reliability_adequation.schedule_big_lambda)^"% \n");      
  if !view then schedule_view Reliability_adequation.pretty_conv_graph "AAA Reliability";

  adequation No_Flatten (Reliability_adequation_no_routing.adequation schedule_length  schedule_big_lambda 45. r false name_rel);
  pss ("\n AAA-F ("^(string_of_float !Reliability_adequation_no_routing.theta)^","^(string_of_float !Reliability_adequation_no_routing.rate_replication) 
    ^")\n      L = "^(string_of_float  !Reliability_adequation_no_routing.schedule_length)^"% \n");      
  if !view then schedule_view Reliability_adequation_no_routing.pretty_conv_graph "AAA Reliability"; 

  (*********  PARETO GRAPH  *********)
  (match false with
  |  false -> ()
  |  true ->
      let res_lam_aaa = ref  "lam  = [ " in
      let res_lam_aaaf = ref "lam1 = [ " in
      let res_len = ref "len  = [ " in  
      let res_rel = ref "rel  = [ " in  
      let res_rep = ref "rep  = [ " in  
       
      for i= 62 downto 0 do  
	let lam = [|
	  0.000000001;0.000000002;0.000000003;0.000000004;0.000000005;0.000000006;0.000000007;0.000000008;0.000000009; 
	  0.00000001; 0.00000002; 0.00000003; 0.00000004; 0.00000005; 0.00000006; 0.00000007; 0.00000008; 0.00000009; 
	  0.0000001;  0.0000002;  0.0000003;  0.0000004;  0.0000005;  0.0000006;  0.0000007;  0.0000008;  0.0000009; 
	  0.000001;   0.000002;   0.000003;   0.000004;   0.000005;   0.000006;   0.000007;   0.000008;   0.000009; 
	  0.00001;    0.00002;    0.00003;    0.00004;    0.00005;    0.00006;    0.00007;    0.00008;    0.00009;
	  0.0001;     0.0002;     0.0003;     0.0004;     0.0005;     0.0006;     0.0007;     0.0008;     0.0009;
          0.001;      0.002;      0.003;      0.004;      0.005;      0.006;      0.007;      0.008;      0.009|] in

  	lam.(i) <- (10. **(float_of_int (- (i/2)))) +. (0.5 *. (float_of_int ((i+1) mod 2))  *. (10. **(float_of_int (- ((i+1)/2)))));

	let new_lambda =  lam.(i)  in  

	ps ("try lambda = "^(string_of_float new_lambda )); 
	adequation No_Flatten (Reliability_adequation.adequation schedule_length new_lambda 45. r false name_rel);  
	match !Reliability_adequation.schedule_failure with
	| true  -> ()
	| false -> 
	    res_lam_aaa  := (!res_lam_aaa^"    "^(string_of_float lam.(i))); 
	    res_lam_aaaf := (!res_lam_aaaf^"    "^(string_of_float !Reliability_adequation.schedule_big_lambda)); 
	    res_len := (!res_len^"    "^(string_of_float !Reliability_adequation.schedule_length));  
	    res_rel := (!res_rel^"    "^(string_of_float !Reliability_adequation.schedule_reliability)); 
	    res_rep := (!res_rep^"    "^(string_of_float !Reliability_adequation.rate_replication));  
      done; 
      res_lam_aaa := (!res_lam_aaa^" ];");res_lam_aaaf := (!res_lam_aaaf^" ];");
      res_len := (!res_len^" ];");res_rel := (!res_rel^" ];"); res_rep := (!res_rep^" ];");  
      ps !res_lam_aaa;
      ps !res_lam_aaaf;  
      ps !res_rel; 
      ps !res_len; 
      ps !res_rep;ps "";ps "";);   
  (************************************) 
  (************     DOGAN    **********
  for ind = 1 to 1 do 
    adequation No_Flatten (Dogan_adequation.adequation name_rel ind);   
    pss ("\n Dogan : \n      L = "^(string_of_float !Dogan_adequation.schedule_length)^"\n      R = "^       
           (string_of_float  !Dogan_adequation.schedule_reliability)); pss "";       
  (* schedule_view Adequation_core.pretty_conv_graph "Dogan";*)
  done;  
  ************************************)
  (************   AAA TP/TB  **********)
  (*Fault_tolerance_adequation.failures := {Fault_tolerance_adequation.npf=0; Fault_tolerance_adequation.nmf=0}; 
    Fault_tolerance_adequation.file_name := name_rel;           
    adequation No_Flatten (Fault_tolerance_adequation.fault_tolerance_adequation);  
    pss (" AAA-TP ("^(string_of_int  (Fault_tolerance_adequation.get_npf 0))^","^  
    (string_of_int  (Fault_tolerance_adequation.get_nmf 0))^")\n      L = "^
    (string_of_float  !Fault_tolerance_adequation.old_len)^"\n      R = "^  
    (string_of_float  !Fault_tolerance_adequation.old_rel));pss ""; 
    *schedule_view Fault_tolerance_adequation.pretty_conv_graph "AAA TP";*)      
  
);
()

let () =
  if (not !Sys.interactive)
  then
    try
      top_win_create (); if !view then mainLoop () else () 
    with exn -> 
      print_string ("Error : "^(Printexc.to_string exn)^"\n");
      exit 0
  else
    print_string ("\t" ^ syndex_version_msg ^ " (Interactive)\n")





