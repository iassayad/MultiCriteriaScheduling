(** This module provides a command line mode for SynDEx. This command
allows to load a file, perform an adequation on it, save the result and
generate executive *)

open Types

(* Here we choose to display the progress of the adequation in a tk mode *)
module Latency_adequation = Latency_adequation.Make (Progress_box.Tk)

module Reliability_adequation = Reliability_adequation.Make (Progress_box.Tk)

module Fault_tolerance_adequation = Fault_tolerance_adequation.Make (Progress_box.Tk)


let info_msg s =
  print_string s
    
let error_msg s =
  failwith ("\nERROR: "^s)
    
(** Returns the adequation of current application *)
let adequation old =
  let (algomain_lib,algomain_name,_),(archimain_lib,archimain_name) =
    Application.algo_main_get (), Application.archi_main_get () in
    match ((algomain_lib,algomain_name),(archimain_lib,archimain_name)) with
      | ("",""),_ -> error_msg "No main algorithm defined"
      | _,("","") -> error_msg "No main architecture defined"
      | (algmain_lib,algmain_name),_ ->
	  let graph = Transformation.transform Flatten algmain_lib algmain_name in
	  let warnings = Transformation.consistancy_check graph in
	  let warning_str = string_of_string_list warnings "\n\n" in
	    (match warning_str with
	       | "" -> ()
	       | _ -> info_msg ("WARNING;\n"^warning_str));

	    (*Adequation.adequation graph*)
	    Fault_tolerance_adequation.split_data := false;
            Fault_tolerance_adequation.failures := {Fault_tolerance_adequation.npf=1; Fault_tolerance_adequation.nmf=0};
            Fault_tolerance_adequation.fault_tolerance_adequation graph

	    (** [generate_executive app_name graph genexec_chrono]
	      generates executive in app_name directory for adequation
	      graph [graph] *)
let generate_executive app_name graph genexec_chrono =
  let archilib,archiname = Application.archi_main_get () in
    match Architecture.operator_main_get archilib archiname with
      | "" -> error_msg "No main operator defined"
      | _ -> Genexec.generate_code app_name graph genexec_chrono

(** [library_directory_update d] sets library directory to [d]. *)
let library_directory_update d =
  Read.library_directory := d

(** Prints SynDEx version. *)
let version () =
  print_string application_msg

(** [process input_file output_file genexec genexec_chrono] loads
   input_file, performs adequation, saves result in output file and
   generates executive if genexec. *)
let process input_file output_file genexec genexec_chrono =
  let loaded_graph = Read.open_syndex_file input_file in
    (* This here is BAD. It must be changed. You won't be able to resave
       an adequation you've just loaded. *)
  let graph, schedules =
    match loaded_graph with
      | Some g -> Some g, None
      | None -> 
	  let g, s = adequation true in
	    Some g, Some s in 

    ps "   Adequation :";   
    ps (" length =  "^(string_of_float !Fault_tolerane_adequation.len))




let simulations =
  let in_f = "/usr/local/kalla/Reliability/v6.2.2/DSN.sdx" in
  let out_f = (Filename.chop_extension in_f)^"_adeq.sdx" in
  let loaded_graph = Read.open_syndex_file in_f in
  let old = false in 
  let graph, schedules = 
    match loaded_graph with
      | Some g ->  Some g, None
      | None -> 
          Hashtbl.clear  Reliability.failure_rates; 
          Reliability.initialize_failure_rates_table  "/usr/local/kalla/Reliability/v6.2.2/DSN.sdx";	 
	  Args.obj_rel := 0.9999;
	  Args.obj_len := 20.0;
	  Args.theta := 90.0;
	  let g, s = adequation old in 
	    Some g, Some s in

    match old with 
      | true ->
          Write.application_save out_f (true, (graph,schedules));
	  ps "   New Adequation_reliability with arbre :";    
	  ps (" length =  "^(string_of_float !Reliability.last_len));
	  ps (" reliability = "^(string_of_float !Reliability.last_rel))
      | false ->
          Write.application_save out_f (true, (graph,schedules));
	  ps "   OLD Adequation :";   
	  ps (" length =  "^(string_of_float !Reliability.last_len));
	  ps (" reliability = "^(string_of_float !Reliability.last_rel))
     






  

 (* Write.application_save output_file (true, (graph,schedules));
  (match genexec, graph with
  | true, Some g -> generate_executive input_file g genexec_chrono   
  | _ -> ()) *)

(** Main. Analyzes command arguments an initiates process. *
let _ =    
  let output_file = ref "" in 
  let output_file_set file_name =
    output_file := file_name in
  let genexec = ref false in
  let genexec_enable _ =
    genexec := true in
  let genexec_chrono = ref false in
  let genexec_chrono_enable _ =
    genexec_chrono := true in

  let keywords =
    ["-o", Arg.String output_file_set,
     "out_file\t specifies the output file (default output is <in_file_base>_adeq.sdx)";
      "-gxc", Arg.Unit genexec_enable,
      "\t generates executive (in in_file directory)";
      "-gxc-chr", Arg.Unit genexec_chrono_enable,
      "\t generates executive with chronos";
     "-v", Arg.Unit version, "\t SynDEx version";
     "-libs", Arg.String library_directory_update,
     "\t specifies the libraries directory"] in

  Arg.parse keywords
    (fun in_f ->
      let out_f =
	match !output_file with
	| "" ->
	    let basename =
	      try
		Filename.chop_extension in_f
	      with _ -> in_f in
	    basename^"_adeq.sdx"
	| s -> s in
      match Sys.file_exists in_f with
      | true -> process in_f out_f !genexec !genexec_chrono
      | false -> error_msg ("File "^in_f^" doesn't exist.\n"))
    (application_msg^"\nUsage: syndex-tui [options] in_file\nOptions:") *)
