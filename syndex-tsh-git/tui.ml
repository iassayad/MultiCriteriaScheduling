(** This module provides a command line mode for SynDEx. This command
allows to load a file, perform an adequation on it, save the result and
generate executive *)

open Types

module Adequation = Latency_adequation.Make (Progress_box.Dummy)

let info_msg s =
  print_string s
    
let error_msg s =
  failwith ("\nERROR: "^s)
    
(** Returns the adequation of current application *)
let adequation () =
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
      Adequation.adequation graph

(** [generate_executive app_name graph genexec_chrono] generates
executive in app_name directory for adequation graph [graph] *)
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
    | None -> let g, s = adequation () in
      Some g, Some s in
  Write.application_save output_file (true, (graph,schedules));
  (match genexec, graph with
  | true, Some g -> generate_executive input_file g genexec_chrono
  | _ -> ())

(** Main. Analyzes command arguments an initiates process. *)
let _ =    
  let output_file = ref "" in
  let output_file_set file_name =
    output_file := file_name in
  let genexec = ref false in
  let genexec_enable _ =
    genexec := true in
  let genexec_chrono = ref false in
  let genexec_chrono_enable _ =
    genexec_enable ();
    genexec_chrono := true in

  let keywords =
    ["-o", Arg.String output_file_set,
     "out_file\t Specify the output file (default output is <in_file_base>_adeq.sdx)";
      "-gxc", Arg.Unit genexec_enable,
      "\t Generate executive (in in_file directory)";
      "-gxc-chr", Arg.Unit genexec_chrono_enable,
      "\t Generate executive with chronos";
     "-v", Arg.Unit version, "\t SynDEx version";
     "-libs", Arg.String library_directory_update,
     "\t Specify the libraries directory"] in

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
    (application_msg^"\nThis command loads a SynDEx application, performs its adequation,\n"^
     "saves its result and optionnally generates executive.\n"^
     "\nUsage: syndex-tui [options] in_file\nOptions:")
