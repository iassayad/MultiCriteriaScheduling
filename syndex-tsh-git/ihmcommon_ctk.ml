(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                             Julien Forget                             *)
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

(** Contains data structure and basic function to handle SynDEx ihm, such as generic windows, message_boxes, etc.*)

(** Type of algorithm definition graph vertices. *)
type algo_vertice_type = VReference | VPort
(** Type of architecture graph vertices. *)
type archi_vertice_type = VOperator | VMedia
(** Type of architecture graph edges. *)
type archi_edge_type = EConnection
(** Type of schedule graph vertices *)
type schedule_vertice_type = VOperation | VOperator_schedule
(** Type of elements of a generic window for options input (see function options_box). *)
type options_box_type = Radiobutton | Checkbutton | Field of bool
(** Mediocre solution to identify opened windows. The identifier is used to avoid opening/try closing the same windows multiple times. *)
type window_type =
  | WAlgorithm of (string list) (* algo path *)
  | WArchitecture of (string*string) (* arc lib, arc name *)
  | WOperator of (string*string)
  | WMedia of (string*string)
  | WAbsoluteConstraints of (string*string)
  | WRelative
  | WAsk_edit_definition
  | WAsk_edit_operator
  | WAsk_edit_media
  | WAsk_edit_architecture
  | WAsk_constraints
  | WAsk_definition_reference_add of (string list)
  | WAsk_operator_reference_add of (string*string)
  | WAsk_media_reference_add of (string*string)
  | WAsk_port_add of (string list)
  | WAsk_definition_modify of (string list)
  | WAsk_definition_reference_name of (string list)
  | WAsk_definition_reference_modify of (string list*string) (* algo path, reference name *)
  | WAsk_definition_description_modify of (string list)
  | WAsk_reference_description_modify of (string list)
  | WAsk_port_modify of (string list*(string* Port.direction)) (* algo path, port name & dir *)
  | WAsk_condition_add of (string list)
  | WAsk_create_xsc
  | WAsk_create_definition
  | WAsk_create_operator
  | WAsk_create_media
  | WAsk_create_architecture
  | WAsk_operator_reference_name of (string*string) 
  | WAsk_media_reference_name of (string*string) 
  | WAsk_operator_reference_modify of (string*string*string) (* arc lib, arc name, op name *)
  | WAsk_media_reference_modify of (string*string*string) (* arc lib, ac name, media name *)
  | WSelect_reference of (string list)
  | WSelect_port of (string list)
  | WSelect_operator_reference of string
  | WSelect_medium_reference of string
  | WSelect_sched_opn
  | WPort_types

(** Schedule display direction. *)
type schedule_direction_type = VerticalSchedule | HorizontalSchedule

(** Contains bindings. *)
and bindcommand_f = Widget.widget -> unit
      
(** Type of an algorithm definition window. *)
and ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_algo = {
  alg_top : ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_top; (* link to the top application window *)
  alg_widget : Widget.widget;
  alg_bar_cond : Widget.widget;
  alg_library : string;
  mutable alg_name : string;
  mutable alg_path : string list;
  alg_menus : (string*Widget.widget) list;
  mutable alg_dimension : int*int;
  mutable alg_variable : string; (* Conditioning variable *)
  mutable alg_values : int list; (* Possible conditioning values *)
  mutable alg_current_value : int; (* Current conditioning value being displayed *)
  mutable alg_buttons : (int * Widget.widget) list;
  alg_bindcommand : bindcommand_f option
} 
    
(** Type of an architecture window. *)
and ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_archi = {
  arc_top : ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_top; (* link to the top application window *)
  arc_widget : Widget.widget;
  arc_library : string;
  arc_name : string;
  arc_menus : (string*Widget.widget) list;
  mutable arc_dimension : int*int;
  arc_bindcommand : bindcommand_f option
}

(** Type of a window displaying an adequation produced schedule. *)
and ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_schedule = {
  sch_top : ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_top; (* link to the top application window *)
  sch_widget : Widget.widget;
  sch_menus : (string*Widget.widget) list;
  mutable sch_direction : schedule_direction_type;
  mutable sch_arrows : bool;
  mutable sch_labels : bool;
  mutable sch_fixed_colors : bool;
  mutable sch_operators_positions : ((string*schedule_direction_type)*int) list;
  mutable sch_scale : bool;
  mutable sch_show_constants : bool
}

(** List of elements kept by the last cut/copy command in an algorithm definition window. *)
and algo_clip = ((string*string*string*(Symbolic.expression list)* Coord.coord2d*repetition_type) list)*((string*Port.direction*string*Symbolic.expression*Coord.coord2d) list)
(** List of elements kept by the last cut/copy command in an architecture window. *)
and archi_clip = (string * string * string * bool * Coord.coord2d) list * (string * string * string * Coord.coord2d) list

(** Type of a SynDEx top application window. *)
and ('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_top = {
  tw_frame : Widget.widget;
  tw_info_bar : Widget.widget;
  tw_msg_bar : Widget.widget;
  mutable tw_current_directory : string;
  mutable tw_current_file : string option;
  mutable tw_chrono : bool;
  mutable tw_algos : ((('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_algo)*(('alg_v,'alg_e) Graph_ctk.graph)) list;
  mutable tw_archis : ((('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_archi)*(('arc_v,'arc_e) Graph_ctk.graph)) list;
  mutable tw_schedules : ((('alg_v,'alg_e,'arc_v,'arc_e,'sch_v,'sch_e) win_schedule)*(('sch_v,'sch_e) Graph_ctk.graph)) list;
  mutable tw_codes : Widget.widget list;
  mutable tw_windows : (window_type*Widget.widget) list;
  mutable tw_file_changed : unit -> unit;
  (* Clipboard stuff *)
  mutable tw_alg_clip : algo_clip;
  mutable tw_arc_clip : archi_clip;
  tw_alg_graphclip : ('alg_v,'alg_e) Graph_ctk.clipboard;
  tw_arc_graphclip : ('arc_v,'arc_e) Graph_ctk.clipboard;
}

(** Information on the object pointed by the mouse used to print an information string in main window. *)
and info_type =
    (* Algo *)
  | Port of (string*Port.direction) 
  | Reference of string 
  | Dependence of ((string*algo_vertice_type*string)*(string*algo_vertice_type*string)*Types.dependence_class) 
      (* Archi *)
  | Operator of (string*string*string)
  | Media of (string*string*string)
  | MediaDefinition of (string*string)
  | Connection of (string*string*string)
      (* Schedule *)
  | Task of string
  | Ecu of string
      (* All *)
  | Canvas
  | Clean

(* Mainly used for debug purpose. Prints the window type of a window which is used as window identifier. *)
let window_from_type wintype = 
  match wintype with
  | WAlgorithm _ -> "WAlgorithm"
  | WArchitecture _ -> "WArchitecture"
  | WOperator _ -> "WOperator"
  | WMedia _ -> "WMedia"
  | WAbsoluteConstraints _ -> "WAbsoluteConstraints"
  | WRelative -> "WRelative"
  | WAsk_edit_definition -> "WAsk_edit_definition"
  | WAsk_edit_operator -> "WAsk_edit_operator"
  | WAsk_edit_media -> "WAsk_edit_media"
  | WAsk_edit_architecture -> "WAsk_edit_architecture"
  | WAsk_constraints -> "WAsk_constraints"
  | WAsk_definition_reference_add _ -> "WAsk_definition_reference_add"
  | WAsk_operator_reference_add _ -> "WAsk_operator_reference_add"
  | WAsk_media_reference_add _ -> "WAsk_media_reference_add"
  | WAsk_port_add _ -> "WAsk_port_add"
  | WAsk_definition_reference_name _ -> "WAsk_definition_reference_name"
  | WAsk_definition_modify _ -> "WAsk_definition_modify"
  | WAsk_definition_reference_modify _ -> "WAsk_definition_reference_modify"
  | WAsk_definition_description_modify _ -> "WAsk_definition_description_modify"
  | WAsk_reference_description_modify _ -> "WAsk_reference_description_modify"
  | WAsk_port_modify _ -> "WAsk_port_modify"
  | WAsk_condition_add _ -> "WAsk_condition_add"
  | WAsk_create_xsc -> "WAsk_create_xsc"
  | WAsk_create_definition -> "WAsk_create_definition"
  | WAsk_create_operator -> "WAsk_create_operator"
  | WAsk_create_media -> "WAsk_create_media"
  | WAsk_create_architecture -> "WAsk_create_architecture"
  | WAsk_operator_reference_name _ -> "WAsk_operator_reference_name"
  | WAsk_media_reference_name _ -> "WAsk_media_reference_name"
  | WAsk_operator_reference_modify _ -> "WAsk_operator_reference_modify"
  | WAsk_media_reference_modify _ -> "WAsk_media_reference_modify"
  | WSelect_reference _ -> "WSelect_reference"
  | WSelect_port _ -> "WSelect_port"
  | WSelect_operator_reference _ -> "WSelect_operator_reference"
  | WSelect_medium_reference _ -> "WSelect_medium_reference"
  | WSelect_sched_opn -> "WSelect_sched_opn"
  | WPort_types -> "WPort_types"

(* Constants and options *)
let clean_info_bar = ref true

let default_autopos_step = 60

let font = "Helvetica -12"
let underlined_font = font^" underline"

let html_program = ref ""

let background_color = NamedColor "gray85"
let operator_color = NamedColor "SkyBlue1"
let media_color = NamedColor "SkyBlue3"
let constant_color = NamedColor "AquaMarine2"
let memory_color = NamedColor "MediumSeaGreen"
let operation_color = NamedColor "SkyBlue1"
let actuator_color = NamedColor "IndianRed3"
let sensor_color = NamedColor "IndianRed1"
let other_color = NamedColor "SpringGreen"
let conditioningport_color = NamedColor "khaki"
let initmemoryport_color = NamedColor "DarkSeaGreen"
let highlight_color = Red
let select_color = Red

let canvas_size_default_x = 200
let canvas_size_default_y = 200
let border = 3
let port_space = 8
let menu_size_max = 20

(* Schedule config *)
(* fst -> Horizontal mode | snd -> Vertical mode *)
let sch_spacing = 25,70
let sch_size = 20,60
let sch_opspace = 20,20
let sch_ratio = 20,10

(* Tags creation stuff *)
let tag_buffer = ref 0
let tag_create () = incr tag_buffer; "TAG" ^ (string_of_int !tag_buffer) 

let is_initialised_tk () =
  try
    Winfo.exists Widget.default_toplevel
  with _ -> false

(* -- Tk widget creating functions -- *)
(** Returns a new frame attached to owner. *)
let frame_create owner =
  Frame.create owner [Relief Groove; BorderWidth (Pixels 2)]

(** Returns a new button attached to owner. *)
and button_create owner name relief =
  Button.create owner [Text name; Relief relief]

(** Returns a new button, in menu owner, named name. *)
and menubutton_create owner name =
  Menubutton.create owner [Text name]

(* -- Conversion functions -- *)
let parse_expression path e = Parserexpression.expression Lexerexpression.lexer (Lexing.from_string e)

let string_of_tag tag = match tag with
  | Tag s -> s
  | _ -> ""

let string_of_path path =
  List.fold_left (function s -> function r -> s^"/"^r) "" path

(* Returns args names contained in argstring plus an error message if there is a syntax error in the argstring *)
and args_names_of_string argstring =
  let args,err = analyze argstring "\\([^,; \n]+\\)" in
    (List.concat args),err

(* Raises failure if there is a syntax error in the argstring *)
and args_values_of_string path argstring =
  let rec fargs la = match la with
    | [e]::ta -> (parse_expression path e)::(fargs ta)
    | _ -> [] in
  let args,err = analyze argstring "\\([^; \n]+\\)" in
  let args,err = try (fargs args),err with _ -> [],"Syntax error: ignoring modifications" in
    args,err

(* Raises failure if there is a syntax error in the argstring *)
let args_names_args_values_of_string path argstring =
  let rec fargs la argnames argvalues = match la with
    | [argname;argvalue]::ta -> fargs ta (argnames^";"^argname) (argvalues^";"^argvalue)
    | _ -> argnames,argvalues in
  let args,err = analyze argstring "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \n]*=[ \n]*\\([^; \n]*\\)" in
  let argnames,argvalues = fargs args "" "" in
  let argnames,errn = args_names_of_string argnames in
  let err = match err with
    | "" -> errn
    | _ -> err in
  let argvalues,errv = args_values_of_string path argvalues in
  let err = match err with
    | "" -> errv
    | _ -> err in
    argnames,argvalues,err

let string_of_durations durations =
  let drs = List.map (function ((lib,name),d) -> (string_of_ref_type lib name)^" = "^(string_of_float d)^"\n") durations in
    List.fold_left (function s -> function t -> s^t) "" (List.sort compare drs)

(* Raises failure if there is a syntax error in durations *)
let durations_analyze durations =
  let rgxp = "\\(\\([a-zA-Z_][a-zA-Z0-9_]*\\)/\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ ]*=[ ]*\\([0-9]+\\(.[0-9]+\\)?\\)" in
  let durations,err = analyze durations rgxp in
  let rec f ld = match ld with
    | [l;lib;name;dr;d]::t -> ((lib,name),(float_of_string dr)) :: (f t)
    | _ -> [] in
    (f durations),err

let get_text_width text =
   list_max_value (Font.measure font) (Str.split (Str.regexp "\n") text) 0

(* -- Info-related functions ----------------------------------------------- *)
let text_clean text =
  match !clean_info_bar with
  | false -> ()
  | true -> Text.delete text (TextIndex ((LineChar (0,0)),[])) (TextIndex (End,[]))

(* A simple shortcut showing a message box, with an ok button and a custom text. *)
let ok_message_box wgt text =
  ignore(Dialog.create wgt syndex_version_msg text (Predefined "") 0 ["Ok"])

let message_info msgbar msg =
  Text.insert msgbar (TextIndex (End,[])) (msg^"\n") [];
  Text.yview msgbar (MoveTo 1.)

let error_message top msg =
  message_info top.tw_msg_bar msg;
  ok_message_box top.tw_frame msg

let evaluation_variable context var msgbar txt =
  try
    Symbolic.result_of_variable context var
  with Failure s -> (message_info msgbar (txt^" : "^s); Symbolic.RFloat 0.)

let evaluation_expression context expr msgbar txt =
  try
    Symbolic.result_of_expression context expr
  with Failure s -> (message_info msgbar (txt^" : "^s); Symbolic.RFloat 0.)

(* -- Generic window opening/closing *)

let generic_window_open top window_identifier open_function =
  match List.filter (function id,_ -> id = window_identifier) top.tw_windows with
  | [] ->
      let win = open_function () in
	top.tw_windows <- (window_identifier,win)::top.tw_windows
  | (_,w)::_ -> raise_window w

let generic_window_close top window_identifier =
  let toclose,others = List.partition (function (id,_) -> id = window_identifier) top.tw_windows in
    top.tw_windows <- others;
    List.iter (function _,w -> Tk.destroy w) toclose

(* -- Dialog and message boxes -- *)
let ask_for_open default help title f widget top wintype =
  let window = Toplevel.create widget [] in
    Wm.title_set window syndex_version_msg;
    let msg = Label.create window [Text title] in
    let hlp = Label.create window [Text help] in
    let field = Entry.create window [Background White] in
      Entry.insert field End default;
      let bottom = Frame.create window [BorderWidth (Pixels 5)] in
      let ok_action _ = (match Entry.get field with
			 | "" -> ()
			 | s -> f s); Tk.destroy window in
      let cancel_action _ = Tk.destroy window in
      let ok = Button.create bottom [Text "ok"; Command ok_action] in
      let cancel = Button.create bottom [Text "cancel"; Command cancel_action] in
	bind window [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],cancel_action));
	bind window [[], Destroy] (BindSet ([],function _ -> generic_window_close top wintype));

	pack [msg;field;hlp] [Expand true; Fill Fill_X]; 
	pack [ok] [Side Side_Left; Expand true];
	pack [cancel] [Side Side_Right; Expand true]; 
	pack [bottom] [Side Side_Bottom; Expand true; Fill Fill_X];
	Focus.set field;
	window

let ask_for default help title f widget top wintype =
  let window = ref Widget.default_toplevel in
    generic_window_open top wintype (function () -> window := ask_for_open default help title f widget top wintype;
				       !window);
    (* Beware the synchronization. *)
    (* Can't wait before ending generic_window_open (otherwise, the window is not added to top.tw_windows yet).*)
    (* However, need to wait before going on, for instance for menu updates to be correctly done.*)
    Tkwait.window !window

(* -- Dialog and message boxes for data faults -- *)
let ask_for_more_open default title_1 title_2 title_3 f check widget top wintype =
  let window = Toplevel.create widget [] in
    Wm.title_set window syndex_version_msg;
    let msg_1 = Label.create window [Text title_1] in
    let msg_2 = Label.create window [Text title_2] in
    let field_1 = Entry.create window [Background White] in 
    let field_2 = Entry.create window [Background White] in
      Entry.insert field_1 End default; 
      Entry.insert field_2 End default;
      let bottom = Frame.create window [BorderWidth (Pixels 5)] in
      let ok_action _ = (match (Entry.get field_1),(Entry.get field_2)  with
			   | "",""  ->  ()
			   | v1,v2  -> f v1 v2); Tk.destroy window in
      let cancel_action _ = Tk.destroy window in
      let check_action _ = check := not !check in
      let field_3 = Checkbutton.create window [Text title_3; Command check_action] in
      let ok = Button.create bottom [Text "ok"; Command ok_action] in
      let cancel = Button.create bottom [Text "cancel"; Command cancel_action] in
	bind window [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],cancel_action));
	bind window [[], Destroy] (BindSet ([],function _ -> generic_window_close top wintype));
	pack [msg_1;field_1;msg_2;field_2] [Expand true; Fill Fill_X]; 
        pack [field_3] [Expand true; Fill Fill_X];
	pack [ok] [Side Side_Left; Expand true];
	pack [cancel] [Side Side_Right; Expand true]; 
	pack [bottom] [Side Side_Bottom; Expand true; Fill Fill_X];
	Focus.set field_1;
        Focus.set field_2;
	window

let ask_for_more default title_1 title_2 title_3 f check widget top wintype =
  let window = ref Widget.default_toplevel in
    generic_window_open top wintype 
      (function () -> 
	 window := ask_for_more_open default title_1 title_2 title_3 f check widget top wintype;
	 !window);   
    Tkwait.window !window


(* -- Dialog and message boxes for data reliability -- *)
let ask_for_more_open_rel default1 default2 default3 default4 title_1 title_2 title_3 title_4 f check widget top wintype =
  let window = Toplevel.create widget [] in
    Wm.title_set window syndex_version_msg;
    let msg_1 = Label.create window [Text title_1] in
    let msg_2 = Label.create window [Text title_2] in
    let msg_3 = Label.create window [Text title_3] in
    let msg_4 = Label.create window [Text title_4] in
    let field_1 = Entry.create window [Background White] in 
    let field_2 = Entry.create window [Background White] in
    let field_3 = Entry.create window [Background White] in 
    let field_4 = Entry.create window [Background White] in
      Entry.insert field_1 End default1; 
      Entry.insert field_2 End default2;
      Entry.insert field_3 End default3; 
      Entry.insert field_4 End default4;
      let bottom = Frame.create window [BorderWidth (Pixels 5)] in 
      let ok_action _ = (match (Entry.get field_1),(Entry.get field_2),
                               (Entry.get field_3),(Entry.get field_4)  with
			   | "","","",""  ->  ()
			   | v1,v2,v3,v4  -> f v1 v2 v3 v4); Tk.destroy window in
      let cancel_action _ = Tk.destroy window in
      let check_action _ = check := not !check in
      let ok = Button.create bottom [Text "ok"; Command ok_action] in
      let cancel = Button.create bottom [Text "cancel"; Command cancel_action] in
	bind window [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],cancel_action));
	bind window [[], Destroy] (BindSet ([],function _ -> generic_window_close top wintype));
	pack [msg_1;field_1;msg_2;field_2;msg_3;field_3;msg_4;field_4] [Expand true; Fill Fill_X]; 
	pack [ok] [Side Side_Left; Expand true];
	pack [cancel] [Side Side_Right; Expand true]; 
	pack [bottom] [Side Side_Bottom; Expand true; Fill Fill_X];
	Focus.set field_1;
        Focus.set field_2;
	Focus.set field_3;
        Focus.set field_4;
	window

let ask_for_more_rel default1 default2 default3 default4 title_1 title_2 title_3 title_4 f check widget top wintype =
  let window = ref Widget.default_toplevel in
    generic_window_open top wintype 
      (function () -> 
      window := 
     ask_for_more_open_rel default1 default2 default3 default4 title_1 title_2 title_3 title_4 f check widget top wintype;
	 !window);   
    Tkwait.window !window


      
let edit win default help text =
  let window = Toplevel.create win [] in
    Wm.title_set window syndex_version_msg;
    let msg=Label.create window [Text text] in
    let hlp=Label.create window [Text help] in
    let txt=Text.create window [Background White] in
    let ysb = Scrollbar.create window [Orient Vertical] in
      Text.configure txt [YScrollCommand (Scrollbar.set ysb)];
      Scrollbar.configure ysb [ScrollCommand (Text.yview txt)];
      let bottom=Frame.create window [BorderWidth (Pixels 5)] in
      let answer=ref None in
      let ok_action _ = answer := Some(Text.get txt (TextIndex(LineChar(0,0),[])) (TextIndex(End,[]))); Tk.destroy window in
      let cancel_action _ = answer := None; Tk.destroy window in
      let ok = Button.create bottom [Text "ok"; Command ok_action] in
      let cancel = Button.create bottom [Text "cancel"; Command cancel_action] in
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],cancel_action));
	pack [msg] [Expand true; Fill Fill_X];
	pack [ysb] [Side Side_Right; Expand true; Fill Fill_Y];
	pack [txt] [Expand true; Fill Fill_Both];
	pack [hlp] [Expand true; Fill Fill_X];
	pack [ok] [Side Side_Left; Expand true];
	pack [cancel] [Side Side_Right; Expand true];
	pack [bottom] [Side Side_Bottom; Expand true; Fill Fill_X];
	Text.insert txt (TextIndex (End,[])) default [];
	Tkwait.window window;
	!answer

let dialog_create title msg default buttons =
  let window = match is_initialised_tk () with
    | true -> Toplevel.create Widget.default_toplevel []
    | false -> openTk () in
    Wm.title_set window title;
    let msg = Label.create window [Text msg; Justify Justify_Left] in
      pack [msg] [];
      let answer = ref default in
      let dialog_end i = Tk.destroy window; answer := i in
	ignore (List.fold_left (function i -> function b ->
				  let button = Button.create window [Text b; Command (function () -> dialog_end i)] in
				    pack [button] [Side Side_Left];
				    bind button [[],KeyPressDetail "Return"] (BindSet ([],(function _ -> dialog_end i)));
				    (match i=default with
				     | true ->
					 Button.configure button [Relief Raised];
					 Focus.set button
				     | false -> ());
				    i+1) 0 buttons);
	Tkwait.window window;
	!answer

let options_box win windowtitle title options okaction =
  let window = Toplevel.create win [] in
    Wm.title_set window windowtitle;
    let msg = Label.create window [Text title] in
      pack [msg] [];
      let updates = List.map (function button_type,options_list,option_value,update ->
				let tv,fld = match button_type with
				  | Checkbutton ->
				      let tv = Textvariable.create () in
				      let frm = frame_create window in
					List.iter (function lbl ->
						     let b = Radiobutton.create frm [Text lbl; Value lbl; Variable tv; Anchor W] in
						       pack [b] [Fill Fill_X]) options_list;
					pack [frm] [Fill Fill_X];
					Textvariable.set tv option_value;
					Some tv,None
				  | Radiobutton ->
				      let tv = Textvariable.create () in
				      let frm = frame_create window in
				      let b = Checkbutton.create frm [Text (List.hd options_list); Variable tv; Anchor W] in
					pack [b] [Fill Fill_X];
					pack [frm] [Fill Fill_X];
					let vl = match option_value with
					  | "true" -> "1"
					  | _ -> "0" in
					  Textvariable.set tv vl;
					  Some tv,None
				  | Field focus ->
				      let frm = frame_create window in
				      let msg = Label.create frm [Text (List.hd options_list)] in
				      let fld = Entry.create frm [Background White] in
					Entry.insert fld End option_value;
					(match focus with
					 | true -> Focus.set fld
					 | false -> ());
					pack [msg] [Side Side_Left];
					pack [fld] [Side Side_Right;Expand true;Fill Fill_X];
					pack [frm] [Fill Fill_X];
					None,Some fld in
				  button_type,tv,fld,update) options in
      let ok_action _ =
	List.iter (function button_type,tv,fld,update ->
		     let vl = match button_type with
		       | Checkbutton | Radiobutton -> 
			   let tv = match tv with
			     | Some tv -> tv
			     | None -> raise (Failure "options_box") in
			   let vl = Textvariable.get tv in
			     (match button_type with
			      | Checkbutton -> vl
			      | Radiobutton -> (match vl with
						| "1" -> "true"
						| _ -> "false")
			      | _ -> "")
		       |	Field _ ->
				  let fld = match fld with
				    | Some fld -> fld
				    | None -> raise (Failure "options_box") in
				    Entry.get fld in
		       update vl) updates;
	Tk.destroy window;
	okaction () in
      let cancel_action _ = Tk.destroy window in
      let ok = Button.create window [Text "ok"; Command ok_action] in
      let cancel = Button.create window [Text "cancel"; Command cancel_action] in
	bind window [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],cancel_action));
	pack [ok] [Side Side_Left; Expand true];
	pack [cancel] [Side Side_Right; Expand true];
	Tkwait.window window

let autopos_step_box win =
  let step_size_x = ref 0 in
  let step_size_y = ref 0 in
  let conv size =
    try
      int_of_string size
    with Failure _ -> default_autopos_step  in
  let update_step_size_x size =
    step_size_x := conv size in
  let update_step_size_y size =
    step_size_y := conv size in
    
  let options = [Field true,["x"],string_of_int default_autopos_step,update_step_size_x;
		 Field false,["y"],string_of_int default_autopos_step,update_step_size_y] in
    options_box win "Space between vertices" "Enter the space size between vertices" options (function () -> ());
    (!step_size_x,!step_size_y)

let file_view win filename =
  let window = Toplevel.create win [] in
    Wm.title_set window ("File "^(Filename.basename filename));
    let txt = Text.create window [Background background_color] in
    let ysb = Scrollbar.create window [Orient Vertical] in
      Text.configure txt [YScrollCommand (Scrollbar.set ysb)];
      Scrollbar.configure ysb [ScrollCommand (Text.yview txt)];
      let ok_action _ = Tk.destroy window in
      let ok = Button.create window [Text "ok"; Command ok_action] in
	bind window [[],KeyPressDetail "Escape"] (BindSet ([],ok_action));
	pack [ysb] [Side Side_Right; Fill Fill_Y];
	pack [txt] [Expand true; Fill Fill_Both];
	pack [ok] [Side Side_Left; Expand true];
	let f = open_in filename in
	  (try
	     while true do
	       Text.insert txt (TextIndex (End,[])) ((input_line f)^"\n") [];
	     done
	   with _ -> ());
	  window

let html_view win filename = exec !html_program [filename]


(* -- Menu creation functions -- *)

let ask_for_libsdef_open title f libdefs widget top win_type =
  let window = Toplevel.create widget [] in
    Wm.title_set window syndex_version_msg;
    let msg=Label.create window [Text title] in
    let frm=Frame.create window [] in
    let lb = Listbox.create frm [] in
    let lb_scr = Scrollbar.create frm [Orient Vertical] in
      Listbox.configure lb [YScrollCommand (Scrollbar.set lb_scr)];
      Scrollbar.configure lb_scr [ScrollCommand (Listbox.yview lb)];
      let btn_frm = Frame.create frm [] in
      let currlib = ref (fst (List.hd libdefs)) in
	
      let mklib_btn (lib,defs) =
	let libname = match lib with
	  | "" -> "local"
	  | _ -> lib in
	let btn = Button.create btn_frm [Text libname] in
	  pack [btn] [Side Side_Top; Fill Fill_Both];
	  (lib,btn) in
      let btn_list = List.map mklib_btn libdefs in
      let update lib =
	Button.configure (List.assoc !currlib btn_list) [Relief Raised];
	currlib := lib;
	Button.configure (List.assoc !currlib btn_list) [Relief Ridge];
	Listbox.delete lb (Number 0) End;
	Listbox.insert lb End (List.assoc lib libdefs) in
	List.iter (function lib,btn -> Button.configure btn [Command (function _ -> update lib)]) btn_list;
	update (fst (List.hd libdefs));
	let quit_action _ = Tk.destroy window in
	let ok_action _ = match Listbox.curselection lb with
	  | d::_ -> f (!currlib,Listbox.get lb d)
	  | _ -> () in
	  
	let bottom = Frame.create window [BorderWidth (Pixels 5)] in
	let quit = Button.create bottom [Text "quit"; Command quit_action] in
	  
	  bind lb [[Double],ButtonPressDetail 1] (BindSet ([], ok_action));
	  bind window [[],KeyPressDetail "Escape"] (BindSet ([], quit_action));
	  bind window [[], Destroy] (BindSet ([],function _ -> generic_window_close top win_type));
	  
	  pack [quit] [Side Side_Right; Expand true]; 
	  pack [btn_frm] [Fill Fill_Both; Side Side_Left];
	  pack [lb] [Expand true; Fill Fill_Both; Side Side_Left];
	  pack [lb_scr] [Fill Fill_Y; Side Side_Left];
	  pack [msg] [Fill Fill_X]; 
	  pack [frm] [Expand true; Fill Fill_Both]; 
	  pack [bottom] [Fill Fill_X];
	  window
	    
let ask_for_libsdef title f libdefs widget top win_type =
  match libdefs with
  | [] -> ()
  | _ -> generic_window_open top win_type (function () -> ask_for_libsdef_open title f libdefs widget top win_type)
      
let clear_menu menu = Menu.delete menu (Number 0) Last

let libs_defs_list l =
  let rec f l =
    match l with
    | [] -> []
    | (lt,_)::q -> let l1,l2 = List.partition (function (l,_) -> l=lt) l in
      let defs = List.sort compare (List.map (function (_,n) -> n) l1) in
	(lt,defs) :: (f l2) in
    List.sort (function (l1,_) -> function (l2,_) -> compare l1 l2) (f l)

let choose_libs_defs f l title widget top win_type = ask_for_libsdef title f (libs_defs_list l) widget top win_type
						       
(* Creates cascade limited-length menus from a list of entries. If there are more than 'menu_size_max' items in the list, 
   a cascade menu, labelled 'more...' is created to hold the remaining items of the list, and so on, in a recursive way... *)
let rec_cascade menu entries =
  let rec add_entries m l size =
    match l with
    | [] -> ()
    | hd::tl ->
	match size = menu_size_max with
	|	true ->
		  let submenu = Menu.create m [] in
		    Menu.add_separator m;
		    Menu.add_cascade m [Label "more..."; Menu submenu];
		    add_entries submenu l 0
	|	false ->
		  Menu.add_command m hd;
		  add_entries m tl (size+1)
  in add_entries menu entries 0

let create_menu_libsdefs menu libsdefs f =
  (* cascade_menu creates from a list limited-lenght menu. If there are more than 'menu_size_max' items in the list, 
     a cascade menu, labelled 'more...' is created to hold the remaining items of the list, and so on, in a recursive way... *)
  let rec cascade_menu (lib,defs,smenu,number) =
    match defs with
    | [] -> ()
    | def::tl ->
	Menu.add_command smenu [Label def; Command (function () -> f lib def)];
	let smenu,number = match number with
	  | 0 ->
	      let mnu = Menu.create smenu [] in
		Menu.add_separator smenu;
		Menu.add_cascade smenu [Label "more..."; Menu mnu];
		mnu,menu_size_max
	  | _ -> smenu,number in
	  cascade_menu(lib,tl,smenu,number-1) in
  let submenu = Menu.create menu [] in
    List.iter (function (lib,defs) ->
		 let submenulib = Menu.create submenu [] in
		   cascade_menu (lib,defs,submenulib,menu_size_max);
		   let lib = match lib with
		     | "" -> "local"
		     | _ -> lib in
		     Menu.add_cascade submenu [Label lib; Menu submenulib]) libsdefs;
    submenu

let code_phases_edit win top lib name possible_phases phases_content_f phases_modify_f =
  let info = "Choose code generation phases\nin which to generate code\n"^
	     "(in addition to the loop sequence):\n" in
  let code_phases = phases_content_f lib name in
  let update_code_phase code_phase add_remove =
    top.tw_file_changed ();
    (* the value above is not up to date *)
    let  code_phases = phases_content_f lib name in
      match add_remove with
      | "true" -> let new_phases = union [code_phases;[code_phase]] in
	  phases_modify_f lib name new_phases
      | "false" -> let new_phases = List.filter (fun cphase -> cphase <> code_phase) code_phases in
	  phases_modify_f lib name new_phases
      | _ -> failwith "Algorith_ctk.algocode_phases_edit: Code phases error" in
  let options = List.map (fun phase -> 
			    let phase_s = string_of_code_phase phase in
			    let phase_value_s = string_of_bool (List.mem phase code_phases) in
			      (Radiobutton,[phase_s],phase_value_s,(update_code_phase phase))) possible_phases in
    options_box win "Code Generation phases" info options (fun () -> ()) 
