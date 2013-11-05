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
open Adequationtypes

type vertice_type = Operator | Operation
and edges_type = Dependence

let reliability_cost = ref ""

let operator_color = White
let operation_color = NamedColor "snow3"
let predecessor_color = NamedColor "Green4"
let current_operation_color = NamedColor "Orange3"
let successor_color = NamedColor "IndianRed3"

let operation_canvas_size_x = 600
let operation_canvas_size_y = 300
let operator_canvas_size_x = operation_canvas_size_x
let operator_canvas_size_y = 25
let operation_width = 60
let horizontal_space = 10
let vertical_space = 10
let border = 3
let scale = ref 1.0

let schedule_direction_of_string dir = match dir with
| "Vertical Display" -> VerticalSchedule
| "Horizontal Display" -> HorizontalSchedule
| _ -> raise (Failure "Schedule_ctk.schedule_direction_of_string")

let string_of_schedule_direction dir = match dir with
| HorizontalSchedule -> "Horizontal Display"
| VerticalSchedule -> "Vertical Display"

let pred_succ operation pred_succ_list =
  match List.mem_assoc operation pred_succ_list with
  | true -> List.assoc operation pred_succ_list
  | false -> []

let time_max schedules =
  let opns = List.concat (List.map (function _,(c,o) -> c@o) schedules) in
  let max1 (_,_,_,eefs,_,_,_) = eefs in
  list_max_value max1 opns 0.

let fault_time_max schedules =
  let opns = List.concat (List.map (function _,(c,o) -> c@o) schedules) in
  let max1 (_,_,esfs,eefs,wesfs,_,_) = wesfs+.(eefs-.esfs) in
  list_max_value max1 opns 0.

let schedule_info_change win schedules preds succs infotype =
  let infobar,msgbar = win.sch_top.tw_info_bar, win.sch_top.tw_msg_bar in
  let condition condlist = match condlist with
    | ("",_)::_ -> ""
    | _ ->
	let condlist = List.sort (function vl1,_ -> function vl2,_ -> compare vl1 vl2) condlist in
	  List.fold_left (function s -> function (vbl,vle) -> s^vbl^"="^(string_of_int vle)^"\n") "" condlist in
  let reference opn =
    let time = "Cycle time : "^(string_of_float (time_max schedules))^"\n"^"Reliability : "^(!reliability_cost)^"\n" in
    let opns = List.concat (List.map (function _,(c,o) -> c@o) schedules) in
    let _,_,esfs,eefs,wesfs,cond,_ = List.find (function id,_,_,_,_,_,_ -> id=opn) opns in
    let predecessors = pred_succ opn preds
    and successors = pred_succ opn succs in
    let pred_string = List.fold_left (function s -> function opn -> s^opn^"\n") "Predecessors :\n" predecessors
    and succ_string = List.fold_left (function s -> function opn -> s^opn^"\n") "Successors :\n" successors in
      time^"Operation "^opn^" : "^(string_of_float esfs)^" -> "^(string_of_float eefs)^"\n"^  
      (condition cond)^"\n"^pred_string^"\n"^succ_string in  
  let operator opr = 
    let sch_time_max = time_max schedules in
    let time = "Cycle time : "^(string_of_float sch_time_max)^"\n" in
    let cnts,opns = List.assoc opr schedules in 
    let opns_sorted = List.sort (function _,_,_,_,_,_,r1 -> function _,_,_,_,_,_,r2 -> compare r1 r2) opns in
    let opr_opns,total_computation_time = List.fold_left
					    (fun (s,c_time) (identifier,_,esfs,eefs,_,_,_) ->
					       let opn_string = identifier^" : "^(string_of_float esfs)^
								" -> "^(string_of_float eefs)^"\n" in
						 (s^opn_string,(c_time+.eefs-.esfs))) ("",0.) opns_sorted in
    let occupation_percent = int_of_float (total_computation_time /. sch_time_max *. 100.) in
      time^"\nOperator "^opr^" :\n"^opr_opns
  and canvas () = "" in
    text_clean infobar;
    let info = match infotype with
      | Task opn -> reference opn
      | Ecu ecu -> operator ecu
      | Canvas -> canvas ()
      | Clean -> ""
      | _ -> "" in
      Text.insert infobar (TextIndex (End,[])) info []

let operator_draw win graph operator_cvs finalpos width (name,tag,(x,y)) =
  let cvs = Graph_ctk.canvas graph in
  let h = Font.metrics font Linespace in
  let xtxt,ytxt,anchor = match win.sch_direction with
  | VerticalSchedule -> x+(width/2),2*border,N
  | HorizontalSchedule -> 2*border,y+(width/2),W in
  let idtxt = Canvas.create_text operator_cvs (Pixels xtxt) (Pixels ytxt)
		[Text name; Anchor anchor;Tags [Tag ("OperatorLabel"^tag);Tag tag;Tag "OperatorLabel"]] in
  let xtxt1,ytxt1,xtxt2,ytxt2 = Canvas.bbox operator_cvs [idtxt] in
  let xtxt1,ytxt1,xtxt2,ytxt2 = match win.sch_direction with
    | VerticalSchedule -> x,ytxt1-border,x+width,ytxt2+border
    | HorizontalSchedule -> xtxt1-border,ytxt1-border,xtxt2+border,ytxt2+border in
  let idrct = Canvas.create_rectangle operator_cvs (Pixels xtxt1) (Pixels ytxt1) (Pixels xtxt2)
		(Pixels ytxt2) [FillColor operator_color; Tags [Tag tag;Tag (tag^"RectangleBackground")]] in
    Canvas.lower_below operator_cvs idrct idtxt;
    let x1,y1,x2,y2 = match win.sch_direction with
      | VerticalSchedule -> x,0,x+width,finalpos
      | HorizontalSchedule -> 0,y,finalpos,y+width in
    let id = Canvas.create_rectangle cvs (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2) [Tags [Tag tag]] in
      Graph_ctk.add_specialtag graph id Graph_ctk.Zoomable
	
let operator_enter win schedules operator = schedule_info_change win schedules [] [] (Ecu (fst operator))

let operator_leave win schedules operator = ()(*schedule_info_change win schedules [] [] Canvas*)
					      
let operator_info _ = ""

let vertices_labels_refresh cvs type_label =
  let id_refresh id =
    let rgxp = Str.regexp type_label in
    let tags = Canvas.gettags cvs id in
    let tags = List.map (function tag ->
      let tag_string = string_of_tag tag in
      match Str.string_match rgxp tag_string 0 with
      |	true ->
	  let idx = Str.match_end () in
	  String.sub tag_string idx ((String.length tag_string)-idx)
      |	false -> "") tags in
    let tag = List.find ((<>) "") tags in
    let vertex_tag = Tag tag
    and vertex_label_tag = Tag (type_label^tag) in
    Canvas.configure_text cvs vertex_label_tag [State Normal];
    let x_opn_1,y_opn_1,x_opn_2,y_opn_2 = Canvas.bbox cvs [vertex_tag] in
    let x_lbl_1,y_lbl_1,x_lbl_2,y_lbl_2 = Canvas.bbox cvs [vertex_label_tag] in
    match (x_opn_2-x_opn_1>x_lbl_2-x_lbl_1) && (y_opn_2-y_opn_1>y_lbl_2-y_lbl_1) with
    | true -> Canvas.configure_text cvs vertex_label_tag [State Normal]
    | false -> Canvas.configure_text cvs vertex_label_tag [State Hidden] in
  let ids = Canvas.find cvs [Withtag (Tag type_label)] in
  List.iter id_refresh ids
			
let all_labels_refresh operation_cvs operator_cvs=
  vertices_labels_refresh operation_cvs "OperationLabel";
  vertices_labels_refresh operator_cvs "OperatorLabel"

let operation_draw win graph spacetitle operationlabel finalpos (name,tag,(x,y)) =
  let cvs = Graph_ctk.canvas graph in
  let x1,y1,x2,y2 = match win.sch_direction with
  | VerticalSchedule -> x,y,x+spacetitle,finalpos
  | HorizontalSchedule -> x,y,finalpos,y+spacetitle in
  ignore(Canvas.create_rectangle cvs (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2)
	   [FillColor operation_color; Tags [Tag tag;Tag (tag^"RectangleBackground")]]);
  match win.sch_labels with
  | true ->
      let operation_label_tag = Tag ("OperationLabel"^tag) in
      ignore(Canvas.create_text cvs (Pixels ((x1+x2)/2)) (Pixels ((y1+y2)/2))
	       [Text operationlabel; Tags [operation_label_tag;Tag tag;Tag "OperationLabel"]])
  | false -> ()

let operation_color_update graph preds succs operation predecessor_color current_operation_color successor_color =
  let cvs = Graph_ctk.canvas graph in
  let tag = Graph_ctk.tag_of_vertice graph operation in
  let predecessors = pred_succ (fst operation) preds
  and successors = pred_succ (fst operation) succs in
  let color_put color id =
    try
      let tag = Graph_ctk.tag_of_vertice graph (id,Operation) in
	ignore(Canvas.configure_rectangle cvs (Tag (tag^"RectangleBackground")) [FillColor color]);
	Canvas.addtag cvs (Tag "Colored") [Withtag (Tag (tag^"RectangleBackground"))]
  with _ -> () in (* Delete error message. For predecessors not found when show_constants=false *)
  List.iter (color_put predecessor_color) predecessors;
  List.iter (color_put successor_color) successors;
  color_put current_operation_color (fst operation)

let operation_enter win graph schedules preds succs operation =
  schedule_info_change win schedules preds succs (Task (fst operation));
  match win.sch_fixed_colors with
  | true -> ()
  | false -> operation_color_update graph preds succs operation predecessor_color current_operation_color successor_color

let operation_color_remove win graph =
  match win.sch_fixed_colors with
  | true -> ()
  | false ->
      let cvs = Graph_ctk.canvas graph in
      let tags = Canvas.find cvs [Withtag (Tag "Colored")] in
      List.iter (function tag ->
	ignore(Canvas.configure_rectangle cvs tag [FillColor operation_color]);
	Canvas.dtag cvs tag (Tag "Colored")) tags

let operation_leave win graph _ _ _ _ =
(*  schedule_info_change win schedules preds succs Canvas;
  operation_color_update graph preds succs operation operation_color operation_color operation_color*)
  operation_color_remove win graph

let operations_select graph operations_list =
  List.iter (function n,_,_,_,_,_,_ -> 
	       try
		 Graph_ctk.select_vertex graph (n,Operation)
	       with Not_found -> () (* Again, constants vertices may not be present*)
	    ) operations_list

let operator_select graph operations_list _ = operations_select graph operations_list

let operation_find (win,g) (schedules,preds,succs) =
  let find_f opnname =
    Graph_ctk.select_vertex g (opnname,VOperation);
    operation_enter win g schedules preds succs (opnname,VOperation) in
  let opnnames (oprname,oprschedule) =
    List.map (fun (opnname,_,_,_,_,_,_) -> (oprname,opnname)) oprschedule in
  let opns = List.concat (List.map (fun (oprname,(_,schedule)) -> opnnames (oprname,schedule)) schedules) in
  let title = "Select operation to find:" in
    choose_libs_defs (function (_,opnname) -> find_f opnname) opns title win.sch_widget win.sch_top WSelect_sched_opn

let operation_info schedules preds succs opn _ = 	      
  let condition condlist = match condlist with
    | ("",_)::_ -> ""
    | _ ->
	let condlist = List.sort (function vl1,_ -> function vl2,_ -> compare vl1 vl2) condlist in
	  List.fold_left (function s -> function (vbl,vle) -> s^vbl^"="^(string_of_int vle)^"\n") "" condlist in
  let opns = List.concat (List.map (function _,(c,o) -> c@o) schedules) in
  let _,_,esfs,eefs,wesfs,cond,_ = List.find (function id,_,_,_,_,_,_ -> id=opn) opns in
  let predecessors = pred_succ opn preds
  and successors = pred_succ opn succs in
  let pred_string = List.fold_left (function s -> function opn -> s^opn^"\n") "Predecessors :\n" predecessors
  and succ_string = List.fold_left (function s -> function opn -> s^opn^"\n") "Successors :\n" successors in
    "Operation "^opn^" : "^(string_of_float esfs)^" -> "^(string_of_float eefs)^"\n"^(condition cond)^pred_string^succ_string
      

let arrows_draw graph tags =
  let cvs = Graph_ctk.canvas graph in
  List.iter (function tag0,tag1 ->
    let _,_,x0,y0 = Canvas.bbox cvs [Tag tag0]
    and x1,y1,_,_ = Canvas.bbox cvs [Tag tag1] in
    ignore(Canvas.create_line cvs [Pixels (x0-1);Pixels (y0-1);Pixels (x1+1);Pixels (y1+1)]
	     [ArrowStyle Arrow_Last;Tags [Tag "Arrow";Tag (tag0^"Arrow");Tag (tag1^"Arrow")]])) tags   

let arrows_succs_operation_draw graph successors operation =
  let tag0 = Graph_ctk.tag_of_vertice graph (operation,Operation) in
  let tags_succs = List.map (function id -> tag0,(Graph_ctk.tag_of_vertice graph (id,Operation))) successors in
  arrows_draw graph tags_succs

let arrows_preds_operation_draw graph predecessors operation =
  let tag0 = Graph_ctk.tag_of_vertice graph (operation,Operation) in
  let tags_preds = List.map (function id -> (Graph_ctk.tag_of_vertice graph (id,Operation)),tag0) predecessors in
  arrows_draw graph tags_preds

let arrows_operation_draw graph preds succs operation =
  let predecessors = pred_succ operation preds
  and successors = pred_succ operation succs in
  arrows_preds_operation_draw graph predecessors operation;
  arrows_succs_operation_draw graph successors operation

let arrows_operation_remove graph opn =
  let cvs = Graph_ctk.canvas graph in
  let tag = Graph_ctk.tag_of_vertice graph (opn,Operation) in
  Canvas.delete cvs [Tag (tag^"Arrow")]

let arrows_graph_remove graph opn =
  let cvs = Graph_ctk.canvas graph in
  Canvas.delete cvs [Tag "Arrow"]

let arrows_operation_redraw graph preds succs opn =
  arrows_operation_remove graph opn;
  arrows_operation_draw graph preds succs opn

let arrows_graph_draw graph successors =
  List.iter (function opn,succs -> arrows_succs_operation_draw graph succs opn) successors

let update_arrows win graph succs = match win.sch_arrows with
| true -> arrows_graph_draw graph succs
| false -> arrows_graph_remove graph ()

let labels_remove graph =
  let cvs = Graph_ctk.canvas graph in
  Canvas.delete cvs [Tag "Label"]

let options win refresh_graph refresh_arrows labels_remove =
  let update_direction dir =
    let dir = schedule_direction_of_string dir in
    match win.sch_direction <> dir with
    | true ->
	win.sch_direction <- dir;
	refresh_graph ()
    | false -> () in
  let update_arrows arrows =
    let arrows = bool_of_string arrows in
    match win.sch_arrows <> arrows with
    | true -> win.sch_arrows <- arrows; refresh_arrows ()
    | false -> () in
  let update_labels labels =
    let labels = bool_of_string labels in
    match win.sch_labels <> labels with
    | true ->
	win.sch_labels <- labels;
	(match labels with
	| true -> refresh_graph ()
	| false -> labels_remove ())
    | false -> () in
  let update_scale scale =
    let scale = bool_of_string scale in
      match win.sch_scale <> scale with
      | true -> win.sch_scale <- scale; refresh_graph ()
      | false -> () in
  let update_show_constants show_constants =
    let show_constants = bool_of_string show_constants in
      match win.sch_show_constants <> show_constants with
      | true -> win.sch_show_constants <- show_constants; refresh_graph ()
      | false -> () in
  let options =
    [Checkbutton,["Horizontal Display";"Vertical Display"],string_of_schedule_direction win.sch_direction,update_direction;
     Radiobutton,["Arrows"],string_of_bool win.sch_arrows,update_arrows;
     Radiobutton,["Labels"],string_of_bool win.sch_labels,update_labels;
     Radiobutton,["Scale"],string_of_bool win.sch_scale,update_scale;
     Radiobutton,["Show Constants"],string_of_bool win.sch_show_constants,update_show_constants] in
  options_box win.sch_widget syndex_version_msg "Options" options (function () -> ())

let schedules_equality sch1 sch2 =
  sch1==sch2

let schedule_view_close sch =
  sch.sch_top.tw_schedules <- List.filter (function s,_ -> (s!=sch)) sch.sch_top.tw_schedules;
  Tk.destroy sch.sch_widget

let update_menus (win,graph) schedule_info direction refresh_graph refresh_arrows labels_remove =
  win.sch_direction <- direction;
  let window_menu = List.assoc "Window" win.sch_menus in
  let edit_menu = List.assoc "Edit" win.sch_menus in
  let cvs_menu = Graph_ctk.get_menu_canvas graph in
  clear_menu window_menu;
  clear_menu edit_menu;
  Menu.add_command window_menu [Label "Schedule Display Options";
				Command (function () -> options win refresh_graph refresh_arrows labels_remove)];
  Menu.add_separator window_menu;
  Menu.add_command window_menu [Label "Close";Command (function () -> schedule_view_close win)];
  Menu.add_command edit_menu [Label "Postscript File"; Command (function () -> Graph_ctk.postscript graph)];
  Menu.add_command edit_menu [Label "JPeg File"; Command (function () -> Graph_ctk.jpeg graph)];
  Menu.add_separator edit_menu;
  Menu.add_command edit_menu [Label "Find Operation"; Command (function () -> operation_find (win,graph) schedule_info)]
    
let rec refresh win graph operation_frm schedule_info operator_cvs operator_frm =
  let operation_cvs = Graph_ctk.canvas graph in
    (* Repacking so that operator_frm and operator_cvs go on the right
       place depending on schedule direction *)
  let side,fill,operator_canvas_size_x,operator_canvas_size_y = match win.sch_direction with
    | VerticalSchedule -> Side_Top,Fill_X,operation_canvas_size_x,operator_canvas_size_y
    | HorizontalSchedule ->   let schedules,_,_ = schedule_info in
      let operatorname_max = (list_max_value (function (n,(_,_)) -> get_text_width n) schedules 0)+4*border in
	Side_Left,Fill_Y,operatorname_max,operation_canvas_size_y in
    pack [operator_frm] [Fill fill;Side side];
    pack [operator_cvs] [Fill Fill_Both;Expand true];
    Canvas.configure operator_cvs [Width (Pixels operator_canvas_size_x);Height (Pixels operator_canvas_size_y)];
    pack [operation_frm] [Fill Fill_Both;Expand true;Side side];
    Graph_ctk.canvas_size graph (operation_canvas_size_x,operation_canvas_size_y);
    Canvas.delete operator_cvs (Canvas.find operator_cvs [All]);
    Graph_ctk.reset graph;
    Graph_ctk.canvas_scale graph 1.;
    scale := 1.;
    let _,_,succs = schedule_info in
      update_menus (win,graph) schedule_info win.sch_direction 
	(function () -> refresh win graph operation_frm schedule_info operator_cvs operator_frm)
	(function () -> update_arrows win graph succs) (function () -> labels_remove graph) ;
      let dirmove = match win.sch_direction with
	| VerticalSchedule -> "Horizontal"
	| HorizontalSchedule -> "Vertical" in
	Graph_ctk.option_set graph Graph_ctk.MoveDirection dirmove;
	schedule_display win graph operation_frm schedule_info operator_cvs operator_frm;
	all_labels_refresh operation_cvs operator_cvs;
	(match win.sch_direction with
	| VerticalSchedule -> Graph_ctk.origin_pan operation_cvs;
	    Graph_ctk.origin_pan operator_cvs
	| HorizontalSchedule -> Graph_ctk.origin_pan_x operation_cvs;
	    Graph_ctk.origin_pan_x operator_cvs);
	Graph_ctk.set_scrollregion operator_cvs
	  
(** Displays the constants operations of operator positionned at operatorposition. *)
and schedule_operator_display_constants constants width win operatorposition h
  constantname_max graph schedules preds succs vertice_delete =
  let rec f number csts = 
    match csts with 
    | [] -> ()
    | (identifier,operationname,_,_,_,_,_)::tl -> 
	let opnpos,finalpos = match win.sch_direction with
	  | VerticalSchedule -> (operatorposition,-(h+2*border)*(number+1)-2*border), -2*border-(h+2*border)*number
	  | HorizontalSchedule -> (-constantname_max,operatorposition+(h + 2*border)*number),-2*border in
	let operation_draw = operation_draw win graph width operationname finalpos in
	let operation_move _ _ = () in
	let operation_enter_f = operation_enter win graph schedules preds succs in
	let operation_leave = operation_leave win graph schedules preds succs in
	let operation_select _ = arrows_operation_remove graph identifier in
	let operation_info _ = operation_info schedules preds succs identifier () in
	let refresh_ports_edges_f _ = ([],[]) in
	  Graph_ctk.vertex_create graph identifier Operation opnpos false [Graph_ctk.Movable]
	    [] operation_draw operation_enter_f operation_leave operation_select operation_move
	    vertice_delete operation_info refresh_ports_edges_f;
	  f (number+1) tl in
    f 0 constants

(** Displays operations (non-constant) of operator positionned at operatorposition *)
and schedule_operator_display_operations opnlist spacetitle opns_place
    operatorposition win factor constants_number h graph succs preds
    schedules vertice_delete =
  let rec aux opnlist idx_max =
    match opnlist with
    | [] -> idx_max*spacetitle
    | (identifier,operationname,esfs,eefs,wesfs,_,rank)::tl ->
	let idx = List.assoc identifier opns_place in
	let pos = operatorposition+(idx-1)*spacetitle in
	let beginpos,finalpos = match win.sch_scale with
	| true -> int_of_float (esfs*.factor),int_of_float (eefs*.factor)
	      (* This might seem completely arbitrary for the
		 horizontal schedule but how else ? *)
	| false -> (rank-constants_number)*(h+2*border),(rank-constants_number+1)*(h+2*border) in
	let opnpos,finalpos = 
	  match win.sch_direction with
	  | VerticalSchedule -> (pos,beginpos),finalpos
	  | HorizontalSchedule -> (beginpos,pos),finalpos in
	
	let operation_draw = operation_draw win graph spacetitle operationname finalpos in
	let operation_move _ _ = Graph_ctk.unselect_all_graph graph;update_arrows win graph succs in
	let operation_enter_f = operation_enter win graph schedules preds succs in
	let operation_leave = operation_leave win graph schedules preds succs in
	let operation_select _ = arrows_operation_remove graph identifier in
	let operation_info _ = operation_info schedules preds succs identifier () in
	let refresh_ports_edges_f _ = ([],[]) in
	Graph_ctk.vertex_create graph identifier Operation opnpos false [Graph_ctk.Zoomable] []
	  operation_draw operation_enter_f operation_leave operation_select operation_move
	  vertice_delete operation_info refresh_ports_edges_f;
	(* 	    ps (identifier^" : pos : "^(string_of_int (fst opnpos))^" begin "^
	   (string_of_int (snd opnpos))^" final "^(string_of_int finalpos)^" rank "^
	   (string_of_int rank)^" factor "^(string_of_float factor)); *)
	aux tl (max idx idx_max) in
  aux opnlist 0

(** Returns the maximum size required to display the constants of schedules *)
and constants_required_size win h schedules =
  match win.sch_show_constants with
  | false -> 2*border
  | true -> match win.sch_direction with
    | VerticalSchedule -> (h + 2*border)*(list_max_value (function (_,(c,_)) -> List.length c) schedules 0)+2*border
    | HorizontalSchedule ->
	let f (_,(c,_)) = list_max_value (function (_,n,_,_,_,_,_) -> get_text_width n) c 0 in
	(list_max_value f schedules 0)+4*border

(** Returns the size reserved for the schedule of each operator *)
and size_per_schedule opns_place win cvs h =
    match win.sch_direction with
    | VerticalSchedule ->
	let n = List.fold_left (function n -> function operator,opns ->
					let idx_max = list_max_value snd opns 0 in
					  match idx_max with
					  | 0 -> n+1
					  | _ -> (n+idx_max)) 0 opns_place in
	let n_operator = List.length opns_place in
	  ((Winfo.width cvs)-(n_operator*border))/n
    | HorizontalSchedule -> h + 2*border

(** Affects an index to each operation of opns. The index is used to
  display different columns for different conditions.*)
and split_by_cond schedules =
  let rec pos smltns idx = match smltns with
  | (_,_,_,idxhd)::tl when idxhd=idx -> pos tl (idx+1)
  | _ -> idx in
  List.map
    (fun (operator,(_,operations_list)) ->
      let opns = List.fold_left
	  (fun opns_placed (operation,_,esfs,eefs,_,_,_) ->
	    let smltns = List.filter
		(fun (_,e,s,_) -> simultaneous esfs eefs e s)
		opns_placed in
	      let idx = pos (List.sort (fun (_,_,_,i1) (_,_,_,i2) -> compare i1 i2) smltns) 1 in
	      (operation,esfs,eefs,idx)::opns_placed)
	  [] operations_list in
      operator,(List.map (fun (opn,_,_,idx) -> opn,idx) opns))
    schedules

(** Displays the schedule of operator operatorname *)
and schedule_display_operator operatorposition (operatorname,(constants_list,operations_list))
    win graph succs preds schedules operator_cvs operation_frm schedule_info operator_frm =
  let h = Font.metrics font Linespace in
  let cvs = Graph_ctk.canvas graph in

  let time_max = time_max schedules in

  let opns_by_cond = split_by_cond schedules in

  let schedule_size = size_per_schedule opns_by_cond win cvs h in

  let constants_size = constants_required_size win h schedules in

  let factor =
    let space = match win.sch_direction with
      | VerticalSchedule -> Winfo.height cvs
      | HorizontalSchedule -> Winfo.width cvs in
      (float_of_int (space-constants_size))/.time_max in
  let vertice_delete _ = () in

  let constants_number = match win.sch_show_constants with
    | true -> List.length constants_list
    | false -> 0 in
  let opns_by_cond = List.assoc operatorname opns_by_cond in  
  let width_max =
    schedule_operator_display_operations operations_list schedule_size opns_by_cond
      operatorposition win factor constants_number h graph succs preds schedules
      vertice_delete in
  let width_max = match width_max,win.sch_direction with
  | 0,VerticalSchedule -> schedule_size
  | 0,HorizontalSchedule -> h+2*border
  | _ -> width_max in
  (match win.sch_show_constants with
  | true ->
      schedule_operator_display_constants constants_list width_max win
	operatorposition h constants_size graph schedules preds succs vertice_delete
  | false -> ());
  let oprpos = match win.sch_direction with
  | VerticalSchedule -> operatorposition,-constants_size
  | HorizontalSchedule -> -constants_size,operatorposition in
  let finalpos = int_of_float (time_max*.factor) in
  let operator_draw = operator_draw win graph operator_cvs finalpos width_max in
  let operator_move oprname (dx,dy) =
    let pos = List.assoc (oprname,win.sch_direction) win.sch_operators_positions in
    win.sch_operators_positions <- ((oprname,win.sch_direction),pos+dx+dy)::
      (List.remove_assoc (oprname,win.sch_direction) win.sch_operators_positions);
    refresh win graph operation_frm schedule_info operator_cvs operator_frm in
  let operator_enter = operator_enter win schedules in
  let operator_leave = operator_leave win schedules in
  let operator_select = operator_select graph operations_list in
  let refresh_ports_edges_f _ = ([],[]) in
  Graph_ctk.vertex_create graph operatorname Operator oprpos false
    [Graph_ctk.Movable] [] operator_draw operator_enter operator_leave
    operator_select operator_move vertice_delete operator_info
    refresh_ports_edges_f;
  let width = (match win.sch_direction,(constants_number<>0) with
  | HorizontalSchedule,true -> (constants_number-1)*schedule_size
  | _ -> 0)+width_max in
  operatorposition + width + border

(** Displays the schedules contained in schedule_info *)
and schedule_display win graph operation_frm schedule_info operator_cvs operator_frm =
  let schedules,preds,succs = schedule_info in
  let sorted_schedules = List.sort (function a,_ -> function b,_ ->
				      try
					let apos = List.assoc (a,win.sch_direction) win.sch_operators_positions in
					  try
					    compare apos (List.assoc (b,win.sch_direction) win.sch_operators_positions)
					  with _ -> -1
					  with _ -> 1) schedules in
    win.sch_operators_positions <- [];
    ignore (List.fold_left
	      (fun oprpos ((opr,_) as sch) ->
		win.sch_operators_positions <- ((opr,win.sch_direction),oprpos)::
		  win.sch_operators_positions;
		schedule_display_operator oprpos sch win graph succs preds
		  schedules operator_cvs operation_frm schedule_info operator_frm)
	      0 sorted_schedules);
  update_arrows win graph succs

let operation_graph_new schedule_view schedules operator_cvs operation_frm operator_frm wgt top =
  let edge_info_f _ _ = None
  and enter_f _ = schedule_info_change schedule_view schedules [] [] Canvas
  and leave_f _ = ()
  and clipboard = Graph_ctk.create_clipboard ()
  and copy_f _ = ()
  and vertex_paste_f _ _ _ = ()
  and edge_paste_f _ _ = ()
			   (* both scale and scroll must also affect
			      operator canvas. This is done via the
			      scroll and scale functions (called at each
			      scale or scroll) *)
  and scale_f operation_cvs s zoomdir = all_labels_refresh operation_cvs operator_cvs;
    match zoomdir,schedule_view.sch_direction with
    | "Vertical",VerticalSchedule | "Horizontal",HorizontalSchedule -> ()
    | _ -> let alltags = Canvas.find operator_cvs [All] in
      let factor = s /. !scale in
	scale := s;
	List.iter (fun tag -> let zx,zy,factorx,factory = Graph_ctk.conv_zoom operator_cvs factor zoomdir in
		     Canvas.scale operator_cvs tag (Pixels zx) (Pixels zy) factorx factory) alltags;
	    Graph_ctk.set_scrollregion operator_cvs
  and scroll_f_x sv = (match schedule_view.sch_direction with
		       | VerticalSchedule -> Canvas.xview operator_cvs sv
		       | HorizontalSchedule -> ());
    Graph_ctk.set_scrollregion operator_cvs
  and scroll_f_y sv = (match schedule_view.sch_direction with
		       | VerticalSchedule -> ()
		       | HorizontalSchedule -> Canvas.yview operator_cvs sv);
    Graph_ctk.set_scrollregion operator_cvs
  and vertice_list = ["Operator",Operator;"Operation",Operation]
  and edges_list = ["Dependence",Dependence] in
  Graph_ctk.create operation_frm wgt None enter_f leave_f edge_info_f clipboard
		copy_f vertex_paste_f edge_paste_f vertice_list edges_list Dependence
		scroll_f_x scroll_f_y scale_f true (function () -> top.tw_current_directory) (Some [operator_frm])

let schedule_new top title schedule_info =
  let wgt = Toplevel.create top.tw_frame [] in
    Wm.title_set wgt title;
    let schedules,_,_ = schedule_info in
    let menu_bar = frame_create wgt in
    let menu_create text =
      let btn = Menubutton.create menu_bar [Text text] in
      let mnu = Menu.create btn [TearOff false] in
	Menubutton.configure btn [Menu mnu];
	pack [btn] [Side Side_Left];
	text,mnu in
    let menus = List.fold_left (function menus -> function t -> (menu_create t)::menus) [] ["Window";"Edit"] in
    let schedule_view = {
      sch_top = top;
      sch_widget = wgt;
      sch_menus = menus;
      sch_direction = VerticalSchedule;
      sch_arrows = false;
      sch_labels = true;
      sch_fixed_colors = false;
      sch_operators_positions = [];
      sch_scale = true;
      sch_show_constants = false} in
      pack [menu_bar] [Fill Fill_X];
      let operation_frm = Frame.create wgt [] in
	(* I had to seperate operator and operation frames and canvases,
	   because we don't want the operators to be zoomed when operations
	   are zoomed *)
	(* Operator frame *)
      let operator_frm = Frame.create operation_frm [] in
      let operator_cvs = Canvas.create operator_frm [] in
	pack [operator_cvs] [Fill Fill_Both;Expand true];
	Canvas.configure operator_cvs [Width (Pixels operator_canvas_size_x);Height (Pixels operator_canvas_size_y)];
	(* There's no proper way to do it, but beware, these
	   increments should be the same as the ones of the operation
	   graph *)
	Canvas.configure operator_cvs [XScrollIncrement (Pixels 10)];
	Canvas.configure operator_cvs [YScrollIncrement (Pixels 10)];
	
	(* Operation frame *)
	pack [operation_frm] [Fill Fill_Both; Expand true];
	let graph = operation_graph_new schedule_view schedules operator_cvs operation_frm operator_frm wgt top in
	  Graph_ctk.canvas_size graph (operation_canvas_size_x,operation_canvas_size_y);
	  let resize _ = refresh schedule_view graph operation_frm schedule_info operator_cvs operator_frm in
	    Graph_ctk.configure_resize graph resize;
	    Graph_ctk.configure_scale graph 1. 100. 0.1;
	    let fixed_colors () =
	      schedule_view.sch_fixed_colors <- not schedule_view.sch_fixed_colors;
	      operation_color_remove schedule_view graph in
	      bind (Graph_ctk.canvas graph) [[],ButtonPressDetail 2] (BindSet ([],(function _ -> fixed_colors ())));
	      bind (Graph_ctk.canvas graph) [[Control],ButtonPressDetail 1] (BindSet ([],(function _ -> fixed_colors ())));
	      (* Frames and canvases will be required to repack them
		 when the schedule direction changes *)
	      schedule_view,graph,operation_frm,operator_cvs,operator_frm

let schedule_view top title schedule_info bindcommand =
  let schedule_view,graph,operation_frm,operator_cvs,operator_frm = schedule_new top title schedule_info in
    refresh schedule_view graph operation_frm schedule_info operator_cvs operator_frm;
    bindcommand schedule_view.sch_widget;
    bind schedule_view.sch_widget [[],Destroy] (BindSet ([], function _ -> schedule_view_close schedule_view));
    top.tw_schedules <- (schedule_view,graph) :: top.tw_schedules
