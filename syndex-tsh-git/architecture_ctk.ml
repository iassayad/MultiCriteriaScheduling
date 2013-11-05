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
open Graph_ctk

let ps n = print_string n;print_newline()

(* -- Conversion functions ------------------------------------------------ *)
let graphconnection_of_connection (on,gn,mn)=
  (on,gn,mn,"p",EConnection)

let string_of_bustype bustype = match bustype with
| SamPP -> "SAM Point to Point"
| SamMP -> "SAM MultiPoint"
| Ram -> "RAM"

(* -- Utilities ------------------------------------------------------------ *)
let archi_set_title wgt lib archi =
  let libm,archim = Application.archi_main_get () in
  let ttl = string_of_ref_type lib archi in
  let main = match Architecture.archi_is_main lib archi with
  | true -> " (main)"
  | false -> "" in
  let rw = match (lib<>"") with
  | true -> " - Read Only"
  | false -> "" in
  let ttl = "Architecture "^ttl^main^rw in
  Wm.title_set wgt ttl

let archi_operator_title win name deflib defname =
  let ttl = name^" ("^(string_of_ref_type deflib defname)^")"
  and main = match Architecture.operator_main_get win.arc_library win.arc_name with
  | n when n=name -> " (main)"
  | _ -> "" in
  (ttl^main)

let archi_media_title win name deflib defname =
  name^" ("^(string_of_ref_type deflib defname)^")"

let archi_update_main top =
  List.iter (function w,_ -> archi_set_title w.arc_widget w.arc_library w.arc_name) top.tw_archis

let archi_main_set win =
  win.arc_top.tw_file_changed ();
  Architecture.archi_main_set (win.arc_library,win.arc_name);
  archi_update_main win.arc_top

let architecture_elt_delete top name elts_list elt_infos =
  let elt_delete_in refname (arc_name,g) =
    let refs = elts_list (Architecture.architecturename_content "" arc_name) in
    let todelete = List.filter (function elt -> 
      let (_,eltlib,eltname) = elt_infos elt in 
      eltlib = "" && eltname = name) refs in
    List.iter (function elt ->
      let (eltrname,_,_) = elt_infos elt in
      Graph_ctk.vertex_remove_graph g (eltrname,VReference)) todelete in
  List.iter (function (w,g) -> match w.arc_library with
  | "" -> elt_delete_in name (w.arc_name,g)
  | _ ->  ()) top.tw_archis

(* ------------------------------------------------------------------------- *)
(*               INFO    FUNCTIONS                                           *)
(* ------------------------------------------------------------------------- *)

let archi_info_change infobar msgbar infotype = 
  let gate (t,n) = t^" "^n in
  let operator (n,dlib,dname) =
    let _,_,gates,_,descr,_ = Architecture.operator_definitionname_content dlib dname in
    let gates = List.fold_left (function s -> function g -> s^(gate g)^"\n") "" gates in
      match  descr with
      | "" -> "Operator "^n^" : "^(string_of_ref_type dlib dname)^ "\n"^gates
      | _ -> "Operator "^n^" : "^(string_of_ref_type dlib dname)^"\nDescription: \"" ^ descr ^ "\"\n"^gates in
  let mediadefinition (mdalib,mdaname) =
    let _,_,bustype,_,descr = Architecture.media_definitionname_content mdalib mdaname in
      match  descr with
      | "" -> "Media Definition : "^(string_of_ref_type mdalib mdaname)^ "\n"^(string_of_bustype bustype)
      | _ -> "Media Definition : "^(string_of_ref_type mdalib mdaname)^"\nDescription: \""^descr^"\"\n"^(string_of_bustype bustype) in
     
  let media (n,algolib,algoname) =
    let _,mdalib,mdaname,bc,_ = Architecture.media_referencename_content algolib algoname n in
    let _,_,bustype,_,_ = Architecture.media_definitionname_content mdalib mdaname in
    let bc = match bustype with
      | SamMP -> "Broadcast : "^(match bc with
				 | true -> "yes"
				 | false -> "no")
      | _ -> "" in
      "Media "^n^" : "^(string_of_ref_type mdalib mdaname)^"\n"^(string_of_bustype bustype)^"\n"^bc in
  let connection (oname,ogate,mname) = "Connection\n"^oname^"."^ogate^" -> "^mname in
    text_clean infobar;
    let info = match infotype with
      | Operator o -> operator o
      | Media m -> media m
      | MediaDefinition m -> mediadefinition m
      | Connection c -> connection c
      | Clean -> ""
      | _ -> "" in
      Text.insert infobar (TextIndex (End,[])) info []
	
(* ------------------------------------------------------------------------- *)
(*               GRAPHIC FUNCTIONS                                           *)
(* ------------------------------------------------------------------------- *)

(* -- Vertice port draw : used to draw a port of a vertex ----------------- *)
let port_draw cvs tag (pname,ptag) (x1,y1) (x2,y2) dx =
  let m = y1+(y2-y1)/2 in
  let cx1,cx2,textx,anchor = (x1-2,x2+dx+2,x1+(x2-x1)/2,N) in
  let idcnx = Canvas.create_rectangle cvs (Pixels cx1) (Pixels (m-3)) (Pixels cx2) (Pixels (m+3)) [Outline Black;FillColor Black; Tags [Tag tag; Tag ptag; Tag (ptag^"Cnx")]] in
  ignore(Canvas.create_rectangle cvs (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2) [FillColor White; Outline White; Tags [Tag tag; Tag ptag; Tag (ptag^"Highlight")]]);
  ignore(Canvas.create_text cvs (Pixels textx) (Pixels y1) [Text pname; Tags [Tag tag; Tag ptag]; Anchor anchor]);
  Canvas.bind cvs (Tag ptag) [[],Enter] (BindExtend ([], function _ ->
    Canvas.configure_rectangle cvs idcnx [Outline Red;FillColor Red]));
  Canvas.bind cvs (Tag ptag) [[],Leave] (BindExtend ([], function _ ->
    Canvas.configure_rectangle cvs idcnx [Outline Black;FillColor Black]))

(* -- Operator draw -------------------------------------------------------- *)
let rec operator_draw cvs title (name,tag,pos) gates =
  let h = Font.metrics font Linespace in
  let r = h/2 in

  (* -- Items positions -- *)
  let box_x1,box_y1 = pos in (* Top-left corner of sw *)
  let ttl_width = get_text_width title in
  let sep_y = box_y1 + 2*border + h in (* horizontal separation line coordinate *)
  let gates_geom l = 
    List.fold_left (function (lengthmax,height) -> function (pname,_) -> (max lengthmax (get_text_width pname)),(height + h)) (0,0) l in 
  let gates_width, gates_height = gates_geom gates in
  let box_x2 = box_x1 + 2*border + (max ttl_width gates_width) in  (* Bottom-right corner *)
  let box_y2 = sep_y + gates_height in
  let ttl_x, ttl_y = (box_x1+box_x2)/2 , box_y1+border+h/2 in

  (* -- Items drawing -- *)
  (* Title text *)
  let id = Canvas.create_text cvs (Pixels ttl_x) (Pixels ttl_y) [Font font;Text title; Tags [Tag tag;Tag (tag^"name")]] in
  (* Gates drawing *)
  ignore (List.fold_left (function y -> function p ->
    port_draw cvs tag p (box_x1,y) (box_x2, y+h) 0; y+h) sep_y gates);
  (* Title rectangle *)
  let id = Canvas.create_rectangle cvs (Pixels box_x1) (Pixels box_y1) 
      (Pixels (box_x2)) (Pixels sep_y) [FillColor operator_color; Tags [Tag tag; Tag (tag^"Highlight")]] in
  Canvas.lower_below cvs id (Tag tag);
  (* Black rectangle around ports *)
  let id=Canvas.create_rectangle cvs (Pixels box_x1) (Pixels sep_y) 
      (Pixels box_x2) (Pixels box_y2) [Tags [Tag tag;Tag (tag^"Highlight")]] in
  Canvas.raise_top cvs (Tag tag)

and operator_draw_graph win graph ((name,tag,_) as opr) =
  let (_,deflib,defname,_) = Architecture.operator_referencename_content win.arc_library win.arc_name name in
  let (_,_,gates,_,_,_) = Architecture.operator_definitionname_content deflib defname in
  let ttl = archi_operator_title win name deflib defname in
  let cvs = Graph_ctk.canvas graph in
  let gates = List.map (function (_,pname) ->
    let ptag = Graph_ctk.tag_of_port graph (name,VOperator) (pname,PInOut) in
    pname,ptag) gates in
  operator_draw cvs ttl opr gates;
  Canvas.bind cvs (Tag tag) [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> operator_definition_edit_libname win.arc_top win.arc_bindcommand (deflib,defname)))

and operator_draw_canvas cvs deflib defname =
  let (_,_,gates,_,_,_) = Architecture.operator_definitionname_content deflib defname in
  let ttl = string_of_ref_type deflib defname in
  let gates = List.map (function (_,pname) -> pname,"gate") gates in
  operator_draw cvs ttl (ttl,"operator",(30,30)) gates

(* -- Media draw ----------------------------------------------------------- *)
and media_draw cvs ttl (name,tag,pos) ptag =
  let h = Font.metrics font Linespace in
  let r = h/2 in

  (* -- Items positions -- *)
  let box_x1,box_y1 = pos in (* Top-left corner of sw *)
  let ttl_width = get_text_width ttl in
  let box_x2 = box_x1 + 2*border + ttl_width in
  let box_y2 = box_y1 + 2*border + h in
  let ttl_x, ttl_y = box_x1+border , (box_y1+box_y2)/2 in

  let oval_x1 = (box_x1+box_x2)/2 - r in
  let oval_x2 = (box_x1+box_x2)/2 + r in
  let oval_y1 = box_y2+border in
  let oval_y2 = oval_y1 + 2*r in
  let cnx_x = (oval_x1+oval_x2)/2 in
  let cnx_y = (oval_y1+oval_y2)/2 in

  (* -- Items drawing -- *)
  ignore(Canvas.create_rectangle cvs (Pixels box_x1) (Pixels box_y1) (Pixels box_x2) (Pixels box_y2) 
	   [FillColor media_color; Tags [Tag tag;Tag (tag^"Highlight")]]);
  ignore(Canvas.create_text cvs (Pixels ttl_x) (Pixels ttl_y) [Font font;Text ttl; Anchor W; Tags [Tag tag; Tag ptag; Tag (tag^"name")]]);
  ignore(Canvas.create_oval cvs (Pixels oval_x1) (Pixels oval_y1) (Pixels oval_x2) (Pixels oval_y2) 
	   [FillColor media_color; Tags [Tag tag; Tag ptag; Tag (ptag^"Cnx"); Tag (tag^"Highlight")]])

and media_draw_graph win graph ((name,tag,_) as mda) =
  let cvs = Graph_ctk.canvas graph in
  let (_,mdalib,mdaname,_,_) = Architecture.media_referencename_content win.arc_library win.arc_name name in
  let ptag = Graph_ctk.tag_of_port graph (name,VMedia) ("p",PInOut) in
  let ttl = archi_media_title win name mdalib mdaname in
  media_draw cvs ttl mda ptag;
  Canvas.bind cvs (Tag tag) [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> media_definition_edit_libname win.arc_top win.arc_bindcommand (mdalib,mdaname)))

and media_draw_canvas cvs mdalib mdaname =
  let ttl = string_of_ref_type mdalib mdaname in
  media_draw cvs ttl (ttl,"media",(30,30)) "port"

(* -- Connection draw ------------------------------------------------------ *)
and connection_draw win g _ (tag,stag,dtag) ((sname,stype,sport),(dname,dtype,dport),_) =
  let cvs = Graph_ctk.canvas g in
  let otag,mtag = match stype,dtype with
  | VMedia,VOperator -> dtag,stag
  | VOperator,VMedia -> stag,dtag
  | _ -> "","" in
  let x1,y1,x2,y2 = Canvas.bbox cvs [Tag (otag^"Cnx")] in
  let x1',y1',x2',y2' = Canvas.bbox cvs [Tag (mtag^"Cnx")] in
  let mx,my = (x1'+x2')/2,(y1'+y2')/2 in

  let ox = match abs(x1-mx) < abs(x2-mx) with
  | true -> x1
  | false -> x2 in  
  let oy = (y1+y2)/2 in

  let h = Font.metrics font Linespace in
  let r = h/2 in

  let l = int_of_float (sqrt (float_of_int ((mx-ox)*(mx-ox)+(my-oy)*(my-oy)))) in
  let x = ((l-r)*mx+r*ox)/l in
  let y = ((l-r)*my+r*oy)/l in

  ignore(Canvas.create_line cvs [Pixels ox;Pixels oy;Pixels x;Pixels y] [Tags [Tag tag; Tag (tag^"Highlight")]])

(* ------------------------------------------------------------------------- *)
(*              ITEM MANIPULATION FUNCTIONS                                  *)
(* ------------------------------------------------------------------------- *)

(* -- Delete/Add  Vertice/Edge --------------------------------------------- *)
(* Raise failure if src is not connectable to dst *)
and connect win ((sname,stype,sport),(dname,dtype,dport),_) =
  try
    let oname,gname,mname = match stype,dtype with
    | VMedia,VOperator -> dname,dport,sname
    | VOperator,VMedia -> sname,sport,dname
    (* Only need to check this kind of connection in the ctk part. This won't occur in a file. *)
    | _ -> failwith "Connections only between an operator and a media." in
    Architecture.connection_add win.arc_library win.arc_name ((oname,gname),mname);
    win.arc_top.tw_file_changed ()
  with Failure msg ->
    error_message win.arc_top msg;
    failwith ""
 	
and archi_edge_delete win ((sname,stype,sgate),(dname,dtype,dgate),_) =
  match stype,dtype with
  | VMedia,VOperator ->
      win.arc_top.tw_file_changed ();
      Architecture.connection_delete win.arc_library win.arc_name dname dgate sname 
  | VOperator,VMedia ->
      win.arc_top.tw_file_changed ();
      Architecture.connection_delete win.arc_library win.arc_name sname sgate dname 
  | _ -> ()

and archi_edge_add win g etype =
  let spctags = [Graph_ctk.Zoomable] in
  let infobar,msgbar = win.arc_top.tw_info_bar, win.arc_top in
  let enter_f ((sname,stype,sport),(dname,dtype,dport),_) = match stype,dtype with
  | VMedia,VOperator -> archi_info_change infobar msgbar (Connection (dname,dport,sname))
  | VOperator,VMedia -> archi_info_change infobar msgbar (Connection (sname,sport,dname))
  | _ -> () in
  let leave_f _ = archi_info_change infobar msgbar Canvas in
  let select_f _ = () in
  let delete = archi_edge_delete win in
  let draw = connection_draw win g in
  let changetype _ _ = () in
  let infostr _ = "" in
  Some (spctags,draw,enter_f,leave_f,select_f,changetype,delete,infostr)

and graph_operator_add (win,g) selected (oname,odeflib,odefname,pos) =
  let gates odeflib odefname =
    let (_,_,ogates,_,_,_) = Architecture.operator_definitionname_content odeflib odefname in
    List.map (function (_,name) -> (name,PInOut,connect win,[EConnection])) ogates in
  let infobar,msgbar = win.arc_top.tw_info_bar, win.arc_top in
  let draw_f = operator_draw_graph win g in
  let enter_f _ = archi_info_change infobar msgbar (Operator (oname,odeflib,odefname)) in
  let leave_f _ = archi_info_change infobar msgbar Canvas in
  let select_f _ = () in
  let move_f _ (dx,dy) = match dx,dy with
  | 0,0 -> ()
  | _ ->
      win.arc_top.tw_file_changed ();
      Architecture.operator_move win.arc_library win.arc_name oname (dx,dy) in
  let delete_f _ =
    win.arc_top.tw_file_changed ();
    Architecture.operator_reference_delete win.arc_library win.arc_name oname in
  let infostr_f _ = "" in
  let refresh_ports_edges_f _ =
    let (_,_,_,_,_,cncs,_,_) = Architecture.architecturename_content win.arc_library win.arc_name in
    let cncs = List.filter (function (oprref,_,_) -> oprref=oname) cncs in
    (gates odeflib odefname),(List.map graphconnection_of_connection cncs) in
  let pos = Coord.pos_of_coord2d pos in
  Graph_ctk.vertex_create g oname VOperator pos selected [Graph_ctk.Movable;Graph_ctk.Zoomable] (gates odeflib odefname) draw_f enter_f leave_f select_f move_f delete_f infostr_f refresh_ports_edges_f

(* Raise Failure if oname already exists in this architecture *)
and archi_operator_add (win,g) selected (oname,(odeflib,odefname),pos) =
  win.arc_top.tw_file_changed ();
  Architecture.operator_reference_add win.arc_library win.arc_name (oname,(odeflib,odefname),pos);
  graph_operator_add (win,g) selected (oname,odeflib,odefname,pos)

and archi_operator_add_ask (win,g) =
  let cvs = Graph_ctk.canvas g in
  let f oprlib oprname =
    let ask_for_fun names = 
      unselect_all cvs;
      let number = ref 0 in
      let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
      let oprs_list,err = analyze names rgxp in
	(match err <> "" with
	 | true -> error_message win.arc_top err
	 | false -> ());
      let rec op_add l = match l with
      | [opr]::t ->
	  number := !number + 1;
	  let (x,y) = conv_window_canvas cvs (!number*20) (!number*20) in
	  let pos = Coord.Coord2d (x,y) in
	  (try archi_operator_add (win,g) true (opr,(oprlib,oprname),pos)
	  with Failure msg -> error_message win.arc_top msg);
	  op_add t
      	| _ -> () in
      op_add oprs_list in
    ask_for "" "syntax: operator_reference_name1 operator_reference_name2 ...\n example: proc1 proc2 proc3" "Name of the new operator : " ask_for_fun win.arc_widget win.arc_top (WAsk_operator_reference_name (win.arc_library,win.arc_name))in
  let olist = Architecture.operatortypes_list () in
  let title = "Architecture "^win.arc_name^",\nChoose operator to reference :" in
    choose_libs_defs (function (oprlib,oprname) -> f oprlib oprname) olist title win.arc_widget win.arc_top (WAsk_operator_reference_add (win.arc_library,win.arc_name))

and graph_media_add (win,g) selected (mname, mdeflib, mdefname, broadcast, pos) =
  let draw_f = media_draw_graph win g in
  let infobar,msgbar = win.arc_top.tw_info_bar, win.arc_top in
  let enter_f _ = archi_info_change infobar msgbar (Media (mname,win.arc_library,win.arc_name)) in
  let leave_f _ = archi_info_change infobar msgbar Canvas in
  let select_f _ = () in
  let move_f _ (dx,dy) = match dx,dy with
  | 0,0 -> ()
  | _ ->
      win.arc_top.tw_file_changed ();
      Architecture.media_move win.arc_library win.arc_name mname (dx,dy) in
  let delete_f _ =
    win.arc_top.tw_file_changed ();
    Architecture.media_reference_delete win.arc_library win.arc_name mname in
  let infostr_f _ = "" in
  let refresh_ports_edges_f _ = ([],[]) in
  let port = ("p",PInOut,connect win,[EConnection]) in
  let pos = Coord.pos_of_coord2d pos in
  Graph_ctk.vertex_create g mname VMedia pos selected [Graph_ctk.Movable;Graph_ctk.Zoomable] [port] draw_f enter_f leave_f select_f move_f delete_f infostr_f refresh_ports_edges_f

(* Raise Failure if mname already exists in this architecture *)
and archi_media_add (win,g) selected (mname, mdeflib, mdefname, broadcast, pos) =
  win.arc_top.tw_file_changed ();
  Architecture.media_reference_add win.arc_library win.arc_name (mname,(mdeflib,mdefname),broadcast,pos);
  graph_media_add (win,g) selected (mname, mdeflib, mdefname, broadcast, pos)

and archi_media_add_ask (win,g) =
  let cvs = Graph_ctk.canvas g in
  let f mdalib mdaname =
    let ask_for_fun names =
      unselect_all cvs;
      let number = ref 0 in
      let rgxp= "\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
      let mdas_list,err = analyze names rgxp in
	(match err <> "" with
	 | true -> error_message win.arc_top err
	 | false -> ());
      let rec f l = match l with
      | [mda]::t -> 
	  number := !number + 1;
	  let (x,y) = conv_window_canvas cvs (!number*20) (!number*20) in
	  let pos = Coord.Coord2d (x,y) in
	  (try archi_media_add (win,g) true (mda,mdalib,mdaname,true,pos)
	  with Failure msg -> error_message win.arc_top msg);
	  f t
      | _ -> () in
      f mdas_list in
    ask_for "" "syntax: media_reference_name1 media_reference_name2 ...\n example: bus1 bus2" "Name of the new media : " ask_for_fun win.arc_widget win.arc_top (WAsk_media_reference_name (win.arc_library,win.arc_name))in
  let mlist = Architecture.mediatypes_list () in
  let title = "Architecture "^win.arc_name^",\nChoose media to reference :" in
  choose_libs_defs (function (mprlib,mprname) -> f mprlib mprname) mlist title win.arc_widget win.arc_top (WAsk_media_reference_add (win.arc_library,win.arc_name))

and graph_connection_add (win,g) cnc =
  let cnc = graphconnection_of_connection cnc in
  Graph_ctk.edge_create_from_etype g cnc

and gates_analyze top gates =
  let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ ]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
  let gates,err = analyze gates rgxp in
    (match err <> "" with
     | true -> error_message top err
     | false -> ());
  let rec f l = match l with
  | [media;gate]::t -> (media,gate) :: (f t)
  | _ -> [] in
  f gates

and operator_durations_edit top win oprlib oprname =
  let _,_,_,drs,_,_ = Architecture.operator_definitionname_content oprlib oprname in
  match edit win (string_of_durations drs) "syntax: operation_definition_name = duration\n example: calcul = 5\n int/Arit_add = 3\n"
    ("Timings of "^(string_of_ref_type oprlib oprname)^" :") with
  | Some durations ->
      top.tw_file_changed ();
      let durations,err = durations_analyze durations in
	(match err <> "" with
	 | true -> error_message top err
	 | false -> ());
      Architecture.operator_durations_save oprlib oprname durations
  | None -> ()

and media_durations_edit top win mdalib mdaname =
  let _,_,_,drs,_ = Architecture.media_definitionname_content mdalib mdaname in
  match edit win (string_of_durations drs) "syntax: data_type = duration\n example: image = 100\n int = 1" ("Timings of "^(string_of_ref_type mdalib mdaname)^" :") with
  | Some durations ->
      top.tw_file_changed ();
      let durations,err = durations_analyze durations in
	(match err <> "" with
	 | true -> error_message top err
	 | false -> ());
      Architecture.media_durations_save mdalib mdaname durations
  | None -> ()

and operator_redraw top cvs lib name =
  Canvas.delete cvs (Canvas.find cvs [All]);
  operator_draw_canvas cvs lib name;
  let win_operator_redraw (win,g) =
    let (_,_,oprlist,_,_,_,_,_) = Architecture.architecturename_content win.arc_library win.arc_name in
    List.iter (function (refname,oprlib,oprname,_) -> match oprlib=lib && oprname=name with
    | true -> Graph_ctk.vertex_refresh_graph g (refname,VOperator)
    | false -> ()) oprlist in
  List.iter win_operator_redraw top.tw_archis

and operator_gates_edit top win cvs (oprlib,oprname) =
  let (_,_,gates,_,_,_) = Architecture.operator_definitionname_content oprlib oprname in
  let oldgates = List.fold_left (function s -> function media,gate -> s^media^" "^gate^"\n") "" gates in
  match edit win oldgates "syntax: medium_definition_name gate_name\n example: SamPointToPoint comm1\n TCP x" ("Gates of "^oprname^" :") with
  | Some gates ->
      top.tw_file_changed ();
      Architecture.operator_gates_save oprlib oprname (gates_analyze top gates);
      operator_redraw top cvs oprlib oprname
  | None -> ()

and operator_code_phases_edit window top oprlib oprname =
  let phases_content_f oprlib oprname = 
    let  (_,_,_,_,_,code_phases) = Architecture.operator_definitionname_content oprlib oprname in
      code_phases in
  let phases_modify_f oprlib oprname code_phases =
    Architecture.operator_modify_code_phases oprlib oprname code_phases in
    code_phases_edit window top oprlib oprname [InitSeq;EndSeq] phases_content_f phases_modify_f
      
and operator_definition_modify top (lib,name) bindcommand =
  let window = Toplevel.create top.tw_frame [] in
  Wm.title_set window syndex_version_msg;
  let frm = frame_create window in
  let cvs = Canvas.create window [] in
  pack [frm] [Fill Fill_X];
  pack [cvs] [Fill Fill_Both];

  let btns = ["Modify gates",(function () -> operator_gates_edit top window cvs (lib,name));
	      "Modify durations",(function () -> operator_durations_edit top window lib name);
	      "Modify code generation phases",(function () -> operator_code_phases_edit window top lib name)] in
  List.iter (function name,f ->
    let btn = button_create frm name Raised in
    Button.configure btn [Command f];
    pack [btn] [Side Side_Left]) btns;
  operator_draw_canvas cvs lib name;
  let infobar,msgbar = top.tw_info_bar, top.tw_msg_bar in
  bind cvs [[],Enter] (BindExtend ([], function _ -> archi_info_change infobar msgbar (Operator ("Definition",lib,name))));
  bind cvs [[],Leave] (BindExtend ([], function _ -> archi_info_change infobar msgbar Clean));
  bind cvs [[],Destroy] (BindExtend ([], function _ -> operator_close top (lib,name)));
  (match bindcommand with
   | Some f -> f window;
   | None -> ());
  window

and media_definition_modify top bindcommand (lib,name) =
  let window = Toplevel.create top.tw_frame [] in
  Wm.title_set window syndex_version_msg;
  let frm = frame_create window in
  let cvs = Canvas.create window [] in
  pack [frm] [Fill Fill_X];
  pack [cvs] [Fill Fill_Both];

  let media_redraw lib name =
    let win_media_redraw (win,g) =
      let (_,_,_,_,mdalist,_,_,_) = Architecture.architecturename_content win.arc_library win.arc_name in
      List.iter (function (refname,mdalib,mdaname,_,_) -> match mdalib=lib && mdaname=name with
      | true -> Graph_ctk.vertex_refresh_graph g (refname,VMedia)
      | false -> ()) mdalist in
    List.iter win_media_redraw top.tw_archis in

  let change_type () =
    let update_bustype bustype =
      top.tw_file_changed ();
      let bustype = match bustype with
      | "SAM Point to Point" -> SamPP
      | "SAM MultiPoint" -> SamMP
      | "RAM" -> Ram
      | _ -> Ram in
      Architecture.media_bustype_change lib name bustype in
    let (_,_,bustype,_,_) = Architecture.media_definitionname_content lib name in
    let options = [Checkbutton,["SAM Point to Point";"SAM MultiPoint";"RAM"],string_of_bustype bustype,update_bustype] in
    options_box window syndex_version_msg "Bus Type" options (function () -> ()) in

  let btns = ["Modify type",change_type;
	      "Modify durations",(function () -> media_durations_edit top window lib name)] in
  List.iter (function name,f ->
    let btn = button_create frm name Raised in
    Button.configure btn [Command f];
    pack [btn] [Side Side_Left]) btns;
  media_draw_canvas cvs lib name;
  let infobar,msgbar = top.tw_info_bar, top.tw_msg_bar in
  bind cvs [[],Enter] (BindExtend ([], function _ -> archi_info_change infobar msgbar (MediaDefinition (lib,name))));
  bind cvs [[],Leave] (BindExtend ([], function _ -> archi_info_change infobar msgbar Clean));
  bind cvs [[],Destroy] (BindExtend ([], function _ -> media_close top (lib,name)));  

  (match bindcommand with
   | Some f -> f window;
   | None -> ());
  window

and operator_definition_edit_libname top bindcommand oprdef =
  generic_window_open top (WOperator oprdef) (function () -> operator_definition_modify top oprdef bindcommand)

and operator_definition_edit top bindcommand =
  let olist = Architecture.operatortypes_list () in
  let title = "Choose operator to edit :" in
  choose_libs_defs (operator_definition_edit_libname top bindcommand) olist title Widget.default_toplevel top WAsk_edit_operator

and operator_definition_new top bindcommand =
  let ask_for_fun operatorname =
    try
      Architecture.operator_definition_create "" operatorname [] [] "" [LoopSeq];
      top.tw_file_changed ();
      operator_definition_edit_libname top bindcommand ("",operatorname);
      List.iter archi_menus_update top.tw_archis
    with Failure msg -> error_message top msg in
  ask_for "" "syntax: operator_definition_name\n" "Enter the name of the new operator definition" ask_for_fun Widget.default_toplevel top WAsk_create_operator
	
and graph_operator_delete top operatorname =
  let elt_list (_,_,oprlist,_,_,_,_,_) = oprlist in
  let elt_info (oprref_name,oprlib,oprname,_) = (oprref_name,oprlib,oprname) in
  architecture_elt_delete top operatorname elt_list elt_info
	
and operator_definition_delete top operatorname =
  top.tw_file_changed ();
  (* Only local operators are deletable, so the operator reference is ("",operatorname) *)
  operator_close top ("",operatorname);
  graph_operator_delete top operatorname;
  Architecture.operator_definition_delete "" operatorname

and media_definition_edit_libname top bindcommand mdadef =
  generic_window_open top (WMedia mdadef) (function () -> media_definition_modify top bindcommand mdadef)

and media_definition_edit top bindcommand =
  let mlist = Architecture.mediatypes_list () in
  let title = "Choose media to edit :" in
  choose_libs_defs (media_definition_edit_libname top bindcommand) mlist title Widget.default_toplevel top WAsk_edit_media

and media_definition_new top bindcommand =
  let ask_for_fun medianame =
    try
      Architecture.media_definition_create "" medianame SamPP [] "";
      top.tw_file_changed ();	
      media_definition_edit_libname top bindcommand ("",medianame);
      List.iter archi_menus_update top.tw_archis
    with Failure msg -> error_message top msg in
  ask_for "" "syntax: medium_definition_name" "Enter the name of the new medium definition" ask_for_fun Widget.default_toplevel top WAsk_create_media

and graph_media_delete top medianame =
  let elt_list (_,_,_,_,mdalist,_,_,_) = mdalist in
  let elt_info (mdaref_name, mdalib, mdaname,_,_) = (mdaref_name,mdalib,mdaname) in
  architecture_elt_delete top medianame elt_list elt_info

and media_definition_delete top medianame =
  top.tw_file_changed ();
  (* Only local medias are deletable, so the media reference is ("",medianame) *)
  media_close top ("",medianame);
  graph_media_delete top medianame;
  Architecture.media_definition_delete "" medianame

and media_close top mdadef =
  generic_window_close top (WMedia mdadef)

and operator_close top oprdef =
  generic_window_close top (WOperator oprdef)

(* -- Copy and Paste functions --------------------------------------------- *) 
and copy win (verticesid,_) =
  let mediasid,operatorsid = List.partition (function (_,vtype) -> vtype = VMedia) verticesid in
  win.arc_top.tw_arc_clip <-
    ((List.map (function n,_ -> Architecture.media_referencename_content win.arc_library win.arc_name n) mediasid),
     (List.map (function n,_ -> Architecture.operator_referencename_content win.arc_library win.arc_name n) operatorsid))

and vertex_paste win g (vold,vtype) vnew =
  let medias,operators = win.arc_top.tw_arc_clip in
  match vtype with
  | VMedia -> 
      let (_,deflib,defname,broadcast,pos) = List.find (function (name,_,_,_,_) -> name=vold) medias in
      archi_media_add (win,g) true (vnew,deflib,defname,broadcast,pos)
  | VOperator ->
      let (_,deflib,defname,pos) = List.find (function (name,_,_,_) -> name=vold) operators in
      archi_operator_add (win,g) true (vnew,(deflib,defname),pos)
	
and edge_paste win g dpd = 
  connect win dpd;
  let ((sname,stype,sport),(dname,dtype,dport),_) = dpd in
  match stype,dtype with
  | VMedia,VOperator -> graph_connection_add (win,g) (dname,dport,sname)
  | VOperator,VMedia -> graph_connection_add (win,g) (sname,sport,dname)
  | _ -> ()
	
(* ------------------------------------------------------------------------- *)
(*              WINDOWS MANIPULATION & DECORATION FUNCTIONS                  *)
(* ------------------------------------------------------------------------- *)

(* -- Close functions ------------------------------------------------------ *)
and archi_close top archi =
  generic_window_close top (WArchitecture archi);
  top.tw_archis <- List.filter (function (a,_) -> (a.arc_library,a.arc_name)<>archi) top.tw_archis

and archi_close_others top archi =
  List.iter (function (a,_) -> archi_close top (a.arc_library,a.arc_name)) (List.filter (function (a,_) -> (a.arc_library,a.arc_name)<>archi) top.tw_archis)

and archi_close_all top =
  List.iter (function (a,_) -> archi_close top (a.arc_library,a.arc_name)) top.tw_archis

(* -- Computes a position for operators and medias ------------------------- *)
and auto_position (win,g) =
  let origin =
    let cvs = Graph_ctk.canvas g in
    conv_window_canvas cvs 0 0 in
  let step_sizes = autopos_step_box win.arc_widget in
  Graph_ctk.place_non_oriented g origin step_sizes

(* -- Update menus --------------------------------------------------------- *)
and archi_menus_update (win,g) =
  let menu_1,menu_2 =
    List.assoc "Window" win.arc_menus,
    List.assoc "Edit" win.arc_menus in
  clear_menu menu_1;
  clear_menu menu_2;
  let menu_cvs = Graph_ctk.get_menu_canvas g in
  let menu_operator = Graph_ctk.get_menu_vertex g VOperator in
  let menu_media = Graph_ctk.get_menu_vertex g VMedia in
  let archi_is_readonly = win.arc_library<>"" in

  Graph_ctk.update_menu_canvas g menu_cvs;
  Menu.add_separator menu_cvs;
  Graph_ctk.update_menu_canvas g menu_2;
  Menu.add_separator menu_2;

  (* Refresh command *)
  let f wgt = Menu.add_command wgt [Label "Refresh"; Accelerator "Ctrl-R";Command (function () -> archi_display (win,g))] in
  bind win.arc_widget [[Control],KeyPressDetail "r"] (BindSet ([], function _ -> archi_display (win,g)));
  f menu_1;
  (* Fit on grid command *)
  let f wgt = Menu.add_command wgt [Label "Align"; Command (function () -> graph_fit g)] in
  f menu_1;
  (* Auto position command *)
  let f wgt = Menu.add_command wgt [Label "Auto position"; Command (function () -> auto_position (win,g))] in
  f menu_1;

  Menu.add_separator menu_1;

  (* Close command *)
  let f wgt = Menu.add_command wgt [Label "Close"; Accelerator "Ctrl-W"; Command (function () -> archi_close win.arc_top (win.arc_library,win.arc_name))] in
  bind win.arc_widget [[Control],KeyPressDetail "w"] (BindSet ([], function _ -> archi_close win.arc_top (win.arc_library,win.arc_name)));
  f menu_1;
  (* Close others command *)
  let f wgt = Menu.add_command wgt [Label "Close Others"; Command (function () -> archi_close_others win.arc_top (win.arc_library,win.arc_name))] in
  f menu_1; 
  (* Close all command *)
  let f wgt = Menu.add_command wgt [Label "Close All"; Command (function () -> archi_close_all win.arc_top)] in
  f menu_1; 

  (* Create media & create operator *)
  let state = match  (not archi_is_readonly) && ((Architecture.operatortypes_list ())<>[]) with
  | true -> Normal
  | false -> Disabled in
  let f wgt = Menu.add_command wgt [Label "Create Operator Reference"; Command (function () -> archi_operator_add_ask (win,g)); State state] in
  f menu_2; f menu_cvs;
  let state = match  (not archi_is_readonly) && ((Architecture.mediatypes_list ())<>[]) with
  | true -> Normal
  | false -> Disabled in
  let f wgt = Menu.add_command wgt [Label "Create Medium Reference"; Command (function () -> archi_media_add_ask (win,g)); State state] in
  f menu_2; f menu_cvs;
  let f wgt = Menu.add_command wgt [Label "Find Operator Reference"; Command (function () -> archi_operator_select (win,g))] in
  f menu_2; f menu_cvs;
  let f wgt = Menu.add_command wgt [Label "Find Medium Reference"; Command (function () -> archi_medium_select (win,g))] in
  f menu_2; f menu_cvs;
  Menu.add_separator menu_2;
  Menu.add_separator menu_cvs;

  (* Main architecture command *)
  bind win.arc_widget [[Control],KeyPressDetail "m"] (BindSet ([], function _ -> archi_main_set win;archi_update_main win.arc_top));
  let f wgt = Menu.add_command wgt [Label "Main Architecture"; Accelerator "Ctrl-M"; Command (function () -> archi_main_set win;archi_update_main win.arc_top)] in
  f menu_2; f menu_cvs;
  (* Update media and operator menus *)
  Graph_ctk.update_menu_vertices g;
  Menu.add_separator menu_media;
  Menu.add_separator menu_operator;
  let state = match (not archi_is_readonly) with
  | true -> Normal
  | false -> Disabled in
  let f wgt = Menu.add_command wgt [Label "Modify"; Command (function () -> archi_selection_modify (win,g)); State state] in
  f menu_media; f menu_operator;
 
  (* Main operator command *)
  let f wgt = Menu.add_command wgt [Label "Main Operator"; Command (function () -> archi_main_operator (win,g)); State state] in
  f menu_operator; 

  (* Broadcast *)
  Menu.add_command menu_media [Label "Properties"; Command (function () -> archi_selection_broadcast (win,g))]

(* -- "Modify" functions --------------------------------------------------- *)
and archi_media_modify (win,g) name =
  let ask_for_fun s =
    win.arc_top.tw_file_changed ();
    Architecture.media_reference_modify win.arc_library win.arc_name name s;
    archi_display (win,g) in
  ask_for name "" "Enter the new name of the medium" ask_for_fun win.arc_widget win.arc_top (WAsk_media_reference_modify (win.arc_library,win.arc_name,name))
	
and archi_operator_modify (win,g) name =
  let ask_for_fun s =
    win.arc_top.tw_file_changed ();
    Architecture.operator_reference_modify win.arc_library win.arc_name name s;
    archi_display (win,g) in
  ask_for name "" "Enter the new name of the operator" ask_for_fun win.arc_widget win.arc_top (WAsk_operator_reference_modify (win.arc_library,win.arc_name,name))

and archi_selection_modify (win,g) =
  let (verticesid,_) = Graph_ctk.selected g in
  List.iter (function (vname,vtype) -> 
    match vtype with
    | VMedia -> archi_media_modify (win,g) vname
    | VOperator -> archi_operator_modify (win,g) vname) verticesid

and archi_selection_broadcast (win,g) =
  let (verticesid,_) = Graph_ctk.selected g in
  let medias = List.filter (function (_,vtype) -> vtype = VMedia) verticesid in
  let bcs = Types.remove_copies (List.map (function (vname,_) ->
    let _,_,_,bc,_ = Architecture.media_referencename_content win.arc_library win.arc_name vname in
    bc) medias) in
  let broadcast_of_bool bc = match bc with
  | true -> "Broadcast"
  | false -> "No Broadcast" in
  let bool_of_broadcast bc = bc="Broadcast" in
  let bc = match List.length bcs with
  | 1 -> broadcast_of_bool (List.hd bcs)
  | _ -> "other" in
  let update_broadcast bc =
    List.iter (function (vname,_) ->
      win.arc_top.tw_file_changed ();
      Architecture.media_broadcast_change win.arc_library win.arc_name vname (bool_of_broadcast bc)) medias in
  let options = [Checkbutton,["Broadcast";"No Broadcast"],bc,update_broadcast] in
  options_box win.arc_widget syndex_version_msg "Options" options (function () -> ())

and archi_main_operator (win,g) =
  win.arc_top.tw_file_changed ();
  let (verticesid,_) = Graph_ctk.selected g in
  let operator = List.find (function (_,vtype) -> vtype = VOperator) verticesid in
  Architecture.operator_main_set (win.arc_library,win.arc_name) (fst operator);
  archi_display (win,g)

(* -- archi selection functions ------------------------------------------- *)

and archi_operator_select (win,g) =
  let select_f refname =
    Graph_ctk.select_vertex g (refname,VOperator) in
  let (_,_,oprrefs,_,_,_,_,_) = Architecture.architecturename_content win.arc_library win.arc_name in
  let refs_names = List.map (fun (ref_name,_,_,_) -> ("operators",ref_name)) oprrefs in
  let title = "Select operator reference to find:" in
    choose_libs_defs (function (_,refname) -> select_f refname) refs_names title win.arc_widget win.arc_top (WSelect_operator_reference win.arc_name)

and archi_medium_select (win,g) =
  let select_f refname =
    Graph_ctk.select_vertex g (refname,VMedia) in
  let (_,_,_,_,mdarefs,_,_,_) = Architecture.architecturename_content win.arc_library win.arc_name in
  let refs_names = List.map (fun (ref_name,_,_,_,_) -> ("media",ref_name)) mdarefs in
  let title = "Select medium reference to find:" in
    choose_libs_defs (function (_,refname) -> select_f refname) refs_names title win.arc_widget win.arc_top (WSelect_operator_reference win.arc_name)

(* -- archi display ------------------------------------------------------- *)
and archi_display (win,g) =
  Graph_ctk.reset g;
  let cvs = Graph_ctk.canvas g in
  let (_,_,operators,_,medias,connections,dim,_) = Architecture.architecturename_content win.arc_library win.arc_name in

  List.iter (graph_operator_add (win,g) false) operators;
  List.iter (graph_media_add (win,g) false) medias;
  List.iter (graph_connection_add (win,g)) connections;

  let x0,y0,_,_ = match Canvas.find cvs [All] with
  | [] -> 0,0,0,0
  | l -> Canvas.bbox cvs l in
  (match dim with
  | Coord.No_coord2d -> ()
  | Coord.Coord2d (x,y) -> win.arc_dimension <- x,y);
  Canvas.configure cvs [Width (Pixels (fst win.arc_dimension-2)); Height (Pixels (snd win.arc_dimension-2))];
  bind cvs [[], Destroy] (BindSet ([],function _ -> archi_close win.arc_top (win.arc_library,win.arc_name)));
  Canvas.configure cvs [ScrollRegion ((Pixels x0),(Pixels y0),(Pixels (Winfo.width cvs)),(Pixels (Winfo.height cvs)))];
  update ();
  Graph_ctk.configure_resize g (function (a,b) -> 
    win.arc_top.tw_file_changed ();
    win.arc_dimension <- (a,b); 
    Architecture.archi_dimension_window_change win.arc_library win.arc_name a b)

(* -- archi new ----------------------------------------------------------- *)
and archi_new_win top bindcommand lib name =
  let wgt = Toplevel.create top.tw_frame [] in
  archi_set_title wgt lib name;
  let menu_bar = frame_create wgt in
  let menu_create text =
    let btn = Menubutton.create menu_bar [Text text] in
    let mnu = Menu.create btn [TearOff false] in
    Menubutton.configure btn [Menu mnu];
    pack [btn] [Side Side_Left];
    text,mnu in
  let menus = List.fold_left (function menus -> function t -> (menu_create t)::menus) [] ["Window";"Edit"] in
  let bar = frame_create wgt  in
  pack [menu_bar; bar] [Fill Fill_X];
  let frm = Frame.create wgt [] in

  pack [frm] [Fill Fill_Both; Expand true];

  let win = {
    arc_top = top;
    arc_widget = wgt;
    arc_library = lib;
    arc_name = name;
    arc_menus = menus;
    arc_dimension = (0,0);
    arc_bindcommand = bindcommand;
  } in

  (* Graph creation *)
  let readonly = (lib<>"") in
  let vtypelist = ["Operator",VOperator; "Media",VMedia] in
  let etypelist = ["Connection",EConnection]in
  let infobar,msgbar = win.arc_top.tw_info_bar, win.arc_top.tw_msg_bar in
  let enter_f _ = archi_info_change infobar msgbar Canvas in
  let leave_f _ = archi_info_change infobar msgbar Clean in
  let clip = top.tw_arc_graphclip in
  let g = Graph_ctk.create frm win.arc_widget None enter_f leave_f (archi_edge_add win) clip (copy win) (vertex_paste win) (edge_paste win) vtypelist etypelist EConnection (fun _ -> ()) (fun _ -> ()) (fun _ _ _ -> ()) false (function () -> win.arc_top.tw_current_directory) None in
  Graph_ctk.configure_scale g 0.1 1. 0.01;
  (* Graph options *)
  let readonly = match lib with
  | "" -> "0"
  | _ -> "" in
  Graph_ctk.option_set g ReadOnly readonly;

  let cvs = Graph_ctk.canvas g in
  win.arc_dimension <- Winfo.width cvs,Winfo.height cvs;

  (* Bindings *)
  (match bindcommand with
  | Some f -> f wgt;
  | None -> ());

  top.tw_archis <- (win,g) :: top.tw_archis;
  (* Displays the window and the graph *)
  archi_display (win,g);
  (* Fill the menus *)
  archi_menus_update (win,g);
  wgt

and archi_open top bindcommand lib name =
  generic_window_open top (WArchitecture (lib,name)) (function () -> archi_new_win top bindcommand lib name)

let architecture_definition_edit top globalbindings =
  let title = "Choose architecture to edit :" in
  choose_libs_defs (function (archilib,archiname) -> archi_open top globalbindings archilib archiname) (Architecture.architectures_list ()) title Widget.default_toplevel top WAsk_edit_architecture

let architecture_definition_delete top archiname =
  top.tw_file_changed ();
  (match List.filter (function (w,_) -> w.arc_library="" && w.arc_name=archiname) top.tw_archis with
  | (wdw,_)::_ -> archi_close top (wdw.arc_library,wdw.arc_name)
  | _ -> ());
  Architecture.architecture_delete "" archiname

let architecture_definition_create top globalbindings =
  let ask_for_fun name =
    try
      Architecture.architecture_create "" name [] "" [] [] (Coord.Coord2d (canvas_size_default_x,canvas_size_default_y)) "";
      top.tw_file_changed ();
      archi_open top globalbindings "" name 
    with Failure msg -> error_message top msg in
  ask_for "" "syntax: architecture_name" "Name of the new architecture : " ask_for_fun Widget.default_toplevel top WAsk_create_architecture
