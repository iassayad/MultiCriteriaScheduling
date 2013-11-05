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
open Ihmcommon_ctk

(* -- Conversion functions ------------------------------------------------- *)
let labeldependence = [Strong_Precedence_Data,"Strong Precedence/Data communication";
		       Weak_Precedence_Data,"Weak Precedence/Data communication";
		       Precedence,"Precedence";
		       Data,"Data communication"]

let string_of_algoclass t =
  match t with
  | Operation -> "Function"
  | Constant -> "Constant"
  | Sensor -> "Sensor"
  | Actuator -> "Actuator"
  | Memory (a,b) -> "Delay ["^(Symbolic.string_of_expression a)^","^(Symbolic.string_of_expression b)^"]"
  | Internal -> "Internal"

let color_of_algoclass opntype =
  match opntype with
  | Constant -> constant_color
  | Sensor -> sensor_color
  | Actuator -> actuator_color
  | Operation -> operation_color
  | Memory _ -> memory_color
  | Internal -> operation_color
	
let string_of_typedpd t =
  List.assoc t labeldependence

let direction_of_string dir = match dir with
  | "In" -> Port.In
  | "Out" -> Port.Out
  | _ -> failwith "Algorithm_ctk.direction_of_string"

(* -- Utilities ------------------------------------------------------------ *)
(** Set the algorithm window title. *)
let algo_set_title wgt lib name path =
  let (_,_,algo_class,_,_,_,_,_,_,_) =  Algorithm.algorithmname_content lib name in
  let path = match Algorithm.algo_is_main lib name with
  | true -> "main"
  | false ->  string_of_path path in
  let rw = match (lib<>"") with
  | true -> " - Read Only"
  | false -> "" in
  let title = "Algorithm "^(string_of_algoclass algo_class)^" "^(string_of_ref_type lib name)^" ("^path^")"^rw in
  Wm.title_set wgt title

let algo_update_path win = ()

(** Returns [true] if the algorithm [name] in library [lib] can have ports in direction [dir]. *)
let algo_can_have_port lib name dir =
  let (_,_,algo_class,_,_,_,_,_,_,_) =  Algorithm.algorithmname_content lib name in
  match algo_class with
  | Operation -> true
  | Actuator -> (dir=Port.In)
  | Sensor -> (dir=Port.Out)
  | Constant -> (dir=Port.Out)
  | Memory _ -> true
  | Internal -> false

(* -- Port name convertion fonctions --------------------------------------- *)

let vport_of_port (port,dir) =
  match dir with
  | Port.In -> "port_i_"^port
  | Port.Out -> "port_o_"^port

let port_of_vport vport =
  let rgxp = Str.regexp "port_\\([i;o]\\)_\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
  let _ = Str.search_forward rgxp vport 0 in
  match (Str.matched_group 1 vport)="i" with
  | true -> ((Str.matched_group 2 vport),Port.In)
  | false -> ((Str.matched_group 2 vport),Port.Out)
	
let port_of_portid (vname,vtype,pname) =
  match vtype with
  | VReference -> (vname,pname)
  | VPort -> let (pname,_) = port_of_vport vname in ("",pname)

let edge_of_dpd ((v1_name,p1_name),(v2_name,p2_name),etype) =
  let (v1_name,p1_name) = match v1_name="" with
  | true -> (vport_of_port (p1_name,Port.In),"p")
  | false -> (v1_name,p1_name) in
  let (v2_name,p2_name) = match v2_name="" with
  | true -> (vport_of_port (p2_name,Port.Out),"p")
  | false -> (v2_name,p2_name) in
  v1_name,p1_name,v2_name,p2_name,etype

let algo_info_change win infotype =
  let (lib,name,algoclass,argsn,prts,_,_,_,algo_descr,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let infobar,msgbar = win.alg_top.tw_info_bar, win.alg_top.tw_msg_bar in
  let info = match algoclass with
    | Internal -> let algotype = string_of_ref_type lib name in
	"Definition Internal "^algotype^"\n"
    | _ -> 
	let context = Algorithm.context_global () in
	let varvalue expr path =
	  match expr with
	  | Symbolic.Float i when fst (modf i) = 0.0 -> string_of_int (int_of_float i)
	  | Symbolic.Float i -> string_of_float i
	  | _ ->
	      let exprg = Algorithm.globalexpression_of_localexpression path expr in
	      let varvalue = Symbolic.string_of_result (evaluation_expression context exprg msgbar (string_of_path path)) in
		(Symbolic.string_of_expression expr)^"="^varvalue in
	let argsvalues argsnames path =
	  match (List.map (function var ->
			     let varg = Algorithm.globalexpression_of_localexpression path (Symbolic.Var var) in
			       Symbolic.string_of_result (evaluation_expression context varg msgbar (string_of_path path))) argsnames) with
	  | [] -> ""
	  | l -> let s = List.fold_left2 (function s -> function n -> function v -> s^n^"="^v^";") "" argsnames l in
	      "<"^(String.sub s 0 (String.length s -1))^">" in
	let portslist title portslist path =
	  match portslist with
	  | [] -> ""
	  | _ ->
	      let port (name,_,typename,typedim,_,_,_) = typename^" "^name^"["^(varvalue typedim path)^"]"^"\n" in
		List.fold_left (function s -> function p -> s^(port p)) title portslist in
	let ports prts path =
	  let prts = List.filter (function (_,_,_,_,portclass,_,_) -> portclass <> Port.Precedence_Port) prts in
	  let pin,pout = List.partition (function (_,d,_,_,_,_,_) -> d=Port.In) prts in
	  let portsin,portsout,portsinout = (exclusion pin pout),(exclusion pout pin),(intersection pin pout) in
	  let portsin = portslist "Ports In\n" portsin path
	  and portsout = portslist "Ports Out\n" portsout path
	  and portsinout = portslist "Ports In Out\n" portsinout path in
	    portsin^"\n"^portsout^"\n"^portsinout in
	let port (pn,pd) =
	  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
	  let p = List.find (function (n,d,_,_,_,_,_) -> n=pn && d=pd) ports in
	    portslist "Port\n" [p] win.alg_path in
	let canvas () =
	  let algoclass = string_of_algoclass algoclass
	  and algotype = string_of_ref_type lib name
	  and portsstring = ports prts win.alg_path
	  and argsv = argsvalues argsn win.alg_path in
	    match  algo_descr with
	    | "" -> "Definition "^algoclass^" "^algotype^argsv^"\n\n"^portsstring^"\n"
	    | _ -> "Definition "^algoclass^" "^algotype^argsv^"\nDescription: \"" ^ algo_descr ^ "\"\n\n"^portsstring^"\n" in
	let reference n =
	  let (_,dlib,dname,argsv,_,_,_,description) = Algorithm.referencename_content win.alg_library win.alg_name n in
	  let (_,_,algoclass,argsnames,prts,_,_,_,_,_) = Algorithm.algorithmname_content dlib dname in
	  let algoclass = string_of_algoclass algoclass
	  and algotype = string_of_ref_type dlib dname in
	  let argsn = (string_of_argsvalues argsv)^" "
	  and argsv = argsvalues argsnames (win.alg_path@[n]) in
	  let desc_s = match description with
	    | "" -> ""
	    | _ -> "Description: \""^description^"\"\n" in
	  let xsc = let xscstr attach_type =
	    match Application.xscname_of_ref (win.alg_path@[n]) attach_type with
	    | "" -> ""
	    | xsc -> "Software component for "^(string_of_attachtype attach_type)^": "^xsc^"\n" in 
	    (xscstr AttachRef)^(xscstr AttachCondI)^(xscstr AttachCondO)^(xscstr AttachExplode)^(xscstr AttachImplode) in
	  let portsstring = ports prts (win.alg_path@[n]) in
	    "Reference: "^n^argsn^"\n"^desc_s^xsc^"\n\nDefinition: "^algoclass^" "^algotype^argsv^"\n"^portsstring in
	let dependence (ps,pd,dc) =
	  let refsrc,prtsrc = port_of_portid ps
	  and refdst,prtdst = port_of_portid pd in
	  let dpd = match dc with
	    | Precedence -> refsrc^" -> "^refdst
	    | _ ->
		let conv p = (match p with
			      | "",p -> p
			      | r,p -> r^"."^p) in
		  (conv (refsrc,prtsrc))^" -> "^(conv (refdst,prtdst)) in
	    "Edge\n"^(string_of_typedpd dc)^"\n"^dpd in
	  let info = match infotype with
	    | Port p -> port p
	    | Reference r -> reference r
	    | Dependence d -> dependence d
	    | Canvas -> canvas ()
	    | Clean -> ""
	    | _ -> "" in
	    info in
    text_clean infobar;
    Text.insert infobar (TextIndex (End,[])) info []

(* ------------------------------------------------------------------------- *)
(*               Repetition factor calcul functions                          *)
(* ------------------------------------------------------------------------- *)

(** Returns the size of port [portname] of definition referenced by [refname]. *)
let reference_port_size alglib algname refname portname =
  let (deflib,defname) = match refname with
    | "" -> (alglib,algname)
    | _ ->
	let (_,deflib,defname,_,_,_,_,_) = Algorithm.referencename_content alglib algname refname in
	  (deflib,defname) in
  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content deflib defname in
  let (_,_,_,prtdim,_,_,_) = try List.find (function (pname,_,_,_,_,_,_) -> pname = portname) ports
  with Not_found -> failwith ("Couldn't find "^portname^" in "^refname) in
    prtdim

(** Returns the common factor between a dependence and the reference it is connected; reference is [sref] if [dir] is [Out] and [dref] if [In]. *)
let common_factor alglib algname path ((sref,sprt),(dref,dprt)) factor dir context =
  let sdim = Algorithm.globalexpression_of_localexpression (path@[sref]) (reference_port_size alglib algname sref sprt) in
(*     ps ((string_of_string_list path " ")^","^sref^" -> "^Symbolic.string_of_expression sdim); *)
  let ddim = Algorithm.globalexpression_of_localexpression (path@[dref]) (reference_port_size alglib algname dref dprt) in
  let nexpr = match dir with
    | Port.In -> Symbolic.Bin(Symbolic.div,sdim,ddim)
    | Port.Out -> Symbolic.Bin(Symbolic.div,ddim,sdim) in
  let rep = match Symbolic.result_of_expression context nexpr with
    | Symbolic.RFloat i -> (match i < 1. with
			    | true -> Repeat 1
			    | false -> (match (i = float_of_int (int_of_float i)) with
					| false -> Error (* Factor is not an integer *)
					| true -> let i = int_of_float i in
					    (match factor with
					     | Calculated (Repeat j) -> (match (i mod j = 0) || (j mod i = 0) with
							    | true -> Repeat (max i j)
							    | false -> Error) (* Incompatible factors *)
					     | Calculated f -> f
					     | _ -> failwith "Algorithm_ctk.common_factor error")))
    | _ -> Undefinied in
    Calculated rep

(** Returns [true] if adding dependence [((sref,sprt),(dref,dprt))] is compatible with the repetition factor [repetition]. *)
let compatible_factor alglib algname path ((sref,sprt),(dref,dprt)) repetition dir context =
  match repetition with
  | Calculated Error -> false
  | Calculated Undefinied -> true
  | Calculated (Repeat i) -> let new_factor = common_factor alglib algname path ((sref,sprt),(dref,dprt)) (Calculated (Repeat i)) dir context in
      (match new_factor with
      | Calculated Error -> false
      | _ -> true)
  | _ -> false

(** Computes the repetition factor of reference [refname]. *)
let repetition_factor alglib algname path refname =
  let (_,deflib,defname,_,_,_,_,_) = Algorithm.referencename_content alglib algname refname in
  let context = Algorithm.context_global () in
(*     ps (List.fold_left (fun s (sexpr,expr) -> s^","^sexpr) "" context); *)
  let factor dpds dir =
    try
      List.fold_left (fun maxvalue ((sref,sprt),(dref,dprt),dpd_class,_) -> common_factor alglib algname path ((sref,sprt),(dref,dprt)) maxvalue dir context) (Calculated (Repeat 1)) dpds
    with Failure _ -> Calculated Undefinied in
    
  let (_,_,_,args,ports,refs,dpds,_,_,_) = Algorithm.algorithmname_content alglib algname in
  let (dpds_in,dpds_out) = List.fold_left (fun (dpds_in,dpds_out) (((sref,_),(dref,_),dpd_class,_) as dpd) -> 
					     match dpd_class <> Precedence with
					     | true -> (match sref = refname with
							| true -> (dpds_in,dpd::dpds_out)
							| false -> match dref = refname with
							  | true -> (dpd::dpds_in,dpds_out)
							  | false -> dpds_in,dpds_out)
					     | false ->  dpds_in,dpds_out) ([],[]) dpds in
  let factor_in = factor dpds_in Port.In in
    match factor_in with
    | Calculated (Repeat i) -> (match i>1 with
      | true -> factor_in
      | false ->
	  factor dpds_out Port.Out)
    | Calculated _ -> factor_in
    | _ -> failwith "Algorithm_ctk.repetition factor error"
	
(** Updates the repetition factor of reference [refname] in algorithm [(alglib,algname)]. *)
let reference_factor_update alglib algname path top refname =
  match refname with
  | "" -> ()
  | _ ->
      let (_,_,_,_,_,_,old_rep,_) = Algorithm.referencename_content alglib algname refname in
      let new_rep = repetition_factor alglib algname path refname in
      let new_rep = match old_rep,new_rep with
	| (Specified n), (Calculated (Repeat 1)) | (Specified n), (Calculated Undefinied) -> old_rep
	| (Specified n),_ -> error_message top ("Warning: "^(string_of_path path)^"/"^refname^" formerly specified repetition factor was lost.");new_rep
	| _,_ -> new_rep in
	Algorithm.repetition_factor_modify alglib algname refname new_rep
	  
(** Sets the repetition factor of [refname] to [new_rep], only if possible. *)
let reference_factor_specify alglib algname refname new_rep top =
  let (_,_,_,_,_,_,old_rep,_) = Algorithm.referencename_content alglib algname refname in
    match new_rep with
    | "" -> (match old_rep with
      | Specified _ -> Algorithm.repetition_factor_modify alglib algname refname (Calculated (Repeat 1))
      | _ -> ())
    | n -> match old_rep with
  | Calculated (Repeat 1) | Calculated Undefinied | Specified _ -> Algorithm.repetition_factor_modify alglib algname refname (Specified (int_of_string new_rep))
  | _ -> error_message top "Repetition factor already implicitely calculated"

(** Updates the repetition factors of each reference in dependence with port [portname]. *)
let port_factor_update alglib algname path top portname =
  let modified_port port_ref =
    match port_ref with
    | Types.Port _ -> false
    | Types.Ref (r,p) -> (r.ref_algorithm.algo_library = alglib) && (r.ref_algorithm.algo_name = algname) && ((Port.port_name p) = portname) in
  let must_update dpd =
    (modified_port dpd.dpd_source) || (modified_port dpd.dpd_destination) in
    List.iter (fun alg -> List.iter (fun dpd -> match must_update dpd with
				     | true -> let ((sref,_),(dref,_),_,_) = Algorithm.dependence_content dpd in
					 reference_factor_update alg.algo_library alg.algo_name path top sref;
					 reference_factor_update alg.algo_library alg.algo_name path top dref
				     | false -> ()) alg.algo_dependences) Application.application.app_algorithms

(* ------------------------------------------------------------------------- *)
(*               GRAPHIC FUNCTIONS                                           *)
(* ------------------------------------------------------------------------- *)

(* -- Vertice port draw : used to draw a port of a vertex ----------------- *)
let vertex_port_draw cvs tag (pname,pdir,ptag) conditioning_port portclass (x1,y1) (x2,y2) dx =
  let m = y1+(y2-y1)/2 in
  let cx1,cx2,textx,anchor =
    match pdir with
    | Port.In -> (x1-dx-2,x1-dx,x1+border,NW)
    | Port.Out -> (x2+dx,x2+dx+2,x2-border,NE) in
  let idcnx = Canvas.create_rectangle cvs (Pixels cx1) (Pixels (m-3)) (Pixels cx2) (Pixels (m+3)) [Outline Black;FillColor Black; Tags [Tag tag; Tag ptag; Tag (ptag^"Cnx")]] in
  Canvas.bind cvs (Tag ptag) [[],Enter] (BindExtend ([], function _ ->
    Canvas.configure_rectangle cvs idcnx [Outline Red;FillColor Red]));
  Canvas.bind cvs (Tag ptag) [[],Leave] (BindExtend ([], function _ ->
    Canvas.configure_rectangle cvs idcnx [Outline Black;FillColor Black]));
  match portclass with
  | Port.Precedence_Port -> ()
  | _ ->
      let color = match conditioning_port with
      | true -> conditioningport_color
      | false -> match portclass with
	| Port.Init_Memory_Port -> initmemoryport_color
	| _ -> White in
      ignore(Canvas.create_rectangle cvs (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2) [FillColor color; Outline color; Tags [Tag tag; Tag ptag]]);
      ignore(Canvas.create_text cvs (Pixels textx) (Pixels y1) [Text pname; Tags [Tag tag; Tag ptag]; Anchor anchor])

(* -- Port Draw ------------------------------------------------------------ *)
let port_draw g conditioning_port portclass (name,tag,pos) =
  let cvs = Graph_ctk.canvas g in
  let (pname,pdir) = port_of_vport name in
  let pdir,gdir = match pdir with
  | Port.In -> Port.Out,Graph_ctk.POut
  | Port.Out -> Port.In,Graph_ctk.PIn in
  let p = (pname,pdir,Graph_ctk.tag_of_port g (name,VPort) ("p",gdir)) in
  let h = Font.metrics font Linespace in
  let w = get_text_width pname in
  let (box_x1,box_y1) = fst pos, (snd pos)+border in
  let (box_x2,box_y2) = (box_x1+w+2*border,box_y1+h+border) in
  ignore(Canvas.create_rectangle cvs (Pixels box_x1) (Pixels box_y1) (Pixels box_x2) (Pixels box_y2) [FillColor White; Tags [Tag tag]]);
  vertex_port_draw cvs tag p conditioning_port portclass (box_x1+1,box_y1+1) (box_x2-1,box_y2-1) 1

(* -- Reference Draw ------------------------------------------------------- *)
let rec reference_draw win g (name,tag,pos) =
  let cvs = Graph_ctk.canvas g in
  let (_,deflib,defname,argval,cond,_,rep,_) = Algorithm.referencename_content win.alg_library win.alg_name name in
  let (_,_,opntype,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content deflib defname in
  let is_memory = Algorithm.is_memory_reference win.alg_library win.alg_name name in 
  let pin,pout = List.partition (function (_,dir,_,_,_,_,_) -> dir=Port.In) ports in
  let path = win.alg_path @ [name] in
  let xsc = Application.xscname_of_ref path AttachRef in
  let context = Algorithm.context_global () in
(*   let dim = " (*"^string_of_int (Algorithm.repetition_factor win.alg_library win.alg_name name)^")" in *)
  let dim = match rep with
    | Calculated (Repeat 1) | Specified 1 -> ""
    | _ -> " (*"^(Algorithm.string_of_repetition rep)^")" in

  let ttl = name^dim in
  let atomic = Algorithm.atomic deflib defname in
  (* -- Find the items positions -- *)
  let h = Font.metrics font Linespace in
  let box_x1,box_y1 = pos in (* Top-left corner of sw *)
  
  let ttl_x,ttl_y = box_x1+border, box_y1+border in
  let xsc_x,xsc_y = ttl_x, ttl_y + h in
  let ttl_width = get_text_width ttl in
  let xsc_width = get_text_width xsc in
  let top_width = max ttl_width xsc_width in
  (* Dimension of the ports zone *)
  let ports_geom l = List.fold_left (function (lengthmax,heigthmax) -> function (pname,_,_,_,portclass,_,_) ->
    match portclass with
    | Port.Precedence_Port -> lengthmax,heigthmax
    | _ -> max lengthmax (get_text_width pname),heigthmax+h) (0,0) l in 
  let pin_width, pin_height = ports_geom pin in
  let pout_width, pout_height = ports_geom pout in
  let ports_width =  pin_width + pout_width + 2*border in
  let ports_height = max pin_height pout_height in
  (* Calcul of the bottom-right corner of the box *)
  let width = 2 * border + (max top_width ports_width) in
  let sep_y  = box_y1 + 2*h + 2*border in 
  let box_x2, box_y2 = box_x1 + width , sep_y + ports_height in
  (* Calcul fo the position of the vertical median line (between ports) *)
  let sep_x = match top_width>ports_width, width/2<pin_width+2*border, width/2<pout_width+2*border with
  | true, true, _ -> box_x1 + pin_width+2*border
  | true, _, true -> box_x2 - pout_width-2*border
  | true, false, false -> box_x1 + width/2
  | false, _, _ -> box_x1 + pin_width+2*border in
  (* Put the conditionning port in front of the input ports list *)
  let cond_pname,_ = Algorithm.cond_list deflib defname in

  (* -- Draw the items -- *)
  (* Title text *)
  let idttl = Canvas.create_text cvs (Pixels ttl_x) (Pixels ttl_y) [Font font;Text ttl; Anchor NW; Tags [Tag tag;Tag (tag^"TITLE")]] in
  let idxsc = Canvas.create_text cvs (Pixels xsc_x) (Pixels xsc_y) [Font font;Text xsc; Anchor NW; Tags [Tag tag;Tag (tag^"XSC")]] in
  (* Ports *)
  let dec_x = match atomic with | true  -> 0 | false -> border in
  ignore (List.fold_left (function y -> function (pname,dir,_,_,portclass,_,_) ->
    let y,ny = match portclass with
    | Port.Precedence_Port -> ttl_y,y
    | _ -> y,y+h in
    vertex_port_draw cvs tag (pname,dir,(Graph_ctk.tag_of_port g (name,VReference) (pname,Graph_ctk.PIn))) (pname=cond_pname) portclass (box_x1,y) (sep_x,y+h) 0;
    ny) sep_y pin);
  ignore (List.fold_left (function y -> function (pname,dir,_,_,portclass,_,_) ->
    let y,ny = match portclass with
    | Port.Precedence_Port -> ttl_y,y
    | _ -> y,y+h in
    vertex_port_draw cvs tag (pname,dir,(Graph_ctk.tag_of_port g (name,VReference) (pname,Graph_ctk.POut))) (pname=cond_pname) portclass (sep_x,y) (box_x2,y+h) dec_x;
    ny) sep_y pout);
  (* Title rectangle *)
  let color = color_of_algoclass opntype in
  let id=Canvas.create_rectangle cvs (Pixels (box_x1)) (Pixels (box_y1)) 
      (Pixels (box_x2)) (Pixels sep_y) [FillColor color; Tags [Tag tag;Tag (tag^"Highlight")]] in
  Canvas.lower_below cvs id (Tag tag);
  (* White ports rectangle *)
  let id = Canvas.create_rectangle cvs (Pixels (box_x1)) (Pixels (sep_y)) (Pixels (box_x2)) (Pixels (box_y2)) 
      [FillColor White; Tags [Tag tag;Tag (tag^"Highlight")]] in
  Canvas.lower_below cvs id (Tag tag);
  (* Drawing of the second rectangle for non-atomic references *)
  (match atomic with
  | false -> let id=Canvas.create_rectangle cvs (Pixels (box_x1+border)) (Pixels (box_y1+border)) 
	(Pixels (box_x2+border)) (Pixels (box_y2+border)) [FillColor White; Tags [Tag tag;Tag (tag^"Highlight")]] in
    Canvas.lower_below cvs id (Tag tag);
  | true -> ());
  (* Drawing of the vertical line separating In ports and Out ports *)
  ignore (Canvas.create_line cvs [Pixels sep_x;Pixels sep_y;Pixels sep_x;Pixels box_y2] [Tags [Tag tag]]);
  (* Drawing of an other rectangle (black border, transparent filling) around the ports *)
  ignore (Canvas.create_rectangle cvs (Pixels (box_x1)) (Pixels (sep_y)) 
	    (Pixels (box_x2)) (Pixels (box_y2)) [Tags [Tag tag;Tag (tag^"Highlight")]]);

  (* -- Bindings -- *)
  Canvas.bind cvs (Tag tag) [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> algo_open win.alg_top win.alg_bindcommand deflib defname path))

(* -- Dependence draw ------------------------------------------------------ *)
and dependence_draw g dash width (tag,stag,dtag) (p1,p2,dpdclass) =
  let cvs = Graph_ctk.canvas g in
  let x1,y1,x2,y2 = Canvas.bbox cvs [Tag (stag^"Cnx")] in
  let x1',y1',x2',y2' = Canvas.bbox cvs [Tag (dtag^"Cnx")] in
  ignore (Canvas.create_line cvs [(Pixels x2);(Pixels ((y1+y2)/2));(Pixels x1');(Pixels ((y1'+y2')/2))] [Dash dash; ArrowStyle Arrow_Last; Width (Pixels width); Tags [Tag tag; Tag (tag^"Highlight")]])

and dependence_draw_etype g etype =
  match etype with
  | Strong_Precedence_Data -> dependence_draw g "" 1
  | Weak_Precedence_Data -> dependence_draw g "-" 1
  | Data -> dependence_draw g "." 1
  | Precedence -> dependence_draw g "" 1

(* ------------------------------------------------------------------------- *)
(*              ITEM MANIPULATION FUNCTIONS                                  *)
(* ------------------------------------------------------------------------- *)
and algo_port_analyze path portstring action top =
  let rgxp = "\\(\\?\\|!\\|\\?!\\|!\\?\\)[ \n]+\\([a-zA-Z_][a-zA-Z0-9_*]*\\)[ \n]*\\(\\[\\([^]]+\\)\\]\\)?[ \n]+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
  let portlist,err = analyze portstring rgxp in
    (match err <> "" with
    | true -> error_message top err
    | false -> ());
  let rec f l = match l with
  | [io;ptypename;d;dim;pname]::t ->
      let dim = match dim with
      | "" -> Symbolic.Float 1.
      | _ -> parse_expression path dim in
      action io ptypename dim pname;
      f t
  | _ -> () in
  f portlist

and algo_reference_analyze path refstring action top =
  let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \n]*\\(<\\([^>]+\\)>\\)?[ \n]*\\(\\[\\([0-9]+\\)\\]\\)?" in
  let reflist,err = analyze refstring rgxp in
    (match err <> "" with
     | true -> error_message top err
     | false -> ());
  let rec f l = match l with
  | [name;a;args;r;rep]::t ->
      let args,err = args_values_of_string path args in
	(match err <> "" with
	 | true -> error_message top err
	 | false -> ());
	action name args rep;
	f t
  | _ -> () in
    f reflist
      
(* -- Delete/Add  Vertice/Edge --------------------------------------------- *)
(* Raise failure if src is not connectable to dst *)
and connect win g (src,dst,dpdclass) = 
  win.alg_top.tw_file_changed ();
  let srcname,srcport = port_of_portid src in
  let dstname,dstport = port_of_portid dst in
  let (v1_name,v1_type,p1_name),(v2_name,v2_type,p2_name) = src,dst in
  let win_dependence_add deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true -> Graph_ctk.edge_create_from_etype g (v1_name,p1_name,v2_name,p2_name,dpdclass)
  | false -> () in
  try
    (let context = Algorithm.context_global () in
       (* We don't check factors anymore as it was a source of errors in case of parameters *)
     let compatible_src = true (*
       match srcname <> "" && dpdclass <> Precedence with
      | true -> let (_,_,_,_,_,_,repetition) = Algorithm.referencename_content win.alg_library win.alg_name srcname in
	  compatible_factor win.alg_library win.alg_name win.alg_path ((srcname,srcport),(dstname,dstport)) repetition Port.Out context
      | false -> true *)in
     let compatible_dst = true (*
       match dstname <> "" && dpdclass <> Precedence with
       | true -> let (_,_,_,_,_,_,repetition) = Algorithm.referencename_content win.alg_library win.alg_name dstname in
	   compatible_factor win.alg_library win.alg_name win.alg_path ((srcname,srcport),(dstname,dstport)) repetition Port.In context
       | false -> true *)in
       match compatible_src && compatible_dst with
       | false -> failwith "Incompatible repetition factors"
       | true -> Algorithm.dependence_add win.alg_library win.alg_name ((srcname,srcport),(dstname,dstport),dpdclass,(Algorithm.condition_create (win.alg_variable,win.alg_current_value))) true;
	   reference_factor_update win.alg_library win.alg_name win.alg_path win.alg_top srcname;
	   reference_factor_update win.alg_library win.alg_name win.alg_path win.alg_top dstname;
	   List.iter (win_dependence_add win.alg_library win.alg_name) win.alg_top.tw_algos;
	   (* Redisplay in case repetition factor has changed *)
	   algo_display (win,g) (* I don't know why but refresh g won't work*))
  with Failure msg ->
    error_message win.alg_top msg;
    failwith msg
      
and algo_edge_delete win g ((v1_name,v1_type,p1_name) as src) ((v2_name,v2_type,p2_name) as dst) =
  win.alg_top.tw_file_changed ();
  let win_dependence_delete deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
    | true -> Graph_ctk.edge_remove_graph g v1_name p1_name v2_name p2_name
    | false -> () in
  let dref,dprt = port_of_portid dst in
  let sref,_ = port_of_portid src in
    Algorithm.dependence_delete win.alg_library win.alg_name dref dprt sref (win.alg_variable,win.alg_current_value);
    reference_factor_update win.alg_library win.alg_name win.alg_path win.alg_top sref;
    reference_factor_update win.alg_library win.alg_name win.alg_path win.alg_top dref;
    List.iter (win_dependence_delete win.alg_library win.alg_name) win.alg_top.tw_algos;
    (* Refresh in case repetition factor has changed *)
    Graph_ctk.refresh g (* I don't know why but algo_display won't work *)

and algo_edge_add win g etype =
  let spctags = [Graph_ctk.Zoomable] in
  let enter_f dpd = algo_info_change win (Dependence dpd) in
  let leave_f _ = algo_info_change win Canvas in
  let select_f _ = () in
  let delete (src,dst,_) = algo_edge_delete win g src dst in
  let draw = dependence_draw_etype g in
  let changetype (src,dst,_) dpdclass =
    win.alg_top.tw_file_changed ();
    let srcname,srcport = port_of_portid src in
    let dstname,dstport = port_of_portid dst in
    Algorithm.dependence_delete win.alg_library win.alg_name dstname dstport srcname (win.alg_variable,win.alg_current_value);
    Algorithm.dependence_add win.alg_library win.alg_name ((srcname,srcport),(dstname,dstport),dpdclass,(Algorithm.condition_create (win.alg_variable,win.alg_current_value))) false in 
  let infostr (_,_,dpdclass) = string_of_typedpd dpdclass in
  Some (spctags,draw,enter_f,leave_f,select_f,changetype,delete,infostr)

and reference_graph_refresh win g deflib defname =
  let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    List.iter (function (ref_name,ref_deflib,ref_defname,_,_,_,_,_) -> match ref_deflib=deflib && ref_defname=defname with
	       | true -> Graph_ctk.vertex_refresh_graph g (ref_name,VReference)
	       | false -> ()) refs
      
(* -- Function to add a port -- *)	
and graph_port_add (win,g) conditioning_port selected (pname,dir,_,_,_,_,pos) = 
  let pdir = match dir with
  | Port.In -> Graph_ctk.POut
  | Port.Out -> Graph_ctk.PIn in
  let ports = ["p",pdir,connect win g,[Strong_Precedence_Data;Weak_Precedence_Data;Data]] in
  let move_f pname (dx,dy) = match dx,dy with
  | 0,0 -> ()
  | _ ->
      win.alg_top.tw_file_changed ();
      let pname,dir = port_of_vport pname in
      Algorithm.port_move win.alg_library win.alg_name pname dir (dx,dy) in
  let delete_f = algo_port_delete win in
  let infostr_f _ = "" in
  let portclass name =
    let (pname,pdir) = port_of_vport name in
    let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    let (_,_,_,_,portclass,_,_) = List.find (function (n,d,_,_,_,_,_) -> n=pname && d=pdir) ports in
    portclass in
  let draw_f (name,tag,pos) = port_draw g conditioning_port (portclass name) (name,tag,pos) in
  let enter_f (pname,_) = algo_info_change win (Port (port_of_vport pname)) in
  let leave_f _ = algo_info_change win Canvas in
  let select_f _ = () in
  let refresh_ports_edges_f (name,_) =
    let (_,_,_,_,_,_,dpds,_,_,_) = Algorithm.algorithm_cond_content win.alg_library win.alg_name (win.alg_variable, win.alg_current_value) in
    let dpds = List.filter (function ((v1_name,p1_name),(v2_name,p2_name),_,_) ->
      let pname,_ = port_of_vport name in
      (v1_name="" && p1_name=pname) || (v2_name="" && p2_name=pname)) dpds in
    let dpds = List.map (function (s,d,etype,_) -> edge_of_dpd (s,d,etype)) dpds in
    ports,dpds in
  let name = vport_of_port (pname,dir) in
  let pos = Coord.pos_of_coord2d pos in
  Graph_ctk.vertex_create g name VPort pos selected [Graph_ctk.Movable;Graph_ctk.Zoomable] ports draw_f enter_f leave_f select_f move_f delete_f infostr_f refresh_ports_edges_f

(* Raise Failure if a port with the same name as p already exists *)
and algo_port_add (win,g) p selected =
  let win_port_add deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true -> graph_port_add (win,g) false selected p
  | false -> reference_graph_refresh win g deflib defname in
  try
    Algorithm.port_add win.alg_library win.alg_name p;
    List.iter (win_port_add win.alg_library win.alg_name) win.alg_top.tw_algos
  with Failure msg -> error_message win.alg_top msg

and algo_port_add_io (win,g) selected io ptypename pdim pname =
  let cvs = Graph_ctk.canvas g in
  let port_add ptypename pdim pname pdir =
    match algo_can_have_port win.alg_library win.alg_name pdir with
    | true ->
	(* Calculates the height between the tops of two ports *)
	let h = 2*border + port_space + Font.metrics font Linespace in
        (* Find the position of every port in the same dir than pdir *)
	let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
	let ports = List.filter (function (_,dir,_,_,portclass,_,pos) -> (portclass<>Port.Precedence_Port) && (dir=pdir) && (pos<>Coord.No_coord2d)) ports in
        (* Calculates the initialisation value *)
	let nx,ny = match ports,pdir with
	| [],Port.In -> Graph_ctk.conv_window_canvas cvs port_space port_space (* No input ports -> top-left corner of the canvas *)
	| [],Port.Out -> Graph_ctk.conv_window_canvas cvs ((fst win.alg_dimension)-30) port_space (* No output ports -> top-right corner of canvas *)
	| ((_,_,_,_,_,_,poshd)::_),_ -> List.fold_left (function (mx,my) -> function (_,_,_,_,_,_,pos) ->
	    let x,y = Coord.pos_of_coord2d pos in
	    (((match pdir with | Port.In -> min | Port.Out -> max) mx x),(max my (y+h)))) (Coord.pos_of_coord2d poshd) ports in
	let pos = Coord.Coord2d (nx,ny) in
	algo_port_add (win,g) (pname,pdir,ptypename,pdim,Port.Data_Port,0,pos) selected;
	win.alg_top.tw_file_changed ()
    | false -> () in
  match io with
  | "?" -> port_add ptypename pdim pname Port.In
  | "!" -> port_add ptypename pdim pname Port.Out
  | _ ->
      port_add ptypename pdim pname Port.In;
      port_add ptypename pdim pname Port.Out
	
and algo_port_add_ask (win,g) =
  let cvs = Graph_ctk.canvas g in
  let help = "syntax: port_direction data_type[port_size] port_name\n '[port_size]' is optionnal\n example: To add an integer input port: ? int i\nTo add an array of real output ports: ! float[2] o\nTo add an integer input/output port: ?! int io\n To parametrize a port size: ! float[N] o or ! float[|List|] o\nYou may add several ports: ? int i1 ? int i2 ! int o" in
  let f ports =
    Graph_ctk.unselect_all cvs;
    algo_port_analyze win.alg_path ports (algo_port_add_io (win,g) true) win.alg_top in
  ask_for "" help "Enter the direction, type and name of the new ports :" f win.alg_widget win.alg_top (WAsk_port_add win.alg_path)

and algo_port_delete win name =
  win.alg_top.tw_file_changed ();
  let win_port_delete deflib defname pname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true -> Graph_ctk.vertex_remove_graph g (name,VPort)
  | false -> reference_graph_refresh win g deflib defname in
  let pname,_ = port_of_vport name in
  Algorithm.port_delete win.alg_library win.alg_name pname;
  List.iter (win_port_delete win.alg_library win.alg_name pname) win.alg_top.tw_algos

(* -- To add a reference -- *)
and algo_reference_delete win refname =
  win.alg_top.tw_file_changed ();
  let win_reference_delete deflib defname (w,g) = match w<>win && w.alg_library=deflib && w.alg_name=defname with
  | true -> Graph_ctk.vertex_remove_graph g (refname,VReference)
  | false -> () in
  Algorithm.ref_delete win.alg_library win.alg_name refname;
  List.iter (win_reference_delete win.alg_library win.alg_name) win.alg_top.tw_algos

and graph_reference_add (win,g) (refname,deflib,defname,_,pos) selected =
  let (_,_,algoclass,_,_,_,_,_,_,_) = Algorithm.algorithmname_content deflib defname in
  let ports deflib defname =
    let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content deflib defname in
    List.map (function (name,dir,_,_,portclass,_,_) ->
      let authorized_edges = match portclass with
      |	Port.Precedence_Port -> [Precedence]
      |	_ -> [Strong_Precedence_Data;Weak_Precedence_Data;Data] in
      let pdir = match dir with
      | Port.In -> Graph_ctk.PIn
      | Port.Out -> Graph_ctk.POut in (name,pdir,connect win g,authorized_edges)) ports in
  let draw_f = reference_draw win g in
  let enter_f (refname,_) = algo_info_change win (Reference refname) in
  let leave_f _ = algo_info_change win Canvas in
  let select_f _ = algo_menus_update (win,g) in
  let move_f refname (dx,dy) = match dx,dy with
  | 0,0 -> ()
  | _ ->
      win.alg_top.tw_file_changed ();
      Algorithm.ref_move win.alg_library win.alg_name refname (dx,dy) in
  let delete_f refname = algo_reference_delete win refname in
  let dlib = match deflib with "" -> "" | _ -> deflib^"/" in
  let refresh_ports_edges_f (refname,_) =
    let ports = ports deflib defname
    and (_,_,_,_,_,_,dpds,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    let dpds = List.filter (function ((v1_name,p1_name),(v2_name,p2_name),_,_) -> v1_name=refname || v2_name=refname) dpds in
    let dpds = List.map (function (s,d,etype,_) -> edge_of_dpd (s,d,etype)) dpds in
    ports,dpds in
  let infostr_f _ = "Definition :\n "^(string_of_algoclass algoclass)^" "^dlib^defname in
  let ports = ports deflib defname in
  let pos = Coord.pos_of_coord2d pos in
  Graph_ctk.vertex_create g refname VReference pos selected [Graph_ctk.Movable;Graph_ctk.Zoomable] ports draw_f enter_f leave_f select_f move_f delete_f infostr_f refresh_ports_edges_f

and algo_reference_add (win,g) (ref_name,ref_deflib,ref_defname,args,pos,rep) selected =
  win.alg_top.tw_file_changed ();
  let win_reference_add deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true -> graph_reference_add (win,g) (ref_name,ref_deflib,ref_defname,args,pos) selected
  | false -> () in
  try
    Algorithm.reference_add win.alg_library win.alg_name (ref_name,ref_deflib,ref_defname,args,(Algorithm.condition_create (win.alg_variable,win.alg_current_value)),pos,rep,"");
    List.iter (win_reference_add win.alg_library win.alg_name) win.alg_top.tw_algos
  with Failure msg -> error_message win.alg_top msg

and algo_reference_add_ask (win,g) =
  let cvs = Graph_ctk.canvas g in
  let f deflib defname = 
    let help = "syntax: reference_name <parameters_values> [repetition_factor].\n '<parameters_values>' and '[repetition_factor]' are optionnal\n example: opn <{1,2};1> [3]" in
    let ask_for_fun names = 
      Graph_ctk.unselect_all cvs;
      let number = ref 0 in
      let ref_add_name name args rep = 
	number := !number + 1;
	let rep = match rep with
	  | "" -> Calculated (Repeat 1)
	  | n -> Specified (int_of_string n) in
	let (x,y) = Graph_ctk.conv_window_canvas cvs (!number*20) (!number*20) in
	let pos = Coord.Coord2d (x,y) in
	  algo_reference_add (win,g) (name,deflib,defname,args,pos,rep) true in
	algo_reference_analyze win.alg_path names ref_add_name win.alg_top in
    ask_for "" help "Names, parameters and repetitions of the new references : " ask_for_fun win.alg_widget win.alg_top (WAsk_definition_reference_name win.alg_path) in
  let algolist = Algorithm.algo_list () in
  let algolistadd = List.filter (function l,n -> l<>win.alg_library || n<>win.alg_name) algolist in
  let title = "Definition "^win.alg_name^",\nChoose definition to reference :" in
   choose_libs_defs (function (deflib,defname) -> f deflib defname) algolistadd title win.alg_widget win.alg_top (WAsk_definition_reference_add win.alg_path)

(* -- To add a  dependence to the graph -- *)
and graph_dependence_add (win,g) dpd =
  let v1_name,p1_name,v2_name,p2_name,etype = edge_of_dpd dpd in
  Graph_ctk.edge_create_from_etype g (v1_name,p1_name,v2_name,p2_name,etype)

(* -- Ports and references "modify" functions ------------------------------ *)
and graph_reference_rename win name newname =
  let win_reference_rename deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true ->
      Graph_ctk.rename_vertex g name newname;
      Graph_ctk.vertex_refresh_graph g (newname,VReference)
  | false -> () in
  List.iter (win_reference_rename win.alg_library win.alg_name) win.alg_top.tw_algos

and algo_definition_modify win =
  let ask_for_fun newname =
    match win.alg_library with
    | "" ->
    (try
      let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \n]*\\(<\\([^>]+\\)>\\)?[ \n]*\\(\\[\\([a-zA-Z0-9\\+-\\*/]+\\)[..]?[ \n]*\\([a-zA-Z0-9\\+-\\*/]+\\)?\\]\\)?" in
      let namelist,err = analyze newname rgxp in
	(match err <> "" with
	 | true -> error_message win.alg_top err
	 | false -> ());
	let newname = match namelist with
	| [] -> failwith "New name incorrect : ignoring."
	| [name;a;args;mmr;mmra;mmrb]::_ -> name 
	| _ -> failwith "Error : Algorithm_ctk.algo_definition_modif" in
	match newname = win.alg_name with
	| true -> ()
	| false ->
	    Algorithm.definition_modify_name win.alg_library win.alg_name newname;
	    let reference_modify (w,g) =
	      win.alg_top.tw_file_changed ();
	      match w.alg_library=win.alg_library && w.alg_name=win.alg_name with
	      | true -> algo_close win.alg_top w.alg_path
	      | false -> algo_display (w,g) in
	      List.iter reference_modify win.alg_top.tw_algos;
	      algo_open win.alg_top win.alg_bindcommand win.alg_library newname win.alg_path
	with Failure msg -> error_message win.alg_top msg)
    | _ -> error_message win.alg_top "Can't modify a library operation." in
    ask_for (string_of_ref_type win.alg_library win.alg_name) "" "New name of the definition:" ask_for_fun win.alg_widget win.alg_top (WAsk_definition_modify win.alg_path)
      
and algo_reference_modify (win,g) rname =
  let (_,_,_,args,_,_,rep,_) = Algorithm.referencename_content win.alg_library win.alg_name rname in
  let rep = match rep with
    | Specified _ -> "["^(Algorithm.string_of_repetition rep)^"]"
    | _ -> "" in
  let current = rname^(string_of_argsvalues args)^" "^rep in
  let ask_for_fun s =
    try
      let ref_modify name args rep =
	win.alg_top.tw_file_changed ();
	Algorithm.reference_modify win.alg_library win.alg_name rname name args;
	(* Repetition factor may have changed due to parametrized port sizes *)
	let (_,deflib,defname,_,_,_,_,_) = Algorithm.referencename_content win.alg_library win.alg_name name in
	let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content deflib defname in
	  graph_reference_rename win rname name;
	  List.iter (fun (pname,_,_,_,_,_,_) -> port_factor_update deflib defname win.alg_path win.alg_top pname) ports;
	  List.iter (fun (w,g) -> let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content w.alg_library w.alg_name in
			 List.iter (fun (refname,_,_,_,_,_,_,_) -> reference_factor_update w.alg_library w.alg_name w.alg_path win.alg_top refname) refs;
		       Graph_ctk.refresh g) win.alg_top.tw_algos;
	  reference_factor_specify win.alg_library win.alg_name name rep win.alg_top;
	  algo_display (win,g) in
	algo_reference_analyze win.alg_path s ref_modify win.alg_top
    with Failure msg -> error_message win.alg_top msg in
    ask_for current "syntax: reference_name <parameters_values> [repetition_factor].\n '<parameters_values>' and '[repetition_factor]' are optionnal\n example: opn <{1,2};1> [3]" "Name, parameters, repetition : " ask_for_fun win.alg_widget win.alg_top (WAsk_definition_reference_modify (win.alg_path,rname))
      
and graph_port_refresh win portname portdir f =
  let win_port_refresh deflib defname (win,g) = match win.alg_library=deflib && win.alg_name=defname with
  | true ->
      f g portname portdir;
      Graph_ctk.vertex_refresh_graph g ((vport_of_port (portname,portdir)),VPort)
  | false -> reference_graph_refresh win g deflib defname in
  List.iter (win_port_refresh win.alg_library win.alg_name) win.alg_top.tw_algos

and connected_references_refresh win newname =
  let win_references_refresh deflib defname (win,g) =
    match win.alg_library=deflib && win.alg_name=defname with
    | true -> () 
    | false ->
	let modified_port (refname,prtname) =
	  match refname with
	  | "" -> false
	  | _ -> let (_,dlib,dname,_,_,_,_,_) = Algorithm.referencename_content win.alg_library win.alg_name refname in
	      (dlib = deflib) && (dname = defname) && (prtname = newname) in
	let must_update ((sref,sprt),(dref,dname),_,_) = (modified_port (sref,sprt)) || (modified_port (dref,dname)) in
	  
	let (_,_,_,_,_,_,dpds,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
	  List.iter (fun  (((sref,_),(dref,_),_,_) as dpd) -> match must_update dpd with
		     | true -> Graph_ctk.vertex_refresh_graph g (sref,VReference);
			 Graph_ctk.vertex_refresh_graph g (dref,VReference)
		     | false -> ()) dpds in
    List.iter (win_references_refresh win.alg_library win.alg_name) win.alg_top.tw_algos
  

and graph_port_rename win pname dir newname =
  let f g newname pdir = Graph_ctk.rename_vertex g (vport_of_port (pname,pdir)) (vport_of_port (newname,pdir)) in
  graph_port_refresh win newname dir f;
    connected_references_refresh win newname

and algo_port_modify (win,g) (pname,pdir) = 
  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let _,_,tn,td,_,_,_ = List.find (function (n,d,_,_,_,_,_) -> n=pname && d=pdir) ports in
  let dir = match pdir with
  | Port.In -> "? "
  | Port.Out -> "! " in
  let current = dir^tn^"["^(Symbolic.string_of_expression td)^"] "^pname in

  let help = "syntax: port_direction data_type[port_size] port_name\n '[port_size]' " ^
	     "is optionnal\n example: To add an integer input port: ? int i\n" ^
	     "To add an array of real output ports: ! float[2] o\nTo add an integer input/output port: ?! int io\n" ^
	     "To parametrize a port size: ! float[N] o or ! float[|List|] o\n" ^
	     "You may add several ports: ? int i1 ? int i2 ! int o" in
  let port_modify alglib algname pname dir pnewname ptype pdim order =
    Algorithm.port_modify alglib algname pname dir pnewname ptype pdim order;
    (* We must update repetition factors if some dependences are connected to this port *)
    port_factor_update alglib algname win.alg_path win.alg_top pnewname in
  let ask_for_fun s =
    try
      let port_modify io newtypename newdim newname =
	let ports = List.filter (function n,_,_,_,_,_,_ -> n=pname) ports in
	  match ports with
	  | (_,_,_,_,_,r1,_)::(_,_,_,_,_,r2,_)::_ ->
	      win.alg_top.tw_file_changed ();
	      port_modify win.alg_library win.alg_name pname Port.In newname newtypename newdim r1;
	      port_modify win.alg_library win.alg_name pname Port.Out newname newtypename newdim r2;
	      graph_port_rename win pname Port.In newname;
	      graph_port_rename win pname Port.Out newname
	  | (_,_,_,_,_,r,_)::_ -> (match io with
				   | "?" when pdir=Port.In ->
				       win.alg_top.tw_file_changed ();
				       port_modify win.alg_library win.alg_name pname Port.In newname newtypename newdim r;
				       graph_port_rename win pname Port.In newname
				   | "!" when pdir=Port.Out ->
				       win.alg_top.tw_file_changed ();
				       port_modify win.alg_library win.alg_name pname Port.Out newname newtypename newdim r;
				       graph_port_rename win pname Port.Out newname
				   | "?!" | "!?" -> (match pdir with
						     | Port.In ->
							 win.alg_top.tw_file_changed ();
							 port_modify win.alg_library win.alg_name pname Port.In newname newtypename newdim r;
							 graph_port_rename win pname Port.In newname;
							 algo_port_add_io (win,g) false "!" newtypename newdim newname
						     | Port.Out ->
							 win.alg_top.tw_file_changed ();
							 port_modify win.alg_library win.alg_name pname Port.Out newname newtypename newdim r;
							 graph_port_rename win pname Port.Out newname;
							 algo_port_add_io (win,g) false "?" newtypename newdim newname)
				   | _ ->
				       algo_port_delete win pname;
				       algo_port_add_io (win,g) false io newtypename newdim newname)
	  | _ -> () in
	algo_port_analyze win.alg_path s port_modify win.alg_top
    with Failure msg -> error_message win.alg_top msg in
    ask_for current help "Enter the definition :" ask_for_fun win.alg_widget win.alg_top (WAsk_port_modify (win.alg_path,(pname,pdir)))

and algo_port_init (win,g) (pname,pdir) = 
  try
    let (_,_,algoclass,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    win.alg_top.tw_file_changed ();
    let _,_,_,_,portclass,_,_ = List.find (function (n,d,_,_,_,_,_) -> n=pname && d=pdir) ports in
    let portclass = match portclass with
    | Port.Data_Port -> Port.Init_Memory_Port
    | Port.Init_Memory_Port -> Port.Data_Port
    | _ -> raise (Failure "A precedence port cannot be an init port of a memory") in
    let initportold = match List.filter (function (_,_,_,_,portclass,_,_) -> portclass=Port.Init_Memory_Port) ports with
    | [] -> None
    | (n,d,_,_,_,_,_)::_ -> Some (n,d) in
    Algorithm.port_modify_class win.alg_library win.alg_name pname pdir portclass;
    (match initportold with
    | Some (pname,pdir) -> graph_port_refresh win pname pdir (fun _ _ _ -> ())
    | None -> ());
    graph_port_refresh win pname pdir (fun _ _ _ -> ());
  with Failure msg -> error_message win.alg_top msg

and algo_selection_modify (win,g) =
  let (verticesid,_) = Graph_ctk.selected g in
  List.iter (function (vname,vtype) -> 
    match vtype with
    | VReference -> algo_reference_modify (win,g) vname
    | VPort -> algo_port_modify (win,g) (port_of_vport vname)) verticesid

and reference_description_edit (win,g) =
  let help = "syntax:  any string accepted, except quotes" in
  let title = "Enter the description of this reference:" in
  let f new_description =
    let (verticesid,_) = Graph_ctk.selected g in
      match String.contains new_description '"' with
      | true -> error_message win.alg_top "Syntax error; new description ignored"
      | false -> win.alg_top.tw_file_changed ();
	  List.iter (function (vname,_) ->
		       Algorithm.reference_modify_description win.alg_library win.alg_name vname new_description) verticesid in
    ask_for "" help title f win.alg_widget win.alg_top (WAsk_reference_description_modify win.alg_path)
	
and algo_selection_port_init (win,g) =
  let (verticesid,_) = Graph_ctk.selected g in
  List.iter (function (vname,vtype) -> 
    match vtype with
    | VReference -> raise (Failure "Not on a reference !")
    | VPort -> algo_port_init (win,g) (port_of_vport vname)) verticesid

(* -- Algo selection functions ----------------------------------------------- *)
and algo_port_select (win,g) =
  let select_f portname =
    Graph_ctk.select_vertex g (portname,VPort) in
  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let ports = List.filter (fun (_,_,_,_,prt_class,_,_) -> prt_class <> Port.Precedence_Port) ports in
  let ports_names = List.map (fun (prt_name,prt_dir,_,_,_,_,_) -> ((string_of_direction prt_dir),prt_name)) ports in
  let title = "Select port to find:" in
    choose_libs_defs (function (pdir,portname) -> select_f ((vport_of_port (portname,(direction_of_string pdir))))) ports_names title win.alg_widget win.alg_top (WSelect_port win.alg_path)

and algo_reference_select (win,g) =  
  let select_f refname =
    Graph_ctk.select_vertex g (refname,VReference) in
  let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let refs_names = List.map (fun (ref_name,_,_,_,_,_,_,_) -> ("references",ref_name)) refs in
  let title = "Select reference to find:" in
    choose_libs_defs (function (_,refname) -> select_f refname) refs_names title win.alg_widget win.alg_top (WSelect_reference win.alg_path)

(* -- Algo display functions ----------------------------------------------- *)
and algo_display (win,g) =
  Graph_ctk.reset g;
  let cvs = Graph_ctk.canvas g in
  (match win.alg_variable with
  | "" -> ()
  | _ ->
      List.iter (function _,b -> Button.configure b [Relief Raised]) win.alg_buttons;
      Button.configure (List.assoc win.alg_current_value win.alg_buttons) [Relief Ridge]);
  let (_,_,_,_,ports,refs,dpds,dim,_,_) = Algorithm.algorithm_cond_content win.alg_library win.alg_name (win.alg_variable,win.alg_current_value) in

  (* -- Add the ports to the graph -- *)
  let cond_pname,_ = Algorithm.cond_list win.alg_library win.alg_name in
  let non_precedence_ports = List.filter (function (_,_,_,_,portclass,_,_) -> portclass <> Port.Precedence_Port) ports in
  List.iter (function (pname,_,_,_,_,_,_) as p -> graph_port_add (win,g) (pname=cond_pname) false p) non_precedence_ports;
  (* -- Add the references to the graph -- *)
  List.iter (function (name,deflib,defname,args,_,pos,_,_) -> graph_reference_add (win,g) (name,deflib,defname,args,pos) false) refs;
  (* -- Add the dependences to the graph -- *)
  List.iter (function (s,d,etype,_) -> graph_dependence_add (win,g) (s,d,etype)) dpds;
  let x0,y0,_,_ = match Canvas.find cvs [All] with
  | [] -> 0,0,0,0
  | l -> Canvas.bbox cvs l in
  Canvas.configure cvs [ScrollRegion ((Pixels x0),(Pixels y0),(Pixels (Winfo.width cvs)),(Pixels (Winfo.height cvs)))];
  (match dim with
  | Coord.No_coord2d -> ()
  | Coord.Coord2d (x,y) -> win.alg_dimension <- x,y);
  Canvas.configure cvs [Width (Pixels (fst win.alg_dimension-2)); Height (Pixels (snd win.alg_dimension-2))];
  update ();
  Graph_ctk.configure_resize g (function (a,b) ->
    win.alg_top.tw_file_changed ();
    win.alg_dimension <- (a,b);
    Algorithm.algo_dimension_window_change win.alg_library win.alg_name a b);
  bind cvs [[], Destroy] (BindSet ([],function _ -> algo_close win.alg_top win.alg_path))

(* -- Copy and Paste functions --------------------------------------------- *)
and copy win (verticesid,_) =
  (* Split ports and references from verticesid *)
  let referencesid,portsid = List.partition (function (_,vtype) -> vtype = VReference) verticesid in
  let refnames = List.map fst referencesid in
  let portnames = List.map (function p -> port_of_vport (fst p)) portsid in
  (* Function to extract reference characteristics from Algorithm *)
  let copyref rname = 
    let (name,deflib,defname,argval,cond,pos,rep,description) = Algorithm.referencename_content win.alg_library win.alg_name rname in 
    (name,deflib,defname,argval,pos,rep) in
  (* Function to extract port characteristics from Algorithm *)
  let copyport (pname,pdir) =
    let (_,_,typename,typedim,_,_,pos) = Algorithm.portname_content win.alg_library win.alg_name pname pdir in 
    (pname,pdir,typename,typedim,pos) in
  (* Copy the data to the algorithm clipboard *)
  win.alg_top.tw_alg_clip <- ((List.map copyref refnames),(List.map copyport portnames))

and vertex_paste win g (vold,vtype) vnew =
  let (references,ports) = win.alg_top.tw_alg_clip in
  match vtype with
  | VPort -> 
      let pold,_ = port_of_vport vold in
      let pnew,_ = port_of_vport vnew in
      let (_,pdir,typename,typedim,pos) = List.find (function (name,_,_,_,_) -> name=pold) ports in
      algo_port_add (win,g) (pnew,pdir,typename,typedim,Port.Data_Port,0,pos) true
  | VReference -> 
      let (_,deflib,defname,args,pos,rep) = List.find (function (name,_,_,_,_,_) -> name=vold) references in
      algo_reference_add (win,g) (vnew,deflib,defname,args,pos,rep) true

and edge_paste win g dpd = 
  let ((v1name,v1type,v1port),(v2name,v2type,v2port),etype) = dpd in
  connect win g dpd; 
  graph_dependence_add (win,g) ((v1name,v1port),(v2name,v2port),etype)

(* -- Conditioning functions ----------------------------------------------- *)
and algo_button_cond_create (win,g) v x =
  let btn = Button.create win.alg_bar_cond [Text (v^" = "^(string_of_int x)); Relief Raised] in
  Button.configure btn [Command (function () -> win.alg_current_value <- x; algo_display (win,g))];
  win.alg_buttons <- win.alg_buttons @ [x,btn];
  pack [btn] [Side Side_Left]

and algo_display_var (win,g) =
  match Algorithm.cond_list win.alg_library win.alg_name with
  | "",_ -> algo_display (win,g)
  | v, l -> 
      win.alg_variable <- v;
      win.alg_values <- l;
      if l<>[] then win.alg_current_value <- List.hd l;
      List.iter (function x -> algo_button_cond_create (win,g) v x) l;
      algo_display (win,g)

and algo_condition_add (win,g) =
  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let port_conditionnable pname =
    (* Does the port labelled "pname" exists, is an input port, and is an integer port ? *)
    List.exists (function (pn,pd,ptn,_,_,_,_) -> pn=pname && pd=Port.In && ptn="int") ports in
  let ask_for_fun s =
    let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \n]*=[ \n]*\\([0-9]+\\)" in
    let conditions,err = analyze s rgxp in
    (match err <> "" with
    | true -> error_message win.alg_top err
    | false -> ());
    let rec f l = match l with
    | [vr;vl]::t -> let v = int_of_string vl in 
      (* There can be only one conditionning variable *)
      (match win.alg_variable with
      |	"" ->
	  (match (port_conditionnable vr) with
	  | true ->
	      win.alg_top.tw_file_changed ();
	      win.alg_variable <- vr;
	      algo_button_cond_create (win,g) vr v;
	      win.alg_values <- win.alg_values @ [v];
	      win.alg_current_value <- v;
	      Algorithm.condition win.alg_library win.alg_name (vr,v);
	  | false -> error_message win.alg_top ("Error in expression \""^vr^"="^vl^"\".\n"^vr^" is not the name of a valid integer input port."))
      |	variable when variable=vr ->
	  algo_button_cond_create (win,g) vr v;
	  win.alg_values <- win.alg_values @ [v];
	  win.alg_current_value <- v;
      |	_ -> error_message win.alg_top ("Error in expression \""^vr^"="^vl^"\".\nYou can have only one conditionning variable, "^win.alg_variable^"."));
      f t
    | _ -> () in
    f conditions;
    algo_menus_update (win,g) in
  ask_for "" "syntax: input_port_name_1=condition_value_1 input_port_name_2=condition_value_2 ...\n example: x=0 x=1\n note : 'x' must be the name of an integer input port" "Enter the new condition :" ask_for_fun win.alg_widget win.alg_top (WAsk_condition_add win.alg_path)

and algo_update_main top =
  List.iter (function (w,g) -> algo_update_path w;
	       algo_set_title w.alg_widget w.alg_library w.alg_name w.alg_path;
	       algo_menus_update (w,g);
	       let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content w.alg_library w.alg_name in
		 List.iter (fun (refname,_,_,_,_,_,_,_) -> reference_factor_update w.alg_library w.alg_name w.alg_path top refname) refs;
		 Graph_ctk.refresh g) top.tw_algos

and xsc_define top =
  let ask_for_fun xscs =
    let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)" in
    let xscnames_list,err = analyze xscs rgxp in
    (match err <> "" with
    | true -> error_message top err
    | false -> ());
    (match xscnames_list with
    | [] -> ()
    | _ ->
	top.tw_file_changed ();
	List.iter Application.xscname_define (List.concat xscnames_list));
(*
    let rec f l = match l with
   | [xsc]::t -> Application.xscname_define xsc; f t
   | _ -> () in
   f xscnames_list;*)
    List.iter (function a -> algo_menus_update a) top.tw_algos in
  ask_for "" "syntax: software_component_1 software_component_2 ..." "Enter the names of the new Software Components" ask_for_fun Widget.default_toplevel top WAsk_create_xsc

and xsc_delete top xsc =
  Application.xscname_delete xsc;
  top.tw_file_changed ();
  List.iter (function w -> algo_menus_update w; algo_display w) top.tw_algos

and xsc_attach attach_type (win,g) xsc refname =
  win.alg_top.tw_file_changed ();
  (match xsc with
  | "" -> Application.remove_ref_xsc (win.alg_path@[refname]) attach_type;
  | _ -> Application.ref_modify_xsc (win.alg_path@[refname]) attach_type xsc);
  algo_display (win,g)

and algo_change_xsc attach_type (win,g) xsc  =
  let (verticesid,_) = Graph_ctk.selected g in
  let referencesid = List.filter (function (_,vtype) -> vtype = VReference) verticesid in
  List.iter (function (rname,_) -> xsc_attach attach_type (win,g) xsc rname) referencesid

and algo_main_set win =
  let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithm_cond_content win.alg_library win.alg_name (win.alg_variable,win.alg_current_value) in
  match List.for_all (function (_,_,_,_,portclass,_,_) -> portclass = Port.Precedence_Port) ports with
  | true ->
      win.alg_top.tw_file_changed ();
      Algorithm.algo_main_set (win.alg_library,win.alg_name) [];
      algo_update_main win.alg_top;
  | false -> 
      error_message win.alg_top "Only a definition without ports can be set as the main definition."

and algo_condition_delete (win,g) =
  (* If we are deleting the last condition, set win.alg_variable as "" *)
  win.alg_values <- List.filter ((<>) win.alg_current_value) win.alg_values;
  let but = List.assoc win.alg_current_value win.alg_buttons in
  win.alg_buttons <- List.filter (function _,b -> b<>but) win.alg_buttons;
  Tk.destroy but;
  (match win.alg_values with
  | [] ->
      win.alg_top.tw_file_changed ();
      win.alg_variable <- "";
      win.alg_current_value <- 1;
      Algorithm.condition win.alg_library win.alg_name ("",1);
  | _ ->
      win.alg_top.tw_file_changed ();
      Algorithm.condition_delete win.alg_library win.alg_name (win.alg_variable,win.alg_current_value);
      Graph_ctk.reset g;
      win.alg_current_value <- List.hd win.alg_values);
  algo_menus_update (win,g)

and algo_durations_edit win =
  win.alg_top.tw_file_changed ();
  let algolib,algoname = win.alg_library,win.alg_name in
  let opr_defs = Architecture.operatortypes_list () in
  let drs = List.concat (List.fold_left (function drs -> function oprlib,oprname ->
    let _,_,_,all_drs,_,_ = Architecture.operator_definitionname_content oprlib oprname in
    let drs_libname = List.filter (function (l,n),_ -> l=algolib && n=algoname) all_drs in
    let drs_libname = List.map (function _,d -> (oprlib,oprname),d) drs_libname in
    drs_libname::drs) [] opr_defs) in
  match edit win.alg_widget (string_of_durations drs) "syntax: operator_definition_name = duration\n example: c40 = 2\n u/U = 1" ("Timings of "^(string_of_ref_type algolib algoname)^" :") with
  | Some durations ->
      let drs,err = durations_analyze durations in
	(match err <> "" with
	 | true -> error_message win.alg_top err
	 | false -> ());
	(* Really not optimal. Improve this if there are performances problems *)
	List.iter (fun ((olib,oname),dr) -> 
		     try let _,_,_,all_drs,_,_ = Architecture.operator_definitionname_content olib oname in
		     let drs_others = List.filter (function (l,n),_ -> l<>algolib || n<>algoname) all_drs in
		       Architecture.operator_durations_save olib oname (((algolib,algoname),dr)::drs_others)
		     with _ -> error_message win.alg_top ("Operator definition "^(string_of_ref_type olib oname)
							  ^" doesn't exist.")
		  )  drs
  | None -> ()

and algo_ports_order_edit win g =
  let algolib,algoname = win.alg_library,win.alg_name in
  Algorithm.ports_order_sort_libname algolib algoname;

  let window = Toplevel.create Widget.default_toplevel [] in
  Wm.title_set window ("Ports Order : "^(string_of_ref_type algolib algoname));
  let function_name = string_of_ref_type algolib algoname in
  let function_label = Label.create window [] in

  let args_frame = Frame.create window [BorderWidth (Pixels 5)] in
  let args_in_frame = Frame.create args_frame [BorderWidth (Pixels 5)] in
  let args_out_frame = Frame.create args_frame [BorderWidth (Pixels 5)] in
  let args_bn_frame = Frame.create args_frame [BorderWidth (Pixels 5)] in

  let args_in_lb = Listbox.create args_in_frame [SelectMode Extended] in
  let args_in_sb = Scrollbar.create args_in_frame [Orient Vertical] in
  Listbox.configure args_in_lb [YScrollCommand (Scrollbar.set args_in_sb)];
  Scrollbar.configure args_in_sb [ScrollCommand (Listbox.yview args_in_lb)];
  let args_out_lb = Listbox.create args_out_frame [SelectMode Extended] in
  let args_out_sb = Scrollbar.create args_out_frame [Orient Vertical] in
  Listbox.configure args_out_lb [YScrollCommand (Scrollbar.set args_out_sb)];
  Scrollbar.configure args_out_sb [ScrollCommand (Listbox.yview args_out_lb)];

  let ports () = 
    let (_,_,_,parameters,ports,_,_,_,_,_) = Algorithm.algorithmname_content algolib algoname in
    let ports = List.sort (function _,_,_,_,_,order1,_ -> function _,_,_,_,_,order2,_ -> compare order1 order2) ports in
    let ports = List.filter (function _,_,_,_,portclass,_,_ -> portclass <> Port.Precedence_Port) ports in
    let ports_in,ports_out = List.partition (function (_,dir,_,_,_,_,_) -> dir=Port.In) ports in
    let pin = List.map (function (pname,_,_,_,_,_,_) -> pname) ports_in in
    let pout = List.map (function (pname,_,_,_,_,_,_) -> pname) ports_out in
    parameters,pin,pout in

  let function_update () =
    let parameters,pin,pout = ports () in
    let args = "("^(string_of_string_list (parameters@pin@pout) ",")^")" in
    Label.configure function_label [Text (function_name^args)] in

  let arguments_update () =
    let _,pin,pout = ports () in
    Listbox.insert args_in_lb End pin;
    Listbox.insert args_out_lb End pout in

  let shift pname pdir offset ports_names =
    let (_,_,_,_,ports,_,_,_,_,_) = Algorithm.algorithmname_content algolib algoname in
    let ports = List.filter (function _,_,_,_,portclass,_,_ -> portclass <> Port.Precedence_Port) ports in
    let _,_,pdt_name,pdt_dim,_,porder,pposition = List.find (function p,d,_,_,_,_,_ -> p=pname && d=pdir) ports in
    match List.filter (function p,d,_,_,_,r,_ -> d=pdir && r=(porder+offset) && (not (List.mem p ports_names))) ports with
    | (portname,_,typename,typedim,_,_,_)::_ ->
	win.alg_top.tw_file_changed ();
	Algorithm.port_modify algolib algoname portname pdir portname typename typedim porder;
	Algorithm.port_modify algolib algoname pname pdir pname pdt_name pdt_dim (porder+offset);
	offset
    | _ -> 0 in

  let up_down_action_sb dir sb updown =
    let current_selection = List.sort compare (Listbox.curselection sb) in
    match current_selection with
    | [] -> ()
    | _ ->
	let ports =
	  List.map (function idx ->
	              Listbox.get sb idx,
                      match Listbox.index sb idx with
                      | Number i -> i
                      | _ -> assert false)
            current_selection in
	let ports_names = List.map fst ports in
	let offset,process_ports = match updown with
	| true -> -1,ports
	| false -> 1,(List.rev ports) in
	let indices = List.map (function pname,oldidx -> oldidx,(oldidx+(shift pname dir offset ports_names))) process_ports in
	let indices = List.sort (function idx1,_ -> function idx2,_ -> compare idx1 idx2) indices in
	let (old_first,new_first),(old_last,new_last) = (List.hd indices),(List.hd (List.rev indices)) in
	Listbox.delete sb (Number old_first) (Number old_last);
	Listbox.insert sb (Number new_first) ports_names;
	Listbox.selection_set sb (Number new_first) (Number new_last) in

  let up_down_action updown =
    up_down_action_sb Port.In args_in_lb updown;
    up_down_action_sb Port.Out args_out_lb updown;
    function_update () in

  let update_graphs () = 
    List.iter (fun (win,g) -> algo_display (win,g)) win.alg_top.tw_algos in

  let up_bn = Button.create args_bn_frame [Text "up"; Command (function () -> up_down_action true)] in
  let down_bn = Button.create args_bn_frame [Text "down"; Command (function () -> up_down_action false)] in

  let ok_action _ = Algorithm.ports_order_sort_libname algolib algoname;
    update_graphs ();
    Tk.destroy window in
  let ok = Button.create window [Text "ok"; Command ok_action] in
  bind window [[],KeyPressDetail "Escape"] (BindSet ([],ok_action));

  pack [function_label] [];
  pack [up_bn;down_bn] [];
  pack [args_in_lb] [Fill Fill_Both;Side Side_Left; Expand true];
  pack [args_in_sb] [Fill Fill_Y;Side Side_Left];
  pack [args_in_frame] [Fill Fill_Both;Side Side_Left;Expand true];
  pack [args_out_lb] [Fill Fill_Both;Side Side_Left; Expand true];
  pack [args_out_sb] [Fill Fill_Y;Side Side_Left];
  pack [args_out_frame] [Fill Fill_Both;Side Side_Right;Expand true];
  pack [args_bn_frame] [Side Side_Right];
  pack [args_frame] [Fill Fill_Both;Expand true];
  pack [ok] [Side Side_Bottom];
  function_update ();
  arguments_update ()

and algo_description_edit win =
  let (_,_,_,_,_,_,_,_,old_description,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
  let help = "syntax: any string accepted, except quotes" in
  let title = "Enter the description of this definition:" in
  let f new_description =
    match String.contains new_description '"' with
    | true -> error_message win.alg_top "Syntax error; new description ignored"
    | false -> win.alg_top.tw_file_changed ();
    Algorithm.definition_modify_description win.alg_library win.alg_name new_description in
  ask_for old_description help title f win.alg_widget win.alg_top (WAsk_definition_description_modify win.alg_path)

and algo_code_phases_edit win =
  let phases_content_f alglib algname =
    let  (_,_,_,_,_,_,_,_,_,code_phases)= Algorithm.algorithmname_content alglib algname in
      code_phases in
  let phases_modify_f alglib algname code_phases =
    Algorithm.definition_modify_code_phases alglib algname code_phases in
    code_phases_edit win.alg_widget win.alg_top win.alg_library win.alg_name [InitSeq;EndSeq] phases_content_f phases_modify_f
      
(* ------------------------------------------------------------------------- *)
(*              WINDOWS MANIPULATION FUNCTIONS                               *)
(* ------------------------------------------------------------------------- *)

and algo_close top path =
  generic_window_close top (WAlgorithm path);
  top.tw_algos <- List.filter (function (algo,_) -> algo.alg_path<>path) top.tw_algos

and algo_close_others top path =
  List.iter (function (algo,_) -> algo_close top algo.alg_path) (List.filter (function (algo,_) -> algo.alg_path<>path) top.tw_algos)

and algo_close_all top =
  List.iter (function (algo,_) -> algo_close top algo.alg_path) top.tw_algos

(* -- Computes a position for ports and references ------------------------- *)
and auto_position (win,g) =
  let (_,_,_,_,_,_,dpds,_,_,_) = Algorithm.algorithm_cond_content win.alg_library win.alg_name (win.alg_variable,win.alg_current_value) in
  let pred v =
    let (vname,vtype) = Graph_ctk.vertexid_of_vertex v in
    let convert l =
     (* Filtering. In case there is a cycle due to a memory, we still need vertices without predecessors *)
      let nomem =
	List.filter (function ((srcrefname,_),_,_,_) -> srcrefname = "" || not (Algorithm.is_memory_reference win.alg_library win.alg_name srcrefname)) l in
      List.map (function ((srcrefname,srcportname),_,_,_) -> match srcrefname with 
      | "" -> Graph_ctk.find_vertex g (vport_of_port (srcportname,Port.In))
      | _ -> Graph_ctk.find_vertex g srcrefname) nomem in

    match vtype with
    | VPort ->
	(let (pname,pdir) = port_of_vport vname in
	match pdir with 
	| Port.In -> []
	| Port.Out ->
	    let preds = List.filter (function (_,(dstrefname,dstportname),_,_) -> dstrefname = "" && dstportname = pname) dpds in
	    convert preds)
    | VReference -> let preds = List.filter (function (_,(dstrefname,dstportname),_,_) -> dstrefname = vname) dpds in
      convert preds in
  
  let origin =
    let cvs = Graph_ctk.canvas g in
    Graph_ctk.conv_window_canvas cvs 0 0 in
  let step_sizes = autopos_step_box win.alg_widget in
  Graph_ctk.place_oriented g pred origin step_sizes

(* -- Update menus --------------------------------------------------------- *)
and algo_menus_update (win,g) =
  let menu_1,menu_2 =
    List.assoc "Window" win.alg_menus,
    List.assoc "Edit" win.alg_menus in
  clear_menu menu_1;
  clear_menu menu_2;
  let menu_cvs = Graph_ctk.get_menu_canvas g in
  let menu_reference = Graph_ctk.get_menu_vertex g VReference in
  let menu_port = Graph_ctk.get_menu_vertex g VPort in
  let algo_is_main = Algorithm.algo_is_main win.alg_library win.alg_name in
  let (_,_,algo_class,_,_,_,_,_,_,_) =  Algorithm.algorithmname_content win.alg_library win.alg_name in
  let algo_is_operation = algo_class = Operation
  and algo_is_memory = match algo_class with
  | Memory _ -> true
  | _ -> false
  and algo_is_readonly = win.alg_library <> "" || (algo_class = Internal)
  and algo_is_conditioned = win.alg_variable<>"" in
  (* Fill menus with graph commands *)
  Graph_ctk.update_menu_canvas g menu_cvs;
  Menu.add_separator menu_cvs;
  Graph_ctk.update_menu_edit g menu_2;
  Menu.add_separator menu_2;

  (* Refresh command *)
  let f wgt = Menu.add_command wgt [Label "Refresh"; Accelerator "Ctrl-R";Command (function () -> algo_display (win,g))] in
  bind win.alg_widget [[Control],KeyPressDetail "r"] (BindSet ([], function _ -> algo_display (win,g)));
  f menu_1;
  (* Fit on grid command *)
  let f wgt = Menu.add_command wgt [Label "Align"; Command (function () -> Graph_ctk.graph_fit g)] in
  f menu_1;
  (* Auto position command *)
  let f wgt = Menu.add_command wgt [Label "Auto position"; Command (function () -> auto_position (win,g))] in
  f menu_1;

  Menu.add_separator menu_1;

  (* Close command *)
  let f wgt = Menu.add_command wgt [Label "Close"; Accelerator "Ctrl-W"; Command (function () -> algo_close win.alg_top win.alg_path)] in
  bind win.alg_widget [[Control],KeyPressDetail "w"] (BindSet ([], function _ -> algo_close win.alg_top win.alg_path));
  f menu_1;
  (* Close others command *)
  let f wgt = Menu.add_command wgt [Label "Close Others"; Command (function () -> algo_close_others win.alg_top win.alg_path)] in
  f menu_1; 
  (* Close all command *)
  let f wgt = Menu.add_command wgt [Label "Close All"; Command (function () -> algo_close_all win.alg_top)] in
  f menu_1;   
  (* Create condition command *)
  let f wgt = Menu.add_command wgt [Label "Create Condition";
                                    Command (function () -> algo_condition_add (win,g));
                                    State (if (algo_is_operation && (not algo_is_main) && (not algo_is_readonly))
                                           then Normal
                                           else Disabled)] in
  f menu_2; f menu_cvs;
  (* Delete condition command *)
  let f wgt = Menu.add_command wgt [Label "Delete Condition";
                                    Command (function () -> algo_condition_delete (win,g));
                                    State (if (algo_is_operation && (not algo_is_main) && algo_is_conditioned && (not algo_is_readonly))
                                           then Normal
                                           else Disabled)] in
  f menu_2; f menu_cvs;
  Menu.add_separator menu_2;
  Menu.add_separator menu_cvs;
  (* Create port & create reference *)
  let f wgt =
    Menu.add_command wgt [Label "Create Port";
                          Command (function () -> algo_port_add_ask (win,g));
                          State (if ((not algo_is_main) && (not algo_is_readonly))
                                 then Normal
                                 else Disabled)] in
  f menu_2; f menu_cvs;

  let algolistadd = List.filter (function l,n -> l<>win.alg_library || n<>win.alg_name) (Algorithm.algo_list ()) in
  let f wgt =
    Menu.add_command wgt [Label "Create Reference";
                          Command (function () -> algo_reference_add_ask (win,g));
                          State (if (algo_is_operation && (not algo_is_readonly) && (algolistadd <> []))
                                 then Normal
                                 else Disabled)] in
  f menu_2; f menu_cvs;
    let f wgt =
      Menu.add_command wgt [Label "Find Port"; Command (function () -> algo_port_select (win,g))] in
      f menu_2; f menu_cvs;
    let f wgt =
      Menu.add_command wgt [Label "Find Reference"; Command (function () -> algo_reference_select (win,g))] in
      f menu_2; f menu_cvs;

  Menu.add_separator menu_2;
  Menu.add_separator menu_cvs;  
  (* Main definition command *)
  bind win.alg_widget [[Control], KeyPressDetail "m"]
      (BindSet ([], function _ -> algo_main_set win; algo_update_main win.alg_top));
  let f wgt = Menu.add_command wgt [Label "Main Definition";
                                    Accelerator "Ctrl-M";
                                    Command (function () -> algo_main_set win; algo_update_main win.alg_top)] in
  f menu_2; f menu_cvs;
  Menu.add_separator menu_cvs;
  Menu.add_separator menu_2;
  (* Modify name command *)
  (let f wgt =
     Menu.add_command wgt [Label "Change name";
                           Command (function () -> algo_definition_modify win);
                           State (if (not algo_is_readonly)
                                  then Normal
                                  else Disabled)] in
     f menu_2; f menu_cvs);
  (* Parameters command *)
  let command_name, command_help =
    if algo_is_main
    then "Parameters Values", "Syntax: parameter_name1=value1 parameter_name2=value2 ... \n example: N=3 M=5"
    else "Parameters Names", "Syntax: parameter_name1 parameter_name2 ...\n example: X T" in
  let command () =
    let (_,_,_,arg_names,_,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    let initial_string =
      if algo_is_main
      then
        List.fold_left2 (fun s n v -> s ^ n ^ "=" ^ (Symbolic.string_of_expression v) ^ "\n")
          "" arg_names (Application.algo_main_argumentsvalues_get ())
      else
        (string_of_string_list arg_names "\n") in
    match edit win.alg_top.tw_frame initial_string command_help command_name with
    | Some string ->
	win.alg_top.tw_file_changed ();
	if (not algo_is_main)
        then
          let names, err = args_names_of_string string in
	  if (err <> "") then error_message win.alg_top err;
	  Algorithm.algo_arguments_names_set win.alg_library win.alg_name names
        else
	  let names, values, _ = args_names_args_values_of_string win.alg_path string in
	  Algorithm.algo_arguments_names_set win.alg_library win.alg_name names;
	  Algorithm.algo_main_arguments_values_set values;
    | None -> () in
  let f wgt = Menu.add_command wgt [Label command_name;
                                    Command command;
                                    State (if (not algo_is_readonly)
                                           then Normal
                                           else Disabled)] in
  f menu_2; f menu_cvs;

  (* Edit Durations of algorithm *)
  let f wgt =
    Menu.add_separator wgt;
    Menu.add_command wgt [Label "Durations"; Command (function () -> algo_durations_edit win)] in
  f menu_2; f menu_cvs;

  (* Edit ports order *)
  let f wgt =
    Menu.add_command wgt [Label "Ports Order";
                          Command (function () -> algo_ports_order_edit win g);
                          State (if (not algo_is_readonly)
                                 then Normal
                                 else Disabled)] in
  f menu_2; f menu_cvs;

  let f wgt =
    Menu.add_command wgt [Label "Description";
                          Command (function () -> algo_description_edit win);
                          State (if (not algo_is_readonly)
                                 then Normal
                                 else Disabled)] in
  f menu_2; f menu_cvs;

  let f wgt =
    let (_,_,alg_class,_,_,_,_,_,_,_) = Algorithm.algorithmname_content win.alg_library win.alg_name in
    Menu.add_command wgt [Label "Code Generation Phases";
                          Command (function () -> algo_code_phases_edit win);
                          State (if ((not algo_is_readonly) && (Algorithm.atomic win.alg_library win.alg_name))
                                 then Normal
                                 else Disabled)] in
  f menu_2; f menu_cvs;

  (* Update reference and port menus *)
  Graph_ctk.update_menu_vertices g;
  Menu.add_separator menu_reference;
  Menu.add_separator menu_port;

  let f wgt = Menu.add_command wgt [Label "Modify";
                                    Command (function () -> algo_selection_modify (win,g));
                                    State (if (not algo_is_readonly)
                                           then Normal
                                           else Disabled)] in
  f menu_reference; f menu_port;

  let f wgt = Menu.add_command wgt [Label "Description";
                                    Command (function () -> reference_description_edit (win,g));
                                    State (if (not algo_is_readonly)
                                           then Normal
                                           else Disabled)] in
  f menu_reference;

  Menu.add_separator menu_reference;

  let submenu_xsc title xsc_state xsc_fun =
    let submenu = Menu.create menu_reference [TearOff false] in
      Menu.add_command submenu [Label "no xsc";
                                Command (function () -> xsc_fun (win,g) "")];
      List.iter (function xsc -> Menu.add_command submenu [Label xsc;
                                                           Command (function () -> xsc_fun (win,g) xsc)])
      (List.sort compare (Application.xsc_namelist ()));
    Menu.add_cascade menu_reference [Label title;
                                     Menu submenu;
                                     State (if (xsc_state)
                                            then Normal
                                            else Disabled)] in

  let (verticesid,_) = Graph_ctk.selected g in
  let conditioned = 
    List.for_all (fun (vname,vtype) -> match vtype with
		  | VReference -> let (_,deflib,defname,_,_,_,_,_) = Algorithm.referencename_content win.alg_library win.alg_name vname in
		      Algorithm.conditioned deflib defname
		  | _ -> false) verticesid in
  let repeated =
    List.for_all (fun (vname,vtype) -> match vtype with
		  | VReference -> let (_,_,_,_,_,_,ref_repetition,_) = Algorithm.referencename_content win.alg_library win.alg_name vname in
		      (match ref_repetition with
		       | Calculated (Repeat i) -> i > 1
		       | _ -> false)
		  | _ -> false) verticesid in
    
    submenu_xsc "Attach Reference to Software Component" true (algo_change_xsc AttachRef);
    submenu_xsc "Attach CondI to Software Component" conditioned (algo_change_xsc AttachCondI);
    submenu_xsc "Attach CondO to Software Component" conditioned (algo_change_xsc AttachCondO);
    submenu_xsc "Attach Explode to Software Component" repeated (algo_change_xsc AttachExplode);
    submenu_xsc "Attach Implode to Software Component" repeated (algo_change_xsc AttachImplode);
    submenu_xsc "Attach All to Software Component" true (algo_change_xsc AttachAll)

(* -- Algo new ------------------------------------------------------------- *)
and algo_new_win top bindcommand lib name path =
  let wgt = Toplevel.create top.tw_frame [] in
    algo_set_title wgt lib name path;
    let menu_bar = frame_create wgt in
    let menu_create text =
      let btn = Menubutton.create menu_bar [Text text] in
      let mnu = Menu.create btn [TearOff false] in
	Menubutton.configure btn [Menu mnu];
	pack [btn] [Side Side_Left];
	text,mnu in
    let menus = List.fold_left (function menus -> function t -> (menu_create t)::menus) [] ["Window";"Edit"] in
    let bar = frame_create wgt  in
    let barcond = frame_create wgt in
      pack [menu_bar; bar;barcond] [Fill Fill_X];
      let frm = Frame.create wgt [] in

	pack [frm] [Fill Fill_Both; Expand true];

	let win = {
	  alg_top = top;
	  alg_widget = wgt;
	  alg_bar_cond = barcond;
	  alg_library = lib;
	  alg_name = name;
	  alg_path = path;
	  alg_menus = menus;
	  alg_dimension = (0,0);
	  alg_variable = "";
	  alg_values = [];
	  alg_current_value = 1;
	  alg_buttons = [];
	  alg_bindcommand = bindcommand;
	} in

	(* Graph creation *)
	let vtypelist = ["Reference",VReference;
			 "Port",VPort] in
	let etypelist = ["Strong Precedence/Data Communication",Strong_Precedence_Data;
			 (*"Weak Precedence/Data Communication",Weak_Precedence_Data;
			 "Data Communication",Data;*)
			 "Precedence",Precedence] in
	let clip = top.tw_alg_graphclip in

	let enter_f () = algo_info_change win Canvas in
	let leave_f () = algo_info_change win Clean in
	let g = Graph_ctk.create frm win.alg_widget None enter_f leave_f (algo_edge_add win) clip (copy win) (vertex_paste win) (edge_paste win) vtypelist etypelist Strong_Precedence_Data (fun _ -> ()) (fun _ -> ()) (fun _ _ _ -> ()) false (function () -> win.alg_top.tw_current_directory) None in
	  Graph_ctk.configure_scale g 0.1 1. 0.01;

	  (* Update repetition factors. We have to do this here as we can't calculate factor when creating each reference, due to unspecified parameters when creating the algorithm from it's leaves to it's root when we load it. *)
	  let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content lib name in
	    List.iter (fun (refname,_,_,_,_,_,_,_) -> reference_factor_update lib name path top refname) refs;

	  (* Graph options *)
	  let readonly = match lib with
	    | "" -> "0"
	    | _ -> "" in
	    Graph_ctk.option_set g Graph_ctk.ReadOnly readonly;

	    let cvs = Graph_ctk.canvas g in
	    let dimension = Winfo.width cvs,Winfo.height cvs in
	      win.alg_dimension <- dimension;

	      (* Bindings *)
	      (match bindcommand with
	       | Some f -> f wgt;
	       | None -> ());

	      top.tw_algos <- (win,g) :: top.tw_algos;
	      (* Displays the window and the graph *)
	      algo_display_var (win,g);
	      (* Fill the menus *)
	      algo_menus_update (win,g);
	      wgt

and algo_open top bindcommand lib name path =
  let path = match path with
  | [] -> [lib;name]
  | _ -> path in
  generic_window_open top (WAlgorithm path) (function () -> algo_new_win top bindcommand lib name path)

let operation_definition_edit top globalbindings =
  let title = "Choose definition to edit :" in
  choose_libs_defs (function (algolib,algoname) -> algo_open top globalbindings algolib algoname []) (Algorithm.algo_list ()) title Widget.default_toplevel top WAsk_edit_definition

let graph_definition_delete algoname top =
  let definition_delete_in refname (alg_name,g) =
    let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content "" alg_name in
    let todelete = List.filter (function (_,deflib,defname,_,_,_,_,_) -> deflib = "" && defname = algoname) refs in
    List.iter (function (rname,_,_,_,_,_,_,_) -> Graph_ctk.vertex_remove_graph g (rname,VReference)) todelete in
  List.iter (function (w,g) -> match w.alg_library with
  | "" -> definition_delete_in algoname (w.alg_name,g)
  | _ -> ()) top.tw_algos

let operation_definition_delete top algoname =
  top.tw_file_changed ();
  (match List.filter (function (w,_) -> w.alg_library="" && w.alg_name=algoname) top.tw_algos with
  | (wdw,_)::_ -> algo_close wdw.alg_top wdw.alg_path
  | _ -> ());
  (* Refresh graphs which contain a reference on this definition *)
  graph_definition_delete algoname top;
  Algorithm.algo_delete "" algoname

let operation_definition_create top globalbindings algotype =
  let ask_for_fun namelist =
    let rgxp = "\\([a-zA-Z_][a-zA-Z0-9_]*\\)[ \n]*\\(<\\([^>]+\\)>\\)?[ \n]*\\(\\[\\([a-zA-Z0-9\\+-\\*/]+\\)[..]?[ \n]*\\([a-zA-Z0-9\\+-\\*/]+\\)?\\]\\)?" in
    let namelist,err = analyze namelist rgxp in
    (match err <> "" with
     | true -> error_message top err
     | false -> ());
    let rec f l = match l with
    | [name;a;args;mmr;mmra;mmrb]::t ->
	(try
	   let args,err = analyze args "\\([^,;]+\\)" in
	     (match err <> "" with
	      | true -> error_message top err
	      | false -> ());
	  let args = List.concat args in
	  let dim = Coord.Coord2d (canvas_size_default_x,canvas_size_default_y) in
	  (match algotype with
	  | Constant -> Algorithm.constant_create "" name args [] dim "";
	  | Sensor -> Algorithm.sensor_create "" name args [] dim "";
	  | Actuator -> Algorithm.actuator_create "" name args [] dim "";
	  | Memory _ -> let a,b = match mmra,mmrb with
	    | "","" -> Symbolic.Float 1.,Symbolic.Float 1.
	    | a,"" -> Symbolic.Float 1., parse_expression [] a
	    | a,b -> parse_expression [] a,parse_expression [] b in
	    Algorithm.memory_create "" name args [] dim "" (a,b)
	  | Operation -> Algorithm.operation_create "" name args [] [] dim "" [LoopSeq]
	  | Internal -> ());
	  algo_open top globalbindings "" name [];
	  top.tw_file_changed ();
	  List.iter algo_menus_update top.tw_algos;
	  f t
	with Failure msg -> error_message top msg)
    | _ -> () in
    f namelist in
  ask_for "" "syntax: definition_name <parameters_names>\n'<parameters_name>' is optionnal.\n example: delay <Inits,N>" "Name of the new definition : " ask_for_fun Widget.default_toplevel top WAsk_create_definition

let port_types_list_view top =
  let types_window types_list_values =
    let wgt = Toplevel.create top.tw_frame [] in
    Wm.title_set wgt "Port types";
    let title_label = Label.create wgt [Text " Port types used in main definition and its hierarchy:  \n"] in
    let types_frame = Frame.create wgt [BorderWidth (Pixels 5)] in
    let types_list = Listbox.create types_frame [SelectMode Extended] in
    let types_sbar = Scrollbar.create types_frame [Orient Vertical] in
      Listbox.configure types_list [YScrollCommand (Scrollbar.set types_sbar)];
      Scrollbar.configure types_sbar [ScrollCommand (Listbox.yview types_list)];
      Listbox.insert types_list End types_list_values;
      let bottom = Frame.create wgt [BorderWidth (Pixels 5)] in
      let ok_action = fun _ -> generic_window_close top WPort_types in
      let ok = Button.create bottom [Text "ok"; Command ok_action] in
      let refresh_action = fun _ -> Listbox.delete types_list (Number 0) End;
	Listbox.insert types_list End (Algorithm.port_types_list ()) in
      let refresh = Button.create bottom [Text "refresh"; Command refresh_action] in
	bind wgt [[], Destroy] (BindSet ([],ok_action));
	bind ok [[],KeyPressDetail "Return"] (BindSet ([],ok_action));
	pack [title_label] [];
	pack [types_list] [Fill Fill_Both; Side Side_Left; Expand true];
	pack [types_sbar] [Fill Fill_Y; Side Side_Left];
	pack [types_frame] [Fill Fill_Both; Expand true];
	pack [ok] [Side Side_Left; Expand true];
	pack [refresh] [Side Side_Left; Expand true];
	pack [bottom] [Side Side_Bottom; Fill Fill_X];
	Focus.set ok;
	wgt in
    
    try 
      let types_list_values () = Algorithm.port_types_list () in
	generic_window_open top WPort_types (fun () -> types_window (types_list_values ()))
    with Failure msg -> error_message top msg
