(************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            Julien Forget                              *)
(*			    Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)


(* WARNING : be cautious when changing this module. *)
(* There are very tough performance requirements, especially when dealing with communications. *)

(** This module contains the functions of the adequation heuristic. The
functor enables us to choose the progress box, which is used to display
the progress of the adequation. That way we can for instance have a
silent progress box for command line mode.*)

module Make (Progress_box : Progress_box.PROGRESS_BOX_SIG) =
  struct
    open Types
    open Adequationtypes

    let debug_level = 0

    let name_of_operator o = match o with
    | Operator opr -> opr.oprref_name
    | Media mda -> mda.mdaref_name

    let lefe_table = Hashtbl.create 30

    let datas = Hashtbl.create 30

    let schedules = Hashtbl.create 30

    let xsc_constraints = Hashtbl.create 30

    let archilib = ref ""

    let archiname = ref ""

    let adequation_order = ref 0

    let psll ll = List.fold_left (function s -> function o1,o2 -> s^(name_of_operator o1)^"->"^(name_of_operator o2)^";") "   " ll

    let dbg graph =
      let o,d,ds=ref 0,ref 0,ref 0 in
      Hashtbl.iter (function n -> function opn -> o := !o + 1; ds:=0; List.iter (function d -> ds:= !ds + 1) opn.t_opn_dependences_successors;d:=!d + !ds;ps(n^" : "^(string_of_int !ds)^" dependences")) graph;
      ps ("\nOperations : "^(string_of_int !o)^", Dependences : "^(string_of_int !d))

    let name_of_og (o,g) = (name_of_operator o)^(match g with | Some g -> "."^g.gte_name | _ -> "")

    let psr r = match r with
    | [] -> ()
    | hd::tl -> ps (List.fold_left (function s -> function og -> s^"->"^(name_of_og og)) (name_of_og hd) tl)

    let debug_constraints s =
      ps s;
      Hashtbl.iter (function xsc -> function oprs -> ps (xsc^" on "^(List.fold_left (function s -> function opr -> match opr with
      | Operator o -> s^o.oprref_name^" "
      | _ -> s) "" oprs))) xsc_constraints

(**Returns operator on which opn has been scheduled. Fails if the operation is not scheduled yet. *)
    let pi opn = match opn.t_opn_operator with
    | Some opr -> opr
    | _ -> raise (Failure ("Adequation.pi : error ("^(name_of_operation opn)^")"))

(** Returns the list of operators compatible with constraints. All operators if constraints are empty. *)
    let operators_of_constraints constraints =
      let oprs_archi = Application.operators_list !archilib !archiname in
      match constraints with
      | (_,oprlist)::_ ->
	  (match List.filter (function (alib,aname),_ -> alib = !archilib && aname = !archiname) oprlist with
	  | [] -> oprs_archi
	  | oprlist ->
	      let oprs_name = Architecture.operators_list !archilib !archiname in
	      let oprlist = List.filter (function _,oprname -> List.mem oprname oprs_name) oprlist in
	      List.map (function _,oprname -> Operator (Architecture.operator_reference !archilib !archiname oprname)) oprlist)
      | _ -> oprs_archi

(** Returns operators on which ref has been constrained (for old versions, direct constraints) *)
    let constraints reference_path =
      let constraints = List.filter (function refpath,_ -> refpath=reference_path) Application.application.app_constraints in
      operators_of_constraints constraints

(** Returns the list of operators on which xscname is constrained *)
    let absoluteconstraints xscname =
      let constraints = List.filter (function xsc,_ -> xsc=xscname) Application.application.app_xsc_absolute_constraints in
      operators_of_constraints constraints

(** Returns the list of operators on which operation is constrained *)
    let operators_constraint operation =
      let xsc = operation.t_opn_xsc_name in
      match xsc with
      | "" -> (*let path = path operation in
		 (match constraints path with
		 | [] -> *)Application.operators_list !archilib !archiname
	    (*        | oprlist -> oprlist) *)
      | _ -> try match Hashtbl.find xsc_constraints xsc with
	| [] -> raise (Failure ("Constraints : impossible to find an operator for "^(identifier_of_operation operation)))
	| oprs -> oprs with Not_found -> failwith "Adequation.operators_constraint error"
	      
    let rec relative_constraints_update () =
      ()

    let operators_constraints_init graph =
      Hashtbl.clear xsc_constraints;
      let init_absolute xsc =
	let refs = List.map fst (Application.refs_of_xsc xsc) in
	let constraints = match refs with
	| [] -> Application.operators_list !archilib !archiname
	| _ -> intersections (List.map constraints refs) in (* if no constraint, this returns all operators in archi*)
	let absolute_constraints = absoluteconstraints xsc in
	let oprs_absolute = intersection constraints absolute_constraints in
	Hashtbl.add xsc_constraints xsc oprs_absolute in
      List.iter init_absolute (Application.xsc_namelist ());
      relative_constraints_update ()

(** Returns operators on which a duration for operation has been defined *)
    let operators_able_to_execute operation =
      let opnlib,opnname = deflibname operation in
      let oprs = Architecture.operators_able_to_execute !archilib !archiname opnlib opnname in
      match oprs with
      | [] -> raise (Failure ("No operator able to execute :\nOperation : "^
			      (name_of_operation operation)^"\nDefinition : "^opnlib^"/"^opnname^
		    "\n\nMaybe you forgot to define a duration for this definition.\n"))
      | _ -> List.map (function opr -> Operator opr) oprs

(** Returns a new schedule gaps list. This list is obtained by deleting interval [esfs;esfs+delta] in gaps.*)
    let remove_gap esfs eefs gaps =
(*  ps ("Removing gaps "^(string_of_gaps_list gaps)^" with esfs "^(string_of_float esfs)^" and eefs "^(string_of_float eefs));*)
      match eefs-.esfs with
      | 0. -> gaps
      | _ -> 
	  (* Tests are surely far from optimal, but I think more readable *)
	  let rec f oldgaps processed =
	    match oldgaps with
	    | [] -> processed (* may happen when updating the gap list of a schedule different from the schedule in which the operation is added (ie for schedule of condition non-exclusive with operation added) *)
	    | (inf,sup)::rest -> 
		match esfs >= sup with
		| true -> processed@oldgaps
		| false -> match (esfs <= sup && esfs >= inf) || (eefs <= sup && eefs >= inf) with
		  | true -> (match esfs <= inf && eefs >= sup with
		    | true -> f rest processed (* deleting gap *)
		    | false -> match esfs > inf && eefs < sup with
		      | true -> f rest (processed@[(eefs,sup);(inf,esfs)]) (* two smaller gaps surrounding [esfs;eefs]. Notice that gaps list will be in decreasing order, which is more optimal than in increasing order. *)
		      | false -> match esfs <= inf && eefs <= sup with
			| true -> f rest (processed@[(eefs,sup)]) (* shortening gap at the left *)
			| false -> match esfs <= sup && sup <= eefs with
			  | true -> f rest (processed@[(inf,esfs)]) (* shortening gap at the right *)
			  | false -> f rest (processed@[(inf,sup)])) (* no intersection with this gap, keep it *)
		  | false -> f rest (processed@[(inf,sup)]) (* no intersection with this gap, keep it *) in
	  f gaps []
	    
(* Function not used anymore, I keep it just in case... *)
(*let restore_gap esfs eefs gaps =
(*   ps ("Restoring gaps "^(string_of_gaps_list gaps)^" with esfs "^(string_of_float esfs)^" and eefs "^(string_of_float eefs)); *)
   match eefs-.esfs with
   | 0. -> gaps
   | _ -> (* Tests are surely far from optimal, but I think more readable *)
   let rec f oldgaps processed prec =
   match oldgaps with
   | [] -> (match prec with
   | Some (inf,sup)-> processed@[(esfs,sup)]
   | None -> processed) (* should never happen *)
   | (inf,sup)::rest -> 
   match prec with
   | None -> (match inf = eefs with
   | true -> f rest processed (Some (inf,sup))
   | false -> (match sup = esfs with
   | true -> processed@((inf,eefs)::rest)
   | false -> f rest (processed@[(inf,sup)]) None))
   | Some (inf_prec,sup_prec) -> match sup = esfs with
   | true -> processed@[(inf,sup_prec)]@rest
   | false -> processed@[(esfs,sup_prec);(inf,sup)]@rest in
   f gaps [] None*)

(** Creates a backup for the gaps lists on all media *)
    let create_gaps_backups () =
      Hashtbl.iter (fun _ (oprclass,oprschedule) -> match oprclass with
      | Operator _ -> ()
      | Media _ -> Hashtbl.iter (fun _ condschedule ->
	  condschedule.t_sch_gaps_backup <- condschedule.t_sch_gaps) oprschedule) schedules

(** Restores the gaps lists on all media from their backups *)
    let restore_gaps () =
      Hashtbl.iter (fun _ (oprclass,oprschedule) -> match oprclass with
      | Operator _ -> ()
      | Media _ -> Hashtbl.iter (fun _ condschedule ->
	  condschedule.t_sch_gaps <- condschedule.t_sch_gaps_backup) oprschedule) schedules

(** Returns the gaps list for condition cond, considering existing schedule operschedule. *)
    let initial_gaps oprschedule cond =
(*   ps ("Initialising gaps for cond "^(string_of_condlist cond)); *)
      let gaps = ref [(0.,max_float)] in
      (* There's probably a more optimal way to do this *)
      Hashtbl.iter (fun c condschedule -> match exclusion_condition c cond with
      | true -> ()
      | false -> List.iter (fun (opn,esfs,eefs) -> gaps := remove_gap esfs eefs !gaps) condschedule.t_sch_operations) oprschedule;
(*     ps ("Gaps initialised are now:"^(string_of_gaps_list !gaps)); *)
      !gaps

(** Adds data produced by opn as an available data on operator after time eefs *)
    let update_datas operator data opn prt eefs =
      let newoprs = (operator,opn,prt,eefs)::(try Hashtbl.find datas data with Not_found -> []) in
      Hashtbl.replace datas data newoprs
	
(** Calculates routing tables of all operators and media *)
    let route oprlist mdalist =
      (* Count Oprs, initialize their id, allocate and initialize their routings: *)
      let nboprs = List.fold_left (fun maxid opr -> opr.oprref_id <- maxid;maxid+1) 0 oprlist in
      let archilib, archiname = Application.archi_main_get () in
      let archi = Application.architecture archilib archiname in
      let init_route_opr nboprs opr =
	opr.oprref_neighbours <- Architecture.links_operator archi opr;
	opr.oprref_links2 <- Array.make nboprs (-1) in
      let init_route_mda nboprs mda =
	mda.mdaref_neighbours <- Architecture.links_media archi mda;
	mda.mdaref_links2 <- Array.make nboprs (-1) in

      List.iter (fun opr -> init_route_opr nboprs opr;
	opr.oprref_links2.(opr.oprref_id) <- 0) oprlist;
      List.iter (init_route_mda nboprs) mdalist;   

      (* Compute routing tables by increasing routing distances *)
      let dist = ref 0 in
      let maxdist = ref 0 in
      let rec process_oprs_connected_opr oprs dst mda =
	match oprs with
	| [] -> ()
	| ((_,_),(opr_cnc,_))::rest -> let opr_cnc = match opr_cnc with
	  | Operator opr -> opr
	  | _ -> failwith "Adequation.route" in
	  match opr_cnc.oprref_links2.(dst.oprref_id) = !dist with
	  | false -> process_oprs_connected_opr rest dst mda
	  | true -> mda.mdaref_links2.(dst.oprref_id) <- !dist + 1;
	      maxdist := !dist + 1 in
      let rec process_oprs_connected_mda oprs dst opr =
	match oprs with
	| [] -> ()
	| ((_,_),(mda_cnc,_))::rest -> let mda_cnc = match mda_cnc with
	  | Media mda -> mda
	  | _ -> failwith "Adequation.route" in
	  match mda_cnc.mdaref_links2.(dst.oprref_id) = !dist with
	  | false -> process_oprs_connected_mda rest dst opr
	  | true -> opr.oprref_links2.(dst.oprref_id) <- !dist + 1;
	      maxdist := !dist + 1 in
      while dist = maxdist do
	(* Media routing tables *)
	List.iter (fun mda -> let oprs_connected = Architecture.links archilib archiname (Media mda) in
	List.iter (fun dst -> 
	  match mda.mdaref_links2.(dst.oprref_id) < 0 with
	  | false -> ()
	  | true -> process_oprs_connected_opr oprs_connected dst mda) oprlist) mdalist;
	(* Operators routing tables *)
	List.iter (fun opr -> let mda_connected = Architecture.links archilib archiname (Operator opr) in
	List.iter (fun dst -> 
	  match opr.oprref_links2.(dst.oprref_id) < 0 with
	  | false -> ()
	  | true -> process_oprs_connected_mda mda_connected dst opr) oprlist) oprlist;
	dist := !dist + 1
      done
	(*;
	   ps "media";
	   ps (List.fold_left (fun s opr -> s^opr.oprref_name^" : "^(string_of_int opr.oprref_id)^", ") "Ids : " oprlist);
	   List.iter (fun mda -> ps (fst (Array.fold_left (fun (s,id) dist -> ((s^"; "^(string_of_int id)^":"^(string_of_int dist)),id+1)) (mda.mdaref_name,0) mda.mdaref_links2))) mdalist;
	   ps "operator";
	   List.iter (fun opr -> ps (fst (Array.fold_left (fun (s,id) dist -> ((s^"; "^(string_of_int id)^":"^(string_of_int dist)),id+1)) (opr.oprref_name,0) opr.oprref_links2))) oprlist*)
	
(** Initializes the route table, data available on operators, schedules and operator_constraints *)
    let architecture_init graph =
      let alib,aname = Application.archi_main_get () in
      Hashtbl.clear datas;
      Hashtbl.clear schedules;
      archilib := alib;
      archiname := aname;
      let oprlist = List.map (Architecture.operator_reference !archilib !archiname ) (Architecture.operators_list !archilib !archiname) in
      let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
      List.iter (function o -> Hashtbl.add schedules o.oprref_name ((Operator o),Hashtbl.create 10)) oprlist;
      List.iter (function m -> Hashtbl.add schedules m.mdaref_name ((Media m),Hashtbl.create 10)) mdalist;
      route oprlist mdalist;
      operators_constraints_init graph

(** Schedules operation on operator at time esfs. *)
(* Remember that a call to this function with status true only happens just after a call with status false *)
    let schedule operation operator status esfs eefs =
      (match status with
      | true -> operation.t_opn_esfs <- esfs;
	  operation.t_opn_adequation_order <- !adequation_order;
	  adequation_order := !adequation_order + 1
      | false -> ());
      operation.t_opn_operator <- Some operator;
      let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
      let oldschedule,lastopn,gaps,gapsbackup = try 
	let cond_schedule = Hashtbl.find oprschedule operation.t_opn_condition in
	cond_schedule.t_sch_operations,cond_schedule.t_sch_last_opn,cond_schedule.t_sch_gaps,cond_schedule.t_sch_gaps_backup
      with Not_found -> ([],None,[],[]) in
      let gaps = match operator,gaps,status with
      | (Media _,[],true) -> initial_gaps oprschedule operation.t_opn_condition
      | _ -> gaps in
      (match status with
      | false -> operation.t_opn_pred_scheduled <- lastopn
      | true -> () (* previously done with a status false *));
      let newschedule = (match status with
      | true -> (operation,esfs,eefs)::oldschedule (* reverse order again to find it faster at deschedule *)
      | false -> oldschedule) in
      Hashtbl.replace oprschedule operation.t_opn_condition ({t_sch_operations=newschedule;t_sch_last_opn=Some operation;t_sch_gaps=gaps;t_sch_gaps_backup=gapsbackup});
      match operator,status with
      | (Media _,true) -> Hashtbl.iter (fun cond schedule -> match exclusion_condition cond operation.t_opn_condition with
	| true -> ()
	| false -> schedule.t_sch_gaps <- (remove_gap esfs eefs schedule.t_sch_gaps)) oprschedule
      | _ -> ()
	    
(**  *)
    let deschedule operation operator status =
      (*   ps ("Descheduling "^(name_of_operation operation)^" from ("^(string_of_float operation.t_opn_esfs)^" to "^(string_of_float (eefs operation))^" cond was "^(string_of_condlist operation.t_opn_condition)); *)
      let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
      let oldschedule,gaps,gapsbackup = 
	let cond_schedule = Hashtbl.find oprschedule operation.t_opn_condition in
	cond_schedule.t_sch_operations,cond_schedule.t_sch_gaps,cond_schedule.t_sch_gaps_backup in
      let newschedule = match status with
      | true -> let rec filter_opn processed todo =
	  match todo with
	  | [] -> processed (* Yet, this should never happen *)
	  | ((opn,_,_) as o)::rest -> match opn = operation with
	    | true -> processed@rest
	    | false -> filter_opn (processed@[o]) rest in
	filter_opn [] oldschedule
      | false -> oldschedule in
(*     ps (List.fold_left (fun s (opn,_,_) -> s^";"^(name_of_operation opn)) "New schedule: " newschedule); *)
      Hashtbl.replace oprschedule operation.t_opn_condition {t_sch_operations=newschedule;t_sch_last_opn=operation.t_opn_pred_scheduled;t_sch_gaps=gaps;t_sch_gaps_backup=gapsbackup};
      operation.t_opn_pred_scheduled <- None;
      operation.t_opn_operator <- None;
      operation.t_opn_esfs <- 0.

(** Deletes the communication corresponding to dpd. Deschedules all its communication operations. *)
    let rec delete_communication graph opr_dst dpd =
      (*   ps ("deleting com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))); *)
      let sopn = dpd.t_dpd_sopn in
      match (is_communication sopn) with
      | true -> let successor = match (is_communication dpd.t_dpd_dopn) with
	| true -> 1(* This com has previously been deleted, we must yet count it in the successors *)
	| false -> 0 in
	(match (((List.length sopn.t_opn_dependences_successors) + successor) > 1) with
	  (* Diffuse (send) data for several receives *)
	| true ->
	    let sopn = dpd.t_dpd_sopn in
	    let data = data_of_communication sopn in
	    (match sopn.t_opn_class with
	    | Communication (Send (_,opr_src,oprs)) ->
		Hashtbl.remove graph (identifier_of_operation sopn);
		let receivers = List.filter (fun opr -> opr <> opr_dst) oprs in
		(* Changing receiver list in other receive operations *)
		(* Should change this. Won't work if two identical receive exist (with different operation number #0 #1 etc). This may happen with conditionning *)
		List.iter (fun opn_rcv ->
		  let rcv_id = identifier_of_operation opn_rcv in
		  let opn_rcv = Hashtbl.find graph rcv_id in
		  let executor = execution_operator_of_communication opn_rcv in
		  Hashtbl.remove graph rcv_id;
		  let comclass = Communication (Receive (data,opr_src,receivers,executor)) in
		  opn_rcv.t_opn_class <- comclass;
		  Hashtbl.add graph (identifier_of_operation opn_rcv) opn_rcv) sopn.t_opn_successors; 
		(* Changing sopn receiver list and that's all. No recursive deletion as predecessors are used by other operations *)
		let comclass = Send (data,opr_src,receivers) in
		sopn.t_opn_class <- Communication comclass;
		let opnname = name_of_comclass comclass in
		sopn.t_opn_path <- [opnname];
		Hashtbl.add graph (identifier_of_operation sopn) sopn
	    | _ -> dependence_remove dpd) (* For instance : Receive having several successors on the same operator (reusing a com, nothing to delete except the dependence)*)
	| false -> 
	    (* 	     ps ("deleting"^(name_of_operation sopn)); *)
	    let dpds = sopn.t_opn_dependences_predecessors in (* Need to store dpds (which will be deleted by Hashtbl.remove) to enable terminal recursivity *)
	    let opr_dst = match sopn.t_opn_class with
	    | Communication (Send (_,sopr,_)) -> sopr
	    | _ -> opr_dst in
	    deschedule sopn (pi sopn) true;
	    Adequationtypes.operation_remove graph sopn;
	    List.iter (delete_communication graph opr_dst) dpds)
      | false -> ()

(** Deletes all predecessors coms of operation *)
    let delete_coms operation graph =
      let delete_one_com dpd =
	match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) || (pi dpd.t_dpd_sopn = pi operation) with
	| true -> ()
	| false -> 
	    (*ps ("delete com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_operation operation)^" (on "^(name_of_operator (pi operation)));*)
	    let opr_dst = operator_of_operator_class (pi operation) in
	    delete_communication graph opr_dst dpd in
      List.iter delete_one_com operation.t_opn_dependences_predecessors

(** Returns true if operation0 and operation1 are simultaneous *)
    let simultaneous s0 e0 s1 e1 = (s0<>e0) && (s1<>e1) && ((s1<=s0 && s0<e1) || (s1<e0 && e0<=e1) || (s0<=s1 && s1<e0) || (s0<e1 && e1<=e0))

(** Returns esfs (of any operation) on operator after time t0 with condition cond. *)
    let esfs_cond operation operator t0 cond =
      let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
      match operator with
      | Operator o ->
	  let topr = (*Notice that this calculation assumes that there is no gap in the schedule *)
	    Hashtbl.fold (fun cd {t_sch_last_opn=lastopn} esfs -> let lastopn = match cd = cond with
	    | true -> operation.t_opn_pred_scheduled (* we just scheduled current operation with this condition, so we must not take it into account *)
	    | false -> lastopn in
	    match lastopn with
	    | None -> esfs
	    | Some opn -> match exclusion_condition opn.t_opn_condition cond with
	      | true -> esfs
	      | false -> max esfs (eefs opn)) oprschedule 0. in
	  max topr t0
      | Media m -> (* this calculation enables to use gaps in the medium schedule *)
	  let delta_opn = delta operation.t_opn_class operator in
	  let rec gap_start l tmin =
	    match l with
	    | [] -> tmin
	    | (inf,sup)::rest -> let tmin = match ((t0+.delta_opn)<=sup && (inf+.delta_opn)<=sup) with
	      | true -> 
		  (*  		ps ("Interval ["^(string_of_float inf)^";"^(string_of_float sup)^"] ok for t0 = "^(string_of_float t0)^" with condition "^(string_of_condlist operation.t_opn_condition));  *)
		  max t0 inf (* we can use this gap *)
	      | false -> tmin in
	      match inf <= t0 with
	      | true -> tmin (* remember that the gap list is in decreasing order *)
	      | false -> gap_start rest tmin in
	  let gaps = (Hashtbl.find oprschedule operation.t_opn_condition).t_sch_gaps in
	  let gaps = match gaps with
	  | [] -> initial_gaps oprschedule operation.t_opn_condition
	  | _ -> gaps in
	  gap_start gaps max_float
	    
	    
(** Place in graph the communication corresponding to dependence dpd after instant tcond with status status *)
    let place_communication graph status tcond dpd =
      (* Place the communication of operation_src.port_src between operator_src and operator_dst after instant t0 *)
      let place_com (data,(operator_src,operation_src,port_src,t0)) operation_dst port_dst operator_dst dpdclass cond place status =

	(* Place the operation of communication opncom on the media *) 
	let place_com_media media opncom opnpred prtpred eefspred place receive =
	  (*      ps("place_com_media "^(name_of_operator (Media media))^" "^(name_of_operation opnpred));*)
	  let portdim = prtpred.t_prt_dim in
	  let portin = match prtpred.t_prt_class with
	  | Precedence_Port -> List.hd (precedence_ports ())
	  | _ -> {t_prt_name="in";t_prt_dir=In;t_prt_typename=prtpred.t_prt_typename;t_prt_dim=portdim;t_prt_class=prtpred.t_prt_class;t_prt_order=1}
	  and portout = match prtpred.t_prt_class with
	  | Precedence_Port -> List.nth (precedence_ports ()) 1
	  | _ -> {t_prt_name="out";t_prt_dir=Out;t_prt_typename=prtpred.t_prt_typename;t_prt_dim=portdim;t_prt_class=prtpred.t_prt_class;t_prt_order=1} in
	  
	  let opnname = unique_identifier graph [] (name_of_comclass opncom) in
	  let ports = match prtpred.t_prt_class with
	  | Precedence_Port -> [portin;portout]
	  | _ -> [portin;portout]@(precedence_ports ()) in
	  let opn = new_opn [opnname] (Communication opncom) [] ports [] [] cond (Some (Media media)) 0 0.
	      status (Ihm [opnname]) None "" "" "" [InitSeq;LoopSeq;EndSeq] in
	  let delta = delta (Communication opncom) (Media media) in
 	  schedule opn (Media media) false 0. 0.;
	  let esfs = match receive with (* This trick of using a boolean was first introduce to place several receives
					   just after the same send (diffuse), but is now used for all receives.
					   This avoids useless comutations and some misbehaviours.
					   However, it won't work any longer if
					   (delta receive <> 0) or if esfs doesn't rely for media
					   on the gaps calculation 
					   (otherwise, things done in function schedule are not correct anymore) *)
	  | true -> eefspred
	  | false -> esfs_cond opn (Media media) eefspred cond in
	  let eefs = esfs +. delta in
	  (match place with
	  | true -> debug_ps 3 debug_level ("Placing com operation "^(name_of_operation opn)^" on "^
					    (name_of_operator (Media media))^" from "^(string_of_float esfs)^
					    " to "^(string_of_float eefs)^" condition "^(string_of_condlist opn.t_opn_condition));
	      schedule opn (Media media) true esfs eefs
	  | false -> ());
	  let opn,portout = match place with
	  | true ->
	      Hashtbl.add graph (identifier_of_operation opn) opn;
	      Adequationtypes.dependence_add opnpred prtpred opn portin dpdclass cond status;
	      (* As routes don't cross, updating data and schedules is not needed if status is false. *)
	      (match status with
	      | true -> 
		  (* Update list of data *)
      		  (match opncom with
		  | Send (_,_,oprs) ->
		      (match Architecture.bustype media with
		      | SamMP,true -> update_datas (Media media) data opn portout eefs
		      | _ -> ())
		  | Receive (_,_,_,executor) -> update_datas (Operator executor) data opn portout eefs
		  | Write _ -> update_datas (Media media) data opn portout eefs
		  | Read (_,opr) -> update_datas (Operator opr) data opn portout eefs
		  | _ -> raise (Failure "Adequation.place_com_media error"));
	      | false -> ());
	      opn,portout
	  | false -> deschedule opn (Media media) false;
	      opnpred,prtpred in
	  opn,portout,eefs in
	
	let read s d sopn sprt esfs place =
	  place_com_media s (Read (data,d)) sopn sprt esfs place false in

	let write s d sopn sprt esfs place =
	  place_com_media d (Write (data,s)) sopn sprt esfs place false in

	let transfer s sam d sopn sprt esfs place =
	  place_com_media sam (Transfer (data,s,[d])) sopn sprt esfs place false in

	let send s media d sopn sprt esfs place =
	  place_com_media media (Send (data,s,[d])) sopn sprt esfs place false in

	let recv s media d executor sopn sprt esfs place =
	  place_com_media media (Receive (data,s,d,executor)) sopn sprt esfs place true in

	let diffuse media opr_dst opn prt place =
	  let eefs = eefs opn in
	  match place with
	  | true -> (match opn.t_opn_class with
	    | Communication (Send (_,opr_src,oprs)) ->
		let receivers = oprs@[opr_dst] in
		Hashtbl.remove graph (identifier_of_operation opn);
		(* Changing receiver list in other receive operations *)
		List.iter (function opn_rcv ->
		  let rcv_id = identifier_of_operation opn_rcv in
		  let opn_rcv = Hashtbl.find graph rcv_id in
		  let executor = execution_operator_of_communication opn_rcv in
		  Hashtbl.remove graph rcv_id;
		  let comclass = Communication (Receive (data,opr_src,receivers,executor)) in
		  opn_rcv.t_opn_class <- comclass;
		  Hashtbl.add graph (identifier_of_operation opn_rcv) opn_rcv) opn.t_opn_successors;
		let comclass = Send (data,opr_src,receivers) in
		opn.t_opn_class <- Communication comclass;
		let opnname = name_of_comclass comclass in
		opn.t_opn_path <- [opnname];
		Hashtbl.add graph (identifier_of_operation opn) opn;
		recv opr_src media receivers opr_dst opn prt eefs place
	    | _ -> raise (Failure "Adequation.diffuse : error"))
	  | false -> opn,prt,eefs in

	let shortest_links opr =
	  let links o = Architecture.links !archilib !archiname o in
	  let dist ((_,_),(opr,_)) =
	    let opr_dst_ref = operator_of_operator_class operator_dst in
	    match opr with
	    | Operator o -> o.oprref_links2.(opr_dst_ref.oprref_id)
	    | Media m -> m.mdaref_links2.(opr_dst_ref.oprref_id) in
	  list_min_elements dist (links opr) 0 in

	let place_link lnk place sopn sprt eefs =
	  match lnk with
	  | (Media media,_)::[(((Operator operator) as onext),gateopr)] ->
	      let opn,prt,eefs = match Architecture.bustype media with
	      | SamMP,true -> diffuse media operator sopn sprt place
	      | Ram,_ -> read media operator sopn sprt eefs place
	      | _ -> raise (Failure "Adequation.place_com_aux error 1") in
	      (opn,prt,eefs),onext
	  | (Operator operator1,_)::(Media media,_)::[(((Operator operator2) as onext),gateopr2)] ->
	      let opn,prt,eefs = match Architecture.bustype media with
	      | SamPP,_ | SamMP,_ ->
		  (*transfer operator1 media operator2 sopn sprt eefs place status*)
		  let opn,prt,eefs = send operator1 media operator2 sopn sprt eefs place in
		  recv operator1 media [operator2] operator2 opn prt eefs place
	      | Ram,_ ->
		  let opn,prt,eefs = write operator1 media sopn sprt eefs place in
		  read media operator2 opn prt eefs place in
	      (opn,prt,eefs),onext
	  | _ -> raise (Failure "Adequation.place_com_aux error 2") in
	let rec find_best_link lnks best_lnk best_eefs sopn sprt sopn_eefs =
	  match lnks with
	  | [] -> best_lnk
	  | ((((Media m) as mda),mg),(o,g))::_ -> (mda,mg)::[(o,g)] (* Just take the head : for operator choice, only their distance is taken into account as no additionnal operation has to be scheduled on them (on contrary to coms on media) *)
	  | ((((Operator opr) as o1),g1),(mda,mg))::rest -> let ((_,_),(o2,g2)) = List.hd (shortest_links mda) in (* See above for hd choice *)
	    let lnk = (o1,g1)::(mda,mg)::[(o2,g2)] in
	    (* don't really place it (boolean place is false) *)
	    let (_,_,eefs),_ = place_link lnk false sopn sprt sopn_eefs in
	    let best_lnk,best_eefs = match eefs < best_eefs with
	    | true -> lnk,eefs
	    | false -> best_lnk,best_eefs in
	    find_best_link rest best_lnk best_eefs sopn sprt sopn_eefs in
	
	let rec place_com_aux sopn sprt eefs place status opr_src = 
	  (* ps ("from "^(name_of_operator opr_src)^" to "^(name_of_operator operator_dst)^" placing "^(name_of_operation sopn)); *)
	  match opr_src = operator_dst with
	  | true -> sopn,sprt,eefs
	  | false -> 
    	      let lnks = shortest_links opr_src in
	      (* ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^" "^(name_of_operator opr)) "Possible next oprs are " lnks); *)
	      let  best_lnk = find_best_link lnks [] max_float sopn sprt eefs in
	      (* This time, place it if place is true *)
	      let (opn,prt,eefs),next_opr = place_link best_lnk place sopn sprt eefs in
	      (*  	    ps ("next opr "^name_of_operator next_opr); *)
	      place_com_aux opn prt eefs place status next_opr in
	
	let eefs = max t0 tcond in
	(*       ps ("placing com from "^(name_of_operator operator_src)); *)
	let opnpred,prtpred,eefs = place_com_aux operation_src port_src eefs place status operator_src in
	(match place with
	  (* Add dependence between last opn created and dopn (operation which required the communication) *)
	| true -> Adequationtypes.dependence_add opnpred prtpred operation_dst port_dst dpdclass cond status
	| false -> ());
	eefs in
      
      let sopn,sprt,dopn,dprt,dpdclass,cond = dpd.t_dpd_sopn,dpd.t_dpd_sprt,dpd.t_dpd_dopn,dpd.t_dpd_dprt,dpd.t_dpd_class,dpd.t_dpd_condition in
      
      debug_ps 3 debug_level ("place_communication..."^(name_of_data (sopn,sprt))^" -> "^(name_of_data (dopn,dprt)));
      
      let data,operator_dst = (sopn,sprt),(pi dopn) in
      
      (* Calculate list of operators containing data *)
      let operators_src_list = Hashtbl.find datas data in
      let operators_src_list = List.filter (function (_,opn,_,_) ->
	match conditioned_operation opn with
	| false -> true
	| true -> List.for_all (function c -> List.mem c dpd.t_dpd_condition) opn.t_opn_condition) operators_src_list in
      let f (o,_,_,_) = match o with
      | Media mda -> mda.mdaref_links2.((operator_of_operator_class operator_dst).oprref_id)
      | Operator opr -> opr.oprref_links2.((operator_of_operator_class operator_dst).oprref_id) in
      let operators_src_list = list_min_elements f operators_src_list 0 in
      (* operators_src_list : list of operators containing data and nearest of operator_dst *)
      let s = List.fold_left (function s -> function (operator_src,operation_src,port_src,t0) -> s^(name_of_operator operator_src)^" ") (" (looking for cond"^(string_of_condlist dpd.t_dpd_condition)^") ") operators_src_list in
      debug_ps 4 debug_level ((name_of_data data)^" on "^s);
      
      (* Not done anymore : Place the communication between each operator and the destination to determine the best route *)
      (*     let f e = place_com (data,e) dopn dprt operator_dst dpdclass cond false status in *)
      let best_operator_src = List.hd operators_src_list in (*((pi sopn),sopn,sprt,tcond)*)(*List.hd (list_min_elements f operators_src_list 0.)*)
      (* Place the communication *)
      ignore(place_com (data,best_operator_src) dopn dprt operator_dst dpdclass cond true status)

(** Returns eefs of dpd (eefs of dpd.sopn). *)
    let dpd_EEFS dpd =
      (* I'm not sure for memories... *)
      match (is_constant dpd.t_dpd_sopn) || (is_memory dpd.t_dpd_sopn) with
      | true -> 0.
      | false -> (*ps ("eefs dpd "^(name_of_operation dpd.t_dpd_sopn)^" is "^(string_of_float (eefs dpd.t_dpd_sopn)));*)
	  eefs dpd.t_dpd_sopn

(** Builds dpds, depending on their conditioning level, using place_com_fun. *)
    let make_cond_dpds place_com_fun dpds =
      let level dpd = match dpd.t_dpd_class with
      | (Condition l),_ -> l
      | _ -> 0 in
      (* Place conditioning dependences respecting order of conditioning level *)
      let rec dpdlevel dpds tc = match dpds with
      | [] -> tc
      | _ ->
	  let levelmin = list_min_value level dpds 0 in
	  let level_list, otherlevel_list = List.partition (function dpd -> level dpd = levelmin) dpds in
	  let tc = List.fold_left (function t -> function dpd -> max t (place_com_fun tc dpd;dpd_EEFS dpd)) tc level_list in
	  dpdlevel otherlevel_list tc in
      dpdlevel dpds 0.

(** Builds operation predecessors coms *)
    let make_coms operation graph status =
      let make_one_com tcond dpd =
	debug_ps 3 debug_level ("making com "^(string_of_dpd dpd));
	match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) || (pi dpd.t_dpd_sopn = pi operation) with
	| true -> ()
	| false -> place_communication graph status tcond dpd;
	    match status with
	    | true -> Adequationtypes.dependence_remove dpd
	    | false -> () in
      
      let dpds_data,dpds_condition = List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) operation.t_opn_dependences_predecessors in
      debug_ps 3 debug_level ((name_of_operation operation)^" coms are : "^(List.fold_left (fun s dpd -> s^", "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))) "" dpds_data));
      let tc = make_cond_dpds (make_one_com) dpds_condition in
      List.iter (make_one_com tc) dpds_data

(** Build memory successor coms *)
    let make_memory_succ_coms graph opn_eefs operation =
      let dpds_data,dpds_condition = List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) operation.t_opn_dependences_successors in
      ignore (make_cond_dpds (fun tc dpd -> match (pi operation = pi dpd.t_dpd_dopn) with
      | true -> ()
      | false -> place_communication graph true tc dpd) dpds_condition);
      List.iter (fun dpd -> match (pi operation = pi dpd.t_dpd_dopn) with
      | true -> ()
      | false -> Adequationtypes.dependence_remove dpd) dpds_condition;
      List.iter (fun dpd -> match (pi operation = pi dpd.t_dpd_dopn)  with
      | true -> ()
      | false -> place_communication graph true opn_eefs dpd;
	  Adequationtypes.dependence_remove dpd) dpds_data

(** Returns operation esfs on operator on which it has been scheduled (taking communication time into account).*)
    let esfs operation =
      let operator = pi operation in
      let delta = delta operation.t_opn_class operator in
      let tdpds = list_max_value dpd_EEFS operation.t_opn_dependences_predecessors 0. in
      debug_ps 1 debug_level ("Dpds esfs: "^(string_of_float tdpds));
      let tcond = esfs_cond operation operator tdpds operation.t_opn_condition in
      tcond

(** Returns the lefe of operation (average estimation)*)
    let rec lefe operation =
      let identifier = identifier_of_operation operation in
      match Hashtbl.mem lefe_table identifier with
      | true -> Hashtbl.find lefe_table identifier
      | false ->
	  (* I'm not sure for memories... *)
	  let succs,lefe_sub = match (is_memory operation) || (operation.t_opn_dependences_successors=[]) with
	  | true -> [],0.
	  | false ->
	      let successors = remove_copies (successors operation) in
	      list_max_elements_value lefe successors 0. in
	  let lefe = match succs with
	  | [] -> 0.
	  | _ -> lefe_sub +. (delta_average (List.hd succs).t_opn_class) in
	  Hashtbl.add lefe_table identifier lefe;
	  lefe

(** Schedules constants on all operators on which they will be needed (possible duplications) *)
    let place_constant graph progress_box =
      let rec place opn dpds = match dpds with
      | [] -> Adequationtypes.operation_remove graph opn
      | hd::tl ->
	  let opr = pi hd.t_dpd_dopn in
	  let dpds_opr_list,dpds_other_opr_list = List.partition (function {t_dpd_dopn=dopn} -> pi dopn = opr) dpds in
	  let newpath = (parentpath_of_operation opn)@[(name_of_operation opn)^"_"^(name_of_operator opr)] in
	  let newopn =
	    {opn with
	     t_opn_path = newpath;
	     t_opn_dependences_predecessors = [];
	     t_opn_dependences_successors = [];
	     t_opn_operator = Some opr;
	     t_opn_esfs = 0.;
	     t_opn_status = true} in
	  Hashtbl.add graph (identifier_of_operation newopn) newopn;
	  List.iter (function prt -> match (prt.t_prt_dir,prt.t_prt_class)  with
	  |	Out,Data_Port | Out,Init_Memory_Port -> update_datas opr (newopn,prt) newopn prt 0.
	  |	_,_ -> ()) newopn.t_opn_ports;
	  schedule newopn opr true 0. 0.;
	  List.iter (function dpd ->
	    let prt = List.find (function {t_prt_name=prtname;t_prt_dir=dir} -> prtname=dpd.t_dpd_sprt.t_prt_name && dir=Out) newopn.t_opn_ports in
	    Adequationtypes.dependence_add newopn prt dpd.t_dpd_dopn dpd.t_dpd_dprt dpd.t_dpd_class dpd.t_dpd_condition dpd.t_dpd_status) dpds_opr_list;
	  let opns = hashtbl_filter (function o -> (not (is_constant o)) && ((execution_operator_of_operation o)=(operator_of_operator_class opr))) graph in
	  List.iter (function o -> o.t_opn_condition <- List.map (function (v,vl) -> (match v with
	  | Some (opncond,prtcond) -> (match opncond=opn with
	    | true -> (Some (newopn,prtcond))
	    | false -> v)
	  | None -> v),vl) o.t_opn_condition) opns;
	  place opn dpds_other_opr_list in
      let constants = List.filter is_constant (list_of_hashtbl graph) in
      List.iter (function opn ->
	place opn opn.t_opn_dependences_successors;
	Progress_box.tick progress_box) constants

(** Returns the optimal esfs, schedule pressure and operator for operation. Fails if no operator is able to execute this operation *)
    let best_opr_esfs_sp operation operators graph =
      let opnlib,opnname = deflibname operation in
      let (opr_opt,esfs_opt,_) = 
	List.fold_left (fun (opr_m,esfs_m,spfastmin) operator -> 
  	  debug_ps 2 debug_level ("Trying "^(identifier_of_operation operation)^" on "^(name_of_operator operator));
	  match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
	  | true -> let delta = delta operation.t_opn_class operator in
	    schedule operation operator false 0. 0.;
	    create_gaps_backups ();
 	    make_coms operation graph false;
	    let esfs = esfs operation in
	    debug_ps 2 debug_level ("ESFS: "^(string_of_float esfs));
	    let spfast = esfs +. delta in
	    debug_ps 2 debug_level ("SPFast: "^(string_of_float spfast));
 	    delete_coms operation graph;
	    restore_gaps ();
	    deschedule operation operator false;
	    (match spfast < spfastmin with
	    | true -> (operator,esfs,spfast)
	    | false -> (opr_m,esfs_m,spfastmin))
	  | false -> (opr_m,esfs_m,spfastmin)) (List.hd operators,max_float,max_float) operators in
      match esfs_opt = max_float with
      | true -> failwith ("No operator able to execute :\nOperation : "^
			  (name_of_operation operation)^"\nDefinition : "^opnlib^"/"^opnname^
			  "\n\nMaybe you forgot to define a duration for this definition.\n")
      | false -> let sp = (lefe operation) +. esfs_opt +. (delta operation.t_opn_class opr_opt) in
	(*       ps ("For operation "^(name_of_operation operation)^" esfs "^(string_of_float esfs_opt)^" sp "^(string_of_float sp)); *)
	(opr_opt,(esfs_opt,sp))
	  
(** Returns true if operation has already been scheduled *)
    let is_scheduled operation = (is_constant operation) || (operation.t_opn_operator <> None)

(** Returns true if operation is schedulable *)
    let is_schedulable operation =
      match (is_scheduled operation) with
      | true -> false
      | false ->
	  let preds = match (is_memory operation) with
	  | true -> (predecessors operation)@(successors operation)
	  | false -> predecessors operation in
	  List.for_all (fun opn -> (is_scheduled opn) || (is_memory opn) || (is_constant opn)) preds

(** Updates the list of schedulables, considering that operation has just been scheduled *)
    let update_schedulables operation schedulables =
      schedulables := List.filter ((<>) operation) !schedulables;
      (*  let mems_schedulables = List.filter (fun opn -> (is_memory opn) && (is_schedulable opn)) (predecessors operation) in
	 let succs_schedulables = List.filter (fun opn -> (not (is_constant opn)) && (not (is_memory opn)) && (is_schedulable opn)) (successors operation) in
	 schedulables := !schedulables @ succs_schedulables @ mems_schedulables*)
      let new_schedulables = List.filter (fun opn -> (is_schedulable opn) && (not (is_constant opn))) ((successors operation)@(predecessors operation)) in
      schedulables := !schedulables@new_schedulables

(** Selects the best candidate (considering schedule pressure) among operations_operatoropt *)
    let find_best_candidate operations_operatoropt =
      let (opn_min,(opr_min,(esfs_min,_))) =
	(* Find the candidate with minimum esfs and, if equalities, minimum eefs *)
	List.hd (list_min_elements (fun (opn,(opr,(esfs,_))) ->
	  let eefs = esfs +. (delta opn.t_opn_class opr) in
	  (esfs,eefs))
		   operations_operatoropt (0.,0.)) in
      let eefs_min = esfs_min +. (delta opn_min.t_opn_class opr_min) in
      (* Select among candidates with esfs < eefs_min the candidate with maximum SP *)
      let (best_cand,_) = List.fold_left (fun (cand_max,sp_max) ((opn,(opr,(esfs,sp))) as cand) ->
	match (((esfs < eefs_min) || ((esfs = eefs_min) && 
				      ((delta opn.t_opn_class opr) = 0.))) && sp > sp_max) with
	| true -> (cand,sp)
	| false -> (cand_max,sp_max))
	  (List.hd operations_operatoropt,-1.) operations_operatoropt in
      best_cand

(** Bebug function to print the result of the adequation *)
    let print_adequation_result () =
      ps "Data :";
      Hashtbl.iter (fun data l -> let available_on = List.fold_left (fun s (operator,opn,prt,eefs) -> s^
	" and "^(name_of_operator operator)^
	" "^(name_of_data (opn,prt))) " on " l in
      ps ((name_of_data data)^available_on)) datas;
      ps "Schedules :";
      Hashtbl.iter (fun oprname (_,oprschedule) ->
	ps ("On opr "^oprname^"..........................................");
	let opns = Hashtbl.fold (fun _ cond_sched opns -> opns@(cond_sched.t_sch_operations)) oprschedule [] in
	let opns_sorted = List.sort (fun ({t_opn_rank=rnk1},_,_) ({t_opn_rank=rnk2},_,_) -> compare rnk1 rnk2) opns in
	List.iter (fun (opn,esfs,eefs) ->
	  ps ((name_of_operation opn)^" "^(string_of_float esfs)^"->"^(string_of_float eefs)))
	  opns_sorted) schedules

(** Computes the adequation of graph. Fails with various exception if something went wrong *)
    let adequation graph =
      let s = Hashtbl.fold
	  (fun _ opn s -> s^"\n"^(identifier_of_operation opn)^
	    (string_of_argsvalues opn.t_opn_arguments_values))
	  graph "Initial graph :" in
      debug_ps 1 debug_level s;
      architecture_init graph;
      Hashtbl.clear lefe_table;
      adequation_order := 0;
      let all_operators = Application.operators_list !archilib !archiname in
      let schedulables = ref (hashtbl_filter (fun o -> (is_schedulable o) && (not (is_constant o))) graph) in
      let total_adequation = hashtbl_length graph in

      let operators_number = List.length all_operators in

      let progress_box = Progress_box.create total_adequation in
      try
	while !schedulables <> [] do
   	  debug_ps 2 debug_level (List.fold_left (function s -> function opn -> s^(identifier_of_operation opn)^" ") "schedulables : " !schedulables);
          (* Calcul of the optimal operator for each operation *)
	  let operations_operatoropt = List.map (function opn -> opn,(best_opr_esfs_sp opn (operators_constraint opn) graph)) !schedulables in
	  let operation_optimal,(operator_optimal,(esfs_optimal,_)) = find_best_candidate operations_operatoropt in
	  schedule operation_optimal operator_optimal false 0. 0.;
	  let delta_opt = delta operation_optimal.t_opn_class operator_optimal in
(* 	      ps "MAKING COMS..."; *)
 	  make_coms operation_optimal graph true;
(* 	      ps "COMS DONE!"; *)
	  schedule operation_optimal operator_optimal true esfs_optimal (esfs_optimal+.delta_opt);
	  (* Data becoming available on operator *)
	  List.iter (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
	  | Out,_ -> debug_ps 4 debug_level ("Adding "^(name_of_data (operation_optimal,prt))^" on "^(name_of_operator operator_optimal)^"(with cond "^(string_of_condlist operation_optimal.t_opn_condition)^")");
	      update_datas operator_optimal (operation_optimal,prt) operation_optimal prt (eefs operation_optimal)
	  | _,_ -> ()) operation_optimal.t_opn_ports;
	  (* We need to place memory successor coms now (this is not needed to determine operation esfs). *)
	  (* Yet successor dependences will increase the total latency. So this should be taken into account (improve this) to choose best operator. *)
	  (match is_memory operation_optimal with
	  | true -> make_memory_succ_coms graph (eefs operation_optimal) operation_optimal
	  | false -> ());
  	  debug_ps 1 debug_level ("adequation place_calcul : "^(identifier_of_operation operation_optimal)^" on "^(name_of_operator operator_optimal)^" at "^(string_of_float esfs_optimal)^"..............ok");
          (* Update the list of schedulables operations *)
	  update_schedulables operation_optimal schedulables;
	  Progress_box.tick progress_box;
	done;
	place_constant graph progress_box;

	(* We need to have the list of operations sorted by esfs (for code generation) *)
	Hashtbl.iter (fun _ (_,oprschedule) ->
	  let opns = Hashtbl.fold (fun _ {t_sch_operations=cond_opns} opns -> opns@cond_opns) oprschedule [] in
	  let opns_sorted = List.stable_sort (function opn1,esfs1,eefs1 -> function opn2,esfs2,eefs2 -> match compare esfs1 esfs2 with
	  | 0 -> (match (esfs1 = eefs1) && (esfs2=eefs2) with (* two successive null duration operations *)
	    | true -> compare opn1.t_opn_adequation_order opn2.t_opn_adequation_order 
	    | false -> compare eefs1 eefs2)
	  | x -> x) opns in
	  ignore (List.fold_left (function i -> function opn,_,_ -> opn.t_opn_rank <- i; i+1) 0 opns_sorted)) schedules;
	
	(match debug_level > 0 with
	| true -> print_adequation_result ()
	| false -> ());
	graph,schedules
      with exn -> Progress_box.close progress_box;raise exn

(** Checks the adequation result. *)
    let check_schedule graph =
      (* Check that no operation is scheduled before one of its
	 predecessors (except if these are delays) *)
      Hashtbl.iter (fun _ opn -> List.iter (fun prec -> match prec.t_opn_class with
      | Communication (Receive ((o,_),_,_,_)) -> (match is_memory o with
	| true -> ()
	| false -> (match eefs prec <= opn.t_opn_esfs with
	  | true -> ()
	  | false -> failwith ((identifier_of_operation opn)^" predecessor "^(identifier_of_operation prec)^" schedule error")))
      | Communication _ -> ()
      | _ -> match is_memory prec with
	| true -> ()
	| false -> match eefs prec <= opn.t_opn_esfs with
	  | true -> ()
	  | false -> failwith ((identifier_of_operation opn)^" predecessor "^(identifier_of_operation prec)^" schedule error")) opn.t_opn_predecessors) graph;
      
      (* Check that all operations are scheduled *)
      Hashtbl.iter (fun _ opn -> match opn.t_opn_operator with
      | None -> failwith ((identifier_of_operation opn)^" is not scheduled on any operator.")
      | _ -> ()) graph;
      ps "Schedule ok"

(** Returns [p] if p is a calcul operation otherwise the full sender
   path to producer of communication p *)
    let rec transitive_predecessors p = 
      match p.t_opn_class with
      | Communication (Receive _) -> let send = List.hd (predecessors p) in
	send::(transitive_predecessors (List.hd (predecessors send)))
      | _ -> [p]

(** Returns [opn] if opn is a calcul operation otherwise the full
   sender paths to receivers of communication opn *)
    let rec transitive_successors opn =
      let succs = successors opn in
      match opn.t_opn_class with
      | Communication (Send _) ->
	  let succs = union (List.map successors (successors opn)) in
	  opn::(union (List.map transitive_successors succs))
      | _ -> [opn]

(** Returns condition in a prettier format, that is easier to use for
   display *)
    let pretty_conv_cond condition = match condition with
    | None, None -> "",1
    | (Some (opn,prt)),(Some vl) ->
	(identifier_of_operation opn)^"."^prt.t_prt_name,vl
    | _ -> raise (Failure "Adequation.pretty_conv_opn : error 1")

(** Returns condition list cl in a prettier format, that is easier to
   use for display *)
    let pretty_conv_cond_list cl = List.map pretty_conv_cond cl

(** Returns opn in a prettier format, that is easier to use for
   display *)
    let pretty_conv_opn opn =
      let rank = match opn.t_opn_class with
	(* Each communication takes 2 ranks, one for Receive & one for Send *)
      | Communication (Send _) -> opn.t_opn_rank/2
      | _ -> opn.t_opn_rank in
      let opn_id = identifier_of_operation opn in
      let opn_name = name_of_operation opn in
      let opn_cond_list = pretty_conv_cond_list opn.t_opn_condition in
      (opn_id, opn_name, opn.t_opn_esfs, eefs opn, opn_cond_list, rank)

(** Adds operation to schedules. Operation is added in either the
   constant list or other operations list of the schedule of the operator
   on which it is scheduled.*)
    let place_in_schedules schedules operation =
      match operation.t_opn_class,operation.t_opn_status with
      | Communication (Receive _),_ -> schedules
      | _,true ->
	  let operator = name_of_operator (pi operation) in
	  let constants,operations = List.assoc operator schedules in
	  let operator_schedule = match is_constant operation with
	  | true -> (pretty_conv_opn operation)::constants,operations
	  | false -> constants,(pretty_conv_opn operation)::operations in
	  (operator,operator_schedule)::(List.remove_assoc operator schedules)
      | _,false -> schedules

(** Returns adequation_graph in a prettier format, that is easier to
   use for display *)
    let pretty_conv_graph adequation_graph =
      let archilib,archiname = Application.archi_main_get () in
      let oprs = Architecture.operators_list archilib archiname in
      let mda = Architecture.media_list archilib archiname in
      let empty_schedules = List.map (function opr -> opr,([],[])) (oprs@mda) in
      remove_false_status adequation_graph;
      let opns = list_of_hashtbl adequation_graph in
      let pretty_schedules =
	List.fold_left place_in_schedules empty_schedules opns in
      let preds =
	List.fold_left
	  (fun preds opn ->
	    match opn.t_opn_class with
	    | Communication (Receive _) -> preds
	    | _ ->
		let preds_opn =
		  union (List.map transitive_predecessors (predecessors opn)) in
		let opn_id = identifier_of_operation opn in
		let preds_ids = List.map identifier_of_operation preds_opn in
		(opn_id,preds_ids)::preds)
	  [] opns in
      let succs =
	List.fold_left
	  (fun succs opn ->
	    match opn.t_opn_class with
	    | Communication (Receive _) -> succs
	    | Communication (Send _) ->
		(* Successor of send are receives, we want the successors
		   of those receives *)
		let recvs_successors =
		  union (List.map successors (successors opn)) in
		let send_succs =
		  union ((List.map transitive_successors) recvs_successors) in
		let send_succs_ids =
		  List.map identifier_of_operation send_succs in
		(identifier_of_operation opn,send_succs_ids)::succs
	    | _ ->
		let opn_id = identifier_of_operation opn in
		let opn_succs =
		  union (List.map transitive_successors (successors opn)) in
		let opn_succs_ids =
		  List.map identifier_of_operation opn_succs in
		(opn_id, opn_succs_ids)::succs)
	  [] opns in
      pretty_schedules,preds,succs   
  end
