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


(* This module contains functions used by both adequation heuristic and
falut-tolerance heuristic *)

(* WARNING : be cautious when changing this module. *)

(* There are very tough performance requirements, especially when
dealing with communications. *)

open Types
open Adequationtypes

let debug_level = 0

let name_of_operator o = match o with
| Operator opr -> opr.oprref_name
| Media mda -> mda.mdaref_name

let lefe_table = Hashtbl.create 30

let lefe_table_median = Hashtbl.create 30

let datas = Hashtbl.create 30

let schedules = Hashtbl.create 30

let xsc_constraints = Hashtbl.create 30

let archilib = ref ""

let archiname = ref ""

let adequation_order = ref 0

let name_of_og (o,g) = 
  (name_of_operator o)^(match g with | Some g -> "."^g.gte_name | _ -> "")

(*let psr r = match r with 
| [] -> () 
| hd::tl ->   
    ps (List.fold_left  
	  (function s -> function og -> s^"->"^(name_of_og og))  
	  (name_of_og hd) tl)  
 
let debug_constraints s = 
  ps s; 
  Hashtbl.iter
    (fun xsc oprs ->
      ps (xsc^" on "^
	  (List.fold_left
	     (function s -> function opr -> match opr with
	     | Operator o -> s^o.oprref_name^" "
	     | _ -> s)
	     "" oprs)))
    xsc_constraints *)

(* Returns operator on which opn has been scheduled. Fails if the
operation is not scheduled yet. *)
let pi opn = match opn.t_opn_operator with
| Some opr -> opr
| _ -> raise (Failure ("Adequation.pi : error ("^(name_of_operation opn)^")"))

(* Returns the list of operators compatible with constraints. All
operators if constraints are empty. *)
let operators_of_constraints constraints =
  let oprs_archi = Application.operators_list !archilib !archiname in
  match constraints with
  | (_,oprlist)::_ ->
      (match List.filter
	  (fun ((alib,aname),_) -> alib = !archilib && aname = !archiname)
	  oprlist with
      | [] -> oprs_archi
      | oprlist ->
	  let oprs_name = Architecture.operators_list !archilib !archiname in
	  let oprlist =
	    List.filter
	      (fun (_,oprname) -> List.mem oprname oprs_name)
	      oprlist in
	  List.map
	    (fun (_,oprname) ->
	      Operator (
	      Architecture.operator_reference !archilib !archiname oprname))
	    oprlist)
  | _ -> oprs_archi

(* Returns operators on which ref has been constrained (for old
versions, direct constraints) *)
let constraints reference_path =
  let constraints =
    List.filter
      (function refpath,_ -> refpath=reference_path)
      Application.application.app_constraints in
  operators_of_constraints constraints

(* Returns the list of operators on which xscname is constrained *)
let absoluteconstraints xscname =
  let constraints =
    List.filter
      (function xsc,_ -> xsc=xscname)
      Application.application.app_xsc_absolute_constraints in
  operators_of_constraints constraints

(* Returns the list of operators on which operation is constrained *)
let operators_constraint operation =
  let xsc = operation.t_opn_xsc_name in
  match xsc with
  | "" -> (*let path = path operation in
	     (match constraints path with
	     | [] -> *)Application.operators_list !archilib !archiname
	(*        | oprlist -> oprlist) *)
  | _ ->
      try
	match Hashtbl.find xsc_constraints xsc with
	| [] ->
	    raise (Failure ("Constraints : impossible to find an operator for "^
			    (identifier_of_operation operation)))
	| oprs ->
	    oprs
      with Not_found -> failwith "Adequation.operators_constraint error"
	  
let rec relative_constraints_update () =
  ()

let operators_constraints_init graph =
  Hashtbl.clear xsc_constraints;
  let init_absolute xsc =
    let refs = List.map fst (Application.refs_of_xsc xsc) in
    let constraints = match refs with
    | [] -> Application.operators_list !archilib !archiname
    (* if no constraint, this returns all operators in archi*)
    | _ -> intersections (List.map constraints refs) in
    let absolute_constraints = absoluteconstraints xsc in
    let oprs_absolute = intersection constraints absolute_constraints in
    Hashtbl.add xsc_constraints xsc oprs_absolute in
  List.iter init_absolute (Application.xsc_namelist ());
  relative_constraints_update ()

(* Returns operators on which a duration for operation has been defined *)
let operators_able_to_execute operation =
  let opnlib,opnname = deflibname operation in
  let oprs =
    Architecture.operators_able_to_execute !archilib !archiname opnlib opnname in
  match oprs with
  | [] ->
      raise (Failure (
	     "No operator able to execute :\nOperation : "^
	     (name_of_operation operation)^"\nDefinition : "^opnlib^"/"^opnname^
	     "\n\nMaybe you forgot to define a duration for this definition.\n"))
  | _ -> List.map (function opr -> Operator opr) oprs

(* Returns a new schedule gaps list. This list is obtained by deleting
interval [esfs;esfs+delta] in gaps.*)
let remove_gap esfs eefs gaps =
  (*  ps ("Removing gaps "^(string_of_gaps_list gaps)^" with esfs "^
     (string_of_float esfs)^" and eefs "^(string_of_float eefs));*)
  match eefs-.esfs with
  | 0. -> gaps
  | _ -> 
      (* Tests are surely far from optimal, but I think more readable *)
      let rec f oldgaps processed =
	match oldgaps with
	| [] ->
          (* may happen when updating the gap list of a schedule
	  different from the schedule in which the operation is added
	  (ie for schedule of condition non-exclusive with operation
	  added) *)
	    processed
	| (inf,sup)::rest -> 
	    match esfs >= sup with
	    | true -> processed@oldgaps
	    | false ->
		match (esfs <= sup && esfs >= inf) ||
		(eefs <= sup && eefs >= inf) with
		| true ->
		    (match esfs <= inf && eefs >= sup with
		    | true -> f rest processed (* deleting gap *)
		    | false -> match esfs > inf && eefs < sup with
		      | true ->
                          (* two smaller gaps surrounding
                             [esfs;eefs]. Notice that gaps list will be in
                             decreasing order, which is more optimal than in
                             increasing order. *)
			  f rest (processed@[(eefs,sup);(inf,esfs)]) 
		      | false -> match esfs <= inf && eefs <= sup with
			| true ->
                        (* shortening gap at the left *)
			    f rest (processed@[(eefs,sup)])
			| false -> match esfs <= sup && sup <= eefs with
			  | true ->
                          (* shortening gap at the right *)
			      f rest (processed@[(inf,esfs)])
			  | false ->
                          (* no intersection with this gap, keep it *)
			      f rest (processed@[(inf,sup)]))
		| false ->
                (* no intersection with this gap, keep it *)
		    f rest (processed@[(inf,sup)]) in
      f gaps []
	
(* Function not used anymore, I keep it just in case... *)
(*let restore_gap esfs eefs gaps =
(*   ps ("Restoring gaps "^(string_of_gaps_list gaps)^" with esfs "^
   (string_of_float esfs)^" and eefs "^(string_of_float eefs)); *)
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

(* Creates a backup for the gaps lists on all media *)
let create_gaps_backups () =
  Hashtbl.iter
    (fun _ (oprclass,oprschedule) -> match oprclass with
    | Operator _ -> ()
    | Media _ ->
	Hashtbl.iter
	  (fun _ condschedule -> 
	    condschedule.t_sch_gaps_backup <- condschedule.t_sch_gaps)
	  oprschedule)
    schedules

(* Restores the gaps lists on all media from their backups *)
let restore_gaps () =
  Hashtbl.iter
    (fun _ (oprclass,oprschedule) -> match oprclass with
    | Operator _ -> ()
    | Media _ ->
	Hashtbl.iter
	  (fun _ condschedule ->
	    condschedule.t_sch_gaps <- condschedule.t_sch_gaps_backup)
	  oprschedule)
    schedules

(* Returns the gaps list for condition cond, considering existing
schedule operschedule. *)
let initial_gaps oprschedule cond =
  (*   ps ("Initialising gaps for cond "^(string_of_condlist cond)); *)
  let gaps = ref [(0.,max_float)] in
  (* There's probably a more optimal way to do this *)
  Hashtbl.iter 
    (fun c condschedule -> match exclusion_condition c cond with
    | true -> ()
    | false ->
	List.iter
	  (fun (opn,esfs,eefs) -> gaps := remove_gap esfs eefs !gaps)
	  condschedule.t_sch_operations) oprschedule;
  (*     ps ("Gaps initialised are now:"^(string_of_gaps_list !gaps)); *)
  !gaps

(* Adds data produced by opn as an available data on operator after
time eefs *)
let update_datas operator data opn prt eefs =
  let newoprs =
    (operator,opn,prt,eefs)::
    (try Hashtbl.find datas data with Not_found -> []) in
  Hashtbl.replace datas data newoprs
    
(* Calculates routing tables of all operators and media *)
let route oprlist mdalist =
  (* Count Oprs, initialize their id, allocate and initialize their
  routings: *)
  let nboprs =
    List.fold_left
      (fun maxid opr -> opr.oprref_id <- maxid;maxid+1)
      0 oprlist in
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
      match opr_cnc.oprref_links2.(dst.oprref_id) == !dist with
      | false -> process_oprs_connected_opr rest dst mda
      | true -> mda.mdaref_links2.(dst.oprref_id) <- !dist + 1;
	  maxdist := !dist + 1 in
  let rec process_oprs_connected_mda oprs dst opr =
    match oprs with
    | [] -> ()
    | ((_,_),(mda_cnc,_))::rest -> let mda_cnc = match mda_cnc with
      | Media mda -> mda
      | _ -> failwith "Adequation.route" in
      match mda_cnc.mdaref_links2.(dst.oprref_id) == !dist with
      | false -> process_oprs_connected_mda rest dst opr
      | true -> opr.oprref_links2.(dst.oprref_id) <- !dist + 1;
	  maxdist := !dist + 1 in
  while dist = maxdist do
    (* Media routing tables *)
    List.iter
      (fun mda ->
	let oprs_connected =
	  Architecture.links archilib archiname (Media mda) in
	List.iter
	  (fun dst -> 
	    match mda.mdaref_links2.(dst.oprref_id) < 0 with
	    | false -> ()
	    | true -> process_oprs_connected_opr oprs_connected dst mda)
	  oprlist)
      mdalist;
    (* Operators routing tables *)
    List.iter
      (fun opr ->
	let mda_connected =
	  Architecture.links archilib archiname (Operator opr) in
	List.iter
	  (fun dst -> 
	    match opr.oprref_links2.(dst.oprref_id) < 0 with
	    | false -> ()
	    | true -> process_oprs_connected_mda mda_connected dst opr)
	  oprlist)
      oprlist;
    dist := !dist + 1
  done
    (*;
   ps "media";
   ps (List.fold_left
   (fun s opr -> s^opr.oprref_name^" : "^(string_of_int opr.oprref_id)^", ")
   "Ids : " oprlist);
   List.iter
   (fun mda ->
   ps (fst (Array.fold_left
   (fun (s,id) dist -> ((s^"; "^(string_of_int id)^":"^(string_of_int dist)),id+1))
   (mda.mdaref_name,0) mda.mdaref_links2)))
   mdalist;
   ps "operator";
   List.iter
   (fun opr ->
   ps (fst (Array.fold_left
   (fun (s,id) dist -> ((s^"; "^(string_of_int id)^":"^(string_of_int dist)),id+1))
   (opr.oprref_name,0) opr.oprref_links2)))
   oprlist*)

(* Initializes the route table, data available on operators,
   schedules and operator_constraints *)
let architecture_init graph =
  let alib,aname = Application.archi_main_get () in
  Hashtbl.clear datas;
  Hashtbl.clear schedules;
  archilib := alib;
  archiname := aname;
  let oprlist =
    List.map
      (Architecture.operator_reference !archilib !archiname )
      (Architecture.operators_list !archilib !archiname) in
  let mdalist =
    List.map
      (Architecture.media_reference !archilib !archiname)
      (Architecture.media_list !archilib !archiname) in
  List.iter
    (fun o ->
      Hashtbl.add schedules o.oprref_name ((Operator o),Hashtbl.create 10))
    oprlist;
  List.iter
    (fun m ->
      Hashtbl.add schedules m.mdaref_name ((Media m),Hashtbl.create 10))
    mdalist;
  route oprlist mdalist;
  operators_constraints_init graph


(* Schedules operation on operator at time esfs. *)
(* Notice that a call to this function with status true only happens
just after a call with status false *)
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
    cond_schedule.t_sch_operations,cond_schedule.t_sch_last_opn,
    cond_schedule.t_sch_gaps,cond_schedule.t_sch_gaps_backup
  with Not_found -> ([],None,[],[]) in
  let gaps = match operator,gaps,status with
  | (Media _,[],true) -> initial_gaps oprschedule operation.t_opn_condition
  | _ -> gaps in
  (match status with
  | false -> operation.t_opn_pred_scheduled <- lastopn
  | true  -> () (* previously done with a status false *));
  let newschedule = (match status with 
  | true ->
  (* reverse order again to find it faster at deschedule *) 
       (*match status with 
       | true -> debug_ps 0 debug_level ("\n add to schedule "^(name_of_operation operation)^" into "^(name_of_operator operator)
			      ^"  at  "^(string_of_float operation.t_opn_esfs)^"  ->  "^(string_of_float eefs)^"\n"); 
       | false -> ());*)
      (operation,esfs,eefs)::oldschedule
  | false -> oldschedule) in
  Hashtbl.replace oprschedule operation.t_opn_condition
    ({t_sch_operations=newschedule;
      t_sch_last_opn=Some operation;
      t_sch_gaps=gaps;t_sch_gaps_backup=gapsbackup});
  match operator,status with
  | (Media _,true) ->
      Hashtbl.iter
	(fun cond schedule ->
	  match exclusion_condition cond operation.t_opn_condition with
	  | true -> ()
	  | false ->
	      schedule.t_sch_gaps <- (remove_gap esfs eefs schedule.t_sch_gaps))
	oprschedule
  | _ -> ()
	
(*  *)
let deschedule operation operator status =
  (*   ps ("Descheduling "^(name_of_operation operation)^" from ("^
     (string_of_float operation.t_opn_esfs)^" to "^
     (string_of_float (eefs operation))^" cond was "^
     (string_of_condlist operation.t_opn_condition)); *)
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
  let oldschedule,gaps,gapsbackup = 
    let cond_schedule = Hashtbl.find oprschedule operation.t_opn_condition in
    cond_schedule.t_sch_operations,
    cond_schedule.t_sch_gaps,
    cond_schedule.t_sch_gaps_backup in
  let newschedule = match status with
  | true -> let rec filter_opn processed todo =
      match todo with
      | [] -> processed (* Yet, this should never happen *)
      | ((opn,_,_) as o)::rest -> match opn =$ operation with
	| true -> processed@rest
	| false -> filter_opn (processed@[o]) rest in
    filter_opn [] oldschedule
  | false -> oldschedule in
  (*     ps (List.fold_left (fun s (opn,_,_) -> s^";"^
     (name_of_operation opn)) "New schedule: " newschedule); *)
  Hashtbl.replace oprschedule operation.t_opn_condition
    {t_sch_operations=newschedule;
     t_sch_last_opn=operation.t_opn_pred_scheduled;
     t_sch_gaps=gaps;t_sch_gaps_backup=gapsbackup};
  operation.t_opn_pred_scheduled <- None;
  operation.t_opn_operator <- None;
  operation.t_opn_esfs <- 0.




 (* Deletes the communication corresponding to dpd. Deschedules all its
communication operations. *)
let rec delete_communication graph opr_dst dpd =
  (*   ps ("deleting com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^
     " to "^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))); *)
  let sopn = dpd.t_dpd_sopn in
  match (is_communication sopn) with
  | true -> let successor = match (is_communication dpd.t_dpd_dopn) with
    | true ->
    (* This com has previously been deleted, we must yet count it in the
    successors *)
	1
    | false -> 0 in
    (match (((List.length sopn.t_opn_dependences_successors) + successor) > 1) with
    | true ->
        (* Diffuse (send) data for several receives *)
	let sopn = dpd.t_dpd_sopn in
	let data = data_of_communication sopn in
	(match sopn.t_opn_class with
	| Communication (Send (_,opr_src,oprs)) ->
	    Hashtbl.remove graph (identifier_of_operation sopn);
	    let receivers = List.filter (fun opr -> opr <>@@ opr_dst) oprs in
	    (* Changing receiver list in other receive operations *)
	    (* Should change this. Won't work if two identical receive
	    exist (with different operation number #0 #1 etc). This may
	    happen with conditionning *)
	    List.iter
	      (fun opn_rcv ->
		let rcv_id = identifier_of_operation opn_rcv in
		let opn_rcv = Hashtbl.find graph rcv_id in
		let executor = execution_operator_of_communication opn_rcv in
		Hashtbl.remove graph rcv_id;
		let comclass =
		  Communication (Receive (data,opr_src,receivers,executor)) in
		opn_rcv.t_opn_class <- comclass;
		Hashtbl.add graph
		(identifier_of_operation opn_rcv)
		opn_rcv)
	      sopn.t_opn_successors;
	    (* Changing sopn receiver list and that's all. No recursive
	    deletion as predecessors are used by other operations *)
	    let comclass = Send (data,opr_src,receivers) in
	    sopn.t_opn_class <- Communication comclass;
	    let opnname = name_of_comclass comclass in
	    sopn.t_opn_path <- [opnname];
	    Hashtbl.add graph (identifier_of_operation sopn) sopn
	| _ ->
         (* For instance : Receive having several successors on the same
         operator (reusing a com, nothing to delete except the
         dependence)*)
	    dependence_remove dpd)
    | false ->  
	(*ps ("deleting"^(name_of_operation sopn)); *)
        (* Need to store dpds (which will be deleted by Hashtbl.remove)
        to enable terminal recursivity *)
	let dpds = sopn.t_opn_dependences_predecessors in
	let opr_dst = match sopn.t_opn_class with
	| Communication (Send (_,sopr,_)) -> sopr
	| _ -> opr_dst in
	deschedule sopn (pi sopn) true;
	Adequationtypes.operation_remove graph sopn;
 	List.iter (delete_communication graph opr_dst) dpds)
  | false -> ()

(* Deletes all predecessors coms of operation *)
let delete_coms operation graph =
  let delete_one_com dpd =
    match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) ||
    (pi dpd.t_dpd_sopn =@ pi operation) with
    | true -> ()
    | false -> 
	(*ps ("delete com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^
	   " to "^(name_of_operation operation)^" (on "^
	   (name_of_operator (pi operation)));*)
	let opr_dst = operator_of_operator_class (pi operation) in        
	delete_communication graph opr_dst dpd in 
  List.iter delete_one_com operation.t_opn_dependences_predecessors 

(* Returns esfs (of any operation) on operator after time t0 with
condition cond. in the presence of faults *)
let fault_esfs_cond operation operator t0 w_t0 cond =
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
    match operator with
      | Operator o -> 
	  let topr,w_topr =
	    (*Notice that this calculation assumes that there is no gap in the schedule *)
	    (*Hashtbl.fold
	      (fun cd {t_sch_last_opn=lastopn} (esfs,wesfs) ->
	      let lastopn =
	      match cd = cond with
	      | true ->
	    (* we just scheduled current operation with this
	      condition, so we must not take it into account *)
	      operation.t_opn_pred_scheduled
	      | false -> lastopn in                   
	      match lastopn with 
	      | None -> (esfs,wesfs)
	      | Some opn ->  
	      match exclusion_condition opn.t_opn_condition cond with 
	      | true  -> (esfs,wesfs)
	      | false -> (max esfs (eefs opn)),(max wesfs opn.t_opn_weefs)) 
	      oprschedule (0.,0.) in *)
            (*ps ("\ncompute esfs ready operator "^(name_of_operator operator)^" for "^(name_of_operation operation));*)
	    Hashtbl.fold   
	      (fun cd {t_sch_last_opn=lastopn;t_sch_operations=opns} (best_esfs,best_wesfs) ->
		 let new_esfs,new_wesfs = 
		   List.fold_left (fun (tmp_esfs,tmp_wesfs) (opn,esfs,eefs)  ->
				     (*ps ("             operation "^(name_of_operation opn)^" eefs = "^(string_of_float eefs));*)
				     (max tmp_esfs eefs),(max tmp_wesfs opn.t_opn_weefs)   
				  ) (best_esfs,best_wesfs) opns in
                   (max new_esfs best_esfs),(max new_wesfs best_wesfs) 
	      ) oprschedule (0.,0.) in
	    (*ps (" esfs ready operator between initial "^(string_of_float t0)^" and computed "^(string_of_float topr)
	           ^" is "^(string_of_float (max topr t0)));*)
	    (max topr t0),(max w_topr w_t0)
      | Media m ->          
	  let tmed, w_tmed =  
	    Hashtbl.fold   
	      (fun cd {t_sch_last_opn=lastopn;t_sch_operations=opns} (best_esfs,best_wesfs) ->
		 let new_esfs,new_wesfs = 
		   List.fold_left (fun (tmp_esfs,tmp_wesfs) (opn,esfs,eefs)  ->
  				     (max tmp_esfs eefs),(max tmp_wesfs opn.t_opn_weefs)  
				  ) (best_esfs,best_wesfs) opns in
                   (max new_esfs best_esfs),(max new_wesfs best_wesfs) 
	      ) oprschedule (0.,0.) in
	    (max tmed t0),(max w_tmed w_t0)  

(* Returns esfs (of any operation) on operator after time t0 with
condition cond. in the presence of faults *)
let rel_esfs_cond operation operator t0 cond =
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
    match operator with
      | Operator o -> 
	  let topr =
	    Hashtbl.fold   
	      (fun cd {t_sch_last_opn=lastopn;t_sch_operations=opns} best_esfs ->
		 let new_esfs = 
		   List.fold_left (fun tmp_esfs (opn,esfs,eefs)  ->
				     (max tmp_esfs eefs)) best_esfs opns in
                   (max new_esfs best_esfs)
	      ) oprschedule 0. in
	    (*ps (" esfs ready operator between initial "^(string_of_float t0)^" and computed "^(string_of_float topr)
	           ^" is "^(string_of_float (max topr t0)));*)
	    (max topr t0) 
      | Media m ->  let topr =
	    Hashtbl.fold   
	      (fun cd {t_sch_last_opn=lastopn;t_sch_operations=opns} best_esfs ->
		 let new_esfs = 
		   List.fold_left (fun tmp_esfs (opn,esfs,eefs)  ->
				     (max tmp_esfs eefs)) best_esfs opns in
                   (max new_esfs best_esfs)
	      ) oprschedule 0. in (max topr t0) 


(* Returns esfs (of any operation) on operator after time t0 with
condition cond. *)
let esfs_cond operation operator t0 cond =
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
  match operator with
  | Operator o ->
      let topr =
    (*Notice that this calculation assumes that there is no gap in the schedule *)
	Hashtbl.fold
	  (fun cd {t_sch_last_opn=lastopn} esfs ->
	    let lastopn =
	      match cd = cond with
	      | true ->
              (* we just scheduled current operation with this
              condition, so we must not take it into account *)
		  operation.t_opn_pred_scheduled
	      | false -> lastopn in
	    match lastopn with
	    | None -> esfs
	    | Some opn ->
		match exclusion_condition opn.t_opn_condition cond with
		| true -> esfs
		| false -> max esfs (eefs opn))
	  oprschedule 0. in
      max topr t0
  | Media m ->
      (* this calculation enables to use gaps in the medium schedule *)
      let delta_opn = delta operation.t_opn_class operator in
      let rec gap_start l tmin =
	match l with
	| [] -> tmin
	| (inf,sup)::rest ->
	    let tmin =
	      match ((t0+.delta_opn)<=sup && (inf+.delta_opn)<=sup) with
	      | true -> 
		   (*ps ("Interval ["^(string_of_float inf)^";"^
		     (string_of_float sup)^"] ok for t0 = "^
		     (string_of_float t0)^" with condition "^
		     (string_of_condlist operation.t_opn_condition));  *)
	      max t0 inf (* we can use this gap *)
	  | false -> tmin in
	  match inf <= t0 with
	  | true -> tmin (* remember that the gap list is in decreasing order *)
	  | false -> gap_start rest tmin in
      let gaps =
	(Hashtbl.find oprschedule operation.t_opn_condition).t_sch_gaps in
      let gaps = match gaps with
      | [] -> initial_gaps oprschedule operation.t_opn_condition
      | _ -> gaps in
      gap_start gaps max_float
	
(* Place the operation of communication opncom on the media *)
let place_com_media media opncom opnpred prtpred eefspred place receive graph
    cond status dpdclass data fault =
  (*ps("place_com_media "^(name_of_operator (Media media))^" "^(name_of_operation opnpred));*)
  let portdim = prtpred.t_prt_dim in
  let portin = match prtpred.t_prt_class with
  | Port.Precedence_Port -> List.hd (precedence_ports ())
  | _ ->
      {t_prt_name="in";
       t_prt_dir=Port.In;
       t_prt_typename=prtpred.t_prt_typename;
       t_prt_dim=portdim;
       t_prt_class=prtpred.t_prt_class;
       t_prt_order=1}
  and portout = match prtpred.t_prt_class with
  | Port.Precedence_Port -> List.nth (precedence_ports ()) 1
  | _ ->
      {t_prt_name="out";
       t_prt_dir=Port.Out;
       t_prt_typename=prtpred.t_prt_typename;
       t_prt_dim=portdim;
       t_prt_class=prtpred.t_prt_class;
       t_prt_order=1} in
  
  let opnname =
    unique_identifier graph [] (name_of_comclass opncom) in
  let ports =
    match prtpred.t_prt_class with
    | Port.Precedence_Port -> [portin;portout]
    | _ -> [portin;portout]@(precedence_ports ()) in
  let opn =
    new_opn [opnname] (Communication opncom) [] ports [] [] cond
      (Some (Media media)) 0 0. status (Ihm [opnname]) None
      "" "" "" [InitSeq;LoopSeq;EndSeq] in
  let delta = delta (Communication opncom) (Media media) in
  schedule opn (Media media) false 0. 0.;
   (*ps ("++++++++++++++++++old start com = "^(string_of_float eefspred));*)
  let esfs,wesfs = 
    match receive with 
  (* This trick of using a boolean was first introduce to place several receives
     just after the same send (diffuse), but is now used for all receives.
     This avoids useless comutations and some misbehaviours.
     However, it won't work any longer if
     (delta receive <> 0) or if esfs doesn't rely for media
     on the gaps calculation 
     (otherwise, things done in function schedule are not correct anymore) *)
  | true  -> eefspred, opnpred.t_opn_weefs
  | false -> 
      match fault with 
              | false -> let esfs = esfs_cond opn (Media media) eefspred cond in
                           esfs,esfs
              | true  -> let esfs,wesfs = fault_esfs_cond opn (Media media) eefspred opnpred.t_opn_weefs cond in
                           esfs,wesfs in

  let eefs,weefs = (esfs +. delta),(wesfs +. delta) in

    (*ps ("++++++++++++++++++end com = "^(string_of_float esfs)^" -- "^(string_of_float eefs));*)

   (*opn.t_opn_weefs <- max (eefs) (opnpred.t_opn_weefs+.delta);*)
   opn.t_opn_weefs <- weefs;

  (match place with
  | true ->
      debug_ps 3 debug_level
	("Placing com operation "^(name_of_operation opn)^" on "^
	 (name_of_operator (Media media))^" from "^(string_of_float esfs)^
	 " to "^(string_of_float eefs)^" condition "^
	 (string_of_condlist opn.t_opn_condition));
      schedule opn (Media media) true esfs eefs
  | false -> ());
  let opn,portout = match place with
  | true ->
      Hashtbl.add graph (identifier_of_operation opn) opn;
      Adequationtypes.dependence_add opnpred prtpred opn portin dpdclass cond status;
      (* As routes don't cross, updating data and schedules is not
      needed if status is false. *)
      (match status with
      | true -> 
	  (* Update list of data *)
      	  (match opncom with
	  | Send (_,_,oprs) ->
	      (match Architecture.bustype media with
	      | SamMP,true -> update_datas (Media media) data opn portout eefs
	      | _ -> ())
	  | Reliable_Send (_,_,oprs) ->
	      (match Architecture.bustype media with
	      | SamMP,true -> update_datas (Media media) data opn portout eefs
	      | _ -> ())
	  | Receive (_,_,_,executor) ->
	      update_datas (Operator executor) data opn portout eefs
	  | Reliable_Receive (_,_,_,executor) ->
	      update_datas (Operator executor) data opn portout eefs
	  | Write _ -> update_datas (Media media) data opn portout eefs
	  | Read (_,opr) -> update_datas (Operator opr) data opn portout eefs
	  | _ -> raise (Failure "Adequation.place_com_media error"));
      | false -> ());
      opn,portout
  | false -> deschedule opn (Media media) false;
      opnpred,prtpred in
  opn,portout,eefs

let read s d sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media s (Read (data,d)) sopn sprt esfs place false
    graph cond status dpdclass data fault

let write s d sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media d (Write (data,s)) sopn sprt esfs place false
    graph cond status dpdclass data  fault

let transfer s sam d sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media sam (Transfer (data,s,[d])) sopn sprt esfs place false
    graph cond status dpdclass data fault 

let send s media d sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media media (Send (data,s,[d])) sopn sprt esfs place false
    graph cond status dpdclass data  fault

let recv s media d executor sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media media (Receive (data,s,d,executor)) sopn sprt esfs place true
    graph cond status dpdclass data fault  

let reliable_send s media d sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media media (Reliable_Send (data,s,[d])) sopn sprt esfs place false
    graph cond status dpdclass data  fault

let reliable_recv s media d executor sopn sprt esfs place data graph cond status dpdclass fault =
  place_com_media media (Reliable_Receive (data,s,d,executor)) sopn sprt esfs place true
    graph cond status dpdclass data fault  

let diffuse media opr_dst opn prt place  data graph cond status dpdclass fault =
  let eefs = eefs opn in
  match place with
  | true -> (match opn.t_opn_class with
    | Communication (Send (_,opr_src,oprs)) ->
	let receivers = oprs@[opr_dst] in
	Hashtbl.remove graph (identifier_of_operation opn);
	(* Changing receiver list in other receive operations *)
	List.iter
	  (function opn_rcv ->
	    let rcv_id = identifier_of_operation opn_rcv in
	    let opn_rcv = Hashtbl.find graph rcv_id in
	    let executor = execution_operator_of_communication opn_rcv in
	    Hashtbl.remove graph rcv_id;
	    let comclass =
	      Communication (Receive (data,opr_src,receivers,executor)) in
	    opn_rcv.t_opn_class <- comclass;
	    Hashtbl.add graph (identifier_of_operation opn_rcv) opn_rcv)
	  opn.t_opn_successors;
	let comclass = Send (data,opr_src,receivers) in
	opn.t_opn_class <- Communication comclass;
	let opnname = name_of_comclass comclass in
	opn.t_opn_path <- [opnname];
	Hashtbl.add graph (identifier_of_operation opn) opn;
	recv opr_src media receivers opr_dst opn prt eefs place data
	  graph cond status dpdclass fault
    | Communication (Reliable_Send (_,opr_src,oprs)) ->
	let receivers = oprs@[opr_dst] in
	Hashtbl.remove graph (identifier_of_operation opn);
	(* Changing receiver list in other receive operations *)
	List.iter
	  (function opn_rcv ->
	    let rcv_id = identifier_of_operation opn_rcv in
	    let opn_rcv = Hashtbl.find graph rcv_id in
	    let executor = execution_operator_of_communication opn_rcv in
	    Hashtbl.remove graph rcv_id;
	    let comclass =
	      Communication (Reliable_Receive (data,opr_src,receivers,executor)) in
	    opn_rcv.t_opn_class <- comclass;
	    Hashtbl.add graph (identifier_of_operation opn_rcv) opn_rcv)
	  opn.t_opn_successors;
	let comclass = Reliable_Send (data,opr_src,receivers) in
	opn.t_opn_class <- Communication comclass;
	let opnname = name_of_comclass comclass in
	opn.t_opn_path <- [opnname];
	Hashtbl.add graph (identifier_of_operation opn) opn;
	reliable_recv opr_src media receivers opr_dst opn prt eefs place data
	  graph cond status dpdclass fault
    | _ -> raise (Failure "Adequation.diffuse : error"))
  | false -> opn,prt,eefs


let update_senders sender opn graph=
  match opn.t_opn_class with
    | Communication (Reliable_Send (data,senders,receivers)) ->
       (match (List.mem sender senders) with
	| false -> (*ps (List.fold_left (fun s opr -> 
          s^" , "^(name_of_operator (Operator opr)))"old senders =" senders);*)
	let new_senders = senders@[sender] in 
        (*ps (List.fold_left (fun s opr ->  
          s^" , "^(name_of_operator (Operator opr))) "new senders =" new_senders);*)
	  Hashtbl.remove graph (identifier_of_operation opn); 
	  let comclass = Reliable_Send (data,new_senders,receivers) in
	    opn.t_opn_class <- Communication comclass;
	    let opnname = name_of_comclass comclass in
	      opn.t_opn_path <- [opnname];
	      Hashtbl.add graph (identifier_of_operation opn) opn
	| true -> ())
    | _ -> raise (Failure "Fault_Tolerance_Adequation.update_senders : error")


let shortest_links opr operator_dst =
  let links o = Architecture.links !archilib !archiname o in
  let dist ((_,_),(opr,_)) =
    let opr_dst_ref = operator_of_operator_class operator_dst in
    match opr with
    | Operator o -> o.oprref_links2.(opr_dst_ref.oprref_id)
    | Media m -> m.mdaref_links2.(opr_dst_ref.oprref_id) in
  list_min_elements dist (links opr) 0 

let place_link lnk place sopn sprt eefs data graph cond status dpdclass fault =
  match lnk with
  | (Media media,_)::[(((Operator operator) as onext),gateopr)] ->
      let opn,prt,eefs =
	match Architecture.bustype media with
	| SamMP,true ->
	    diffuse media operator sopn sprt place data graph cond status dpdclass fault
	| Ram,_ ->
	    read media operator sopn sprt eefs place data graph cond status dpdclass fault
	| _ -> raise (Failure "Adequation.place_com_aux error 1") in
      (opn,prt,eefs),onext
  | (Operator operator1,_)::(Media media,_)::
    [(((Operator operator2) as onext),gateopr2)] ->
      let opn,prt,eefs = match Architecture.bustype media with
      | SamPP,_ | SamMP,_ ->
	  (*transfer operator1 media operator2 sopn sprt eefs place status*)
	  let opn,prt,eefs =
	    send operator1 media operator2 sopn sprt eefs place data graph
	      cond status dpdclass fault in
	    recv operator1 media [operator2] operator2 opn prt eefs place data
	    graph cond status dpdclass fault
      | Ram,_ ->
	  let opn,prt,eefs =
	    write operator1 media sopn sprt eefs place data graph cond
	      status dpdclass fault in
	  read media operator2 opn prt eefs place data graph cond
	    status dpdclass fault in
      (opn,prt,eefs),onext
  | _ -> raise (Failure "Adequation.place_com_aux error 2") 

let reliable_place_link lnk place sopn sprt eefs data graph cond status dpdclass =
  match lnk with
  | (Media media,_)::[(((Operator operator) as onext),gateopr)] ->
      let opn,prt,eefs =
	match Architecture.bustype media with
	| SamMP,true -> diffuse media operator sopn sprt place data graph cond status dpdclass true 
	| Ram,_ -> raise (Failure " ram media is not yet implemented : 
                        see adequation_core 'reliable_place_link' function ") 
	    (*read media operator sopn sprt eefs place data graph cond status dpdclass fault*)
	| _ -> raise (Failure "Adequation.place_com_aux error 1") in 
      (opn,prt,eefs),onext
  | (Operator operator1,_)::(Media media,_)::
    [(((Operator operator2) as onext),gateopr2)] ->
      let opn,prt,eefs = match Architecture.bustype media with
      | SamPP,_ ->
	  (*transfer operator1 media operator2 sopn sprt eefs place status*)
	  let opn,prt,eefs =
	    send operator1 media operator2 sopn sprt eefs place data graph
	      cond status dpdclass true in
	    recv operator1 media [operator2] operator2 opn prt eefs place data
	    graph cond status dpdclass true
      | SamMP,_ ->
	  (*transfer operator1 media operator2 sopn sprt eefs place status*)
	  let opn,prt,eefs =
	    reliable_send [operator1] media operator2 sopn sprt eefs place data graph
	      cond status dpdclass true in
	    reliable_recv [operator1] media [operator2] operator2 opn prt eefs place data
	    graph cond status dpdclass true
      | Ram,_ -> raise (Failure " ram media is not yet implemented :
                        see adequation_core 'reliable_place_link' function ") 
	  (*let opn,prt,eefs =
	     write operator1 media sopn sprt eefs place data graph cond
	     status dpdclass fault in 
	    read media operator2 opn prt eefs place data graph cond 
	    status dpdclass fault  *)  in  
      (opn,prt,eefs),onext  
  | _ -> raise (Failure "Adequation.place_com_aux error 2") 

let rec find_best_link lnks best_lnk best_eefs sopn sprt sopn_eefs data graph
    cond status dpdclass operator_dst fault =
  match lnks with
  | [] -> best_lnk
  | ((((Media m) as mda),mg),(o,g))::_ ->
  (* Just take the head : for operator choice, only their distance is
     taken into account as no additionnal operation has to be scheduled on
     them (on contrary to coms on media) *)
      (mda,mg)::[(o,g)]
  | ((((Operator opr) as o1),g1),(mda,mg))::rest ->
      let ((_,_),(o2,g2)) = List.hd (shortest_links mda operator_dst) in 
    (* See above for hd choice *)
    let lnk = (o1,g1)::(mda,mg)::[(o2,g2)] in
    (* don't really place it (boolean place is false) *)
    let (_,_,eefs),_ =
      place_link lnk false sopn sprt sopn_eefs data graph cond status dpdclass fault in
    let best_lnk,best_eefs = match eefs < best_eefs with
    | true -> lnk,eefs
    | false -> best_lnk,best_eefs in
    find_best_link rest best_lnk best_eefs sopn sprt sopn_eefs data
      graph cond status dpdclass operator_dst fault 

(* Returns eefs of dpd (eefs of dpd.sopn). *)
let dpd_EEFS dpd =
  (* I'm not sure for memories... *)
  match (is_constant dpd.t_dpd_sopn) || (is_memory dpd.t_dpd_sopn) with
  | true -> 0.
  | false ->
      (*ps ("eefs dpd "^(name_of_operation dpd.t_dpd_sopn)^" is "^
	(string_of_float (eefs dpd.t_dpd_sopn)));*)
      eefs dpd.t_dpd_sopn

(* Builds dpds, depending on their conditioning level, using place_com_fun. *)
let make_cond_dpds place_com_fun dpds =
  let level dpd = match dpd.t_dpd_class with
  | (Condition l),_ -> l
  | _ -> 0 in
  (* Place conditioning dependences respecting order of conditioning level *)
  let rec dpdlevel dpds tc = match dpds with
  | [] -> tc
  | _ ->
      let levelmin = list_min_value level dpds 0 in
      let level_list, otherlevel_list =
	List.partition (function dpd -> level dpd = levelmin) dpds in
      let tc =
	List.fold_left
	  (fun t dpd -> max t (place_com_fun tc dpd;dpd_EEFS dpd))
	  tc level_list in
      dpdlevel otherlevel_list tc in
  dpdlevel dpds 0.

(* Returns the lefe of operation (average estimation)*)
let rec lefe operation =
  let identifier = identifier_of_operation operation in
  match Hashtbl.mem lefe_table identifier with
  | true -> Hashtbl.find lefe_table identifier
  | false ->
      (* I'm not sure for memories... *)
      let succs,lefe_sub = match (is_memory operation) ||
      (operation.t_opn_dependences_successors=[]) with
      | true -> [],0.
      | false ->
	  let successors = remove_copies (successors operation) in
	  list_max_elements_value lefe successors 0. in
      let lefe = match succs with
      | [] -> 0.
      | _ -> lefe_sub +. (delta_average (List.hd succs).t_opn_class) in
      Hashtbl.add lefe_table identifier lefe;
      lefe

(* Returns the lefe of operation (average estimation)*)
let rec lefe_median operations_median operation =
  (*ps (" compute lefe_median of "^(name_of_operation operation));*)
  let identifier = identifier_of_operation operation in 
  match Hashtbl.mem lefe_table_median identifier with 
  | true -> Hashtbl.find lefe_table_median identifier  
  | false -> 
      (* I'm not sure for memories... *)
      let succs,lefe_sub = (match (operation.t_opn_dependences_successors=[]) with
			    | true -> [], 0.
			    | false -> 
				let successors = remove_copies (successors operation) in 
				let succs,lefe_sub = list_max_elements_value (lefe_median operations_median) successors 0. in
                                succs,lefe_sub) in
      let lefe_med = match succs with
	| [] ->   0.
	| _ -> lefe_sub (*+. (delta_median operations_median (List.hd succs))*) in
      let lefe_med = lefe_med +. (delta_median operations_median operation) in       
      Hashtbl.add lefe_table_median identifier lefe_med;
      (*ps (" lefe_median of "^(name_of_operation operation)^" = "^(string_of_float lefe_med));*)
      lefe_med



	
(* Returns true if operation has already been scheduled *)
let is_scheduled operation =
  (is_constant operation) || (operation.t_opn_operator != None)

(* Returns true if operation is schedulable *)
let is_schedulable operation =
  match (is_scheduled operation) with
  | true -> false
  | false ->
      let preds = match (is_memory operation) with
      | true -> (predecessors operation)@(successors operation)
      | false -> predecessors operation in
      List.for_all
	(fun opn -> (is_scheduled opn) || (is_memory opn) || (is_constant opn))
	preds

(* Updates the list of schedulables, considering that operation has
just been scheduled *)
let update_schedulables operation schedulables =
  schedulables := List.filter ((<>$) operation) !schedulables;
  let new_schedulables =
    List.filter
      (fun opn -> (is_schedulable opn) && (not (is_constant opn)))
      ((successors operation)@(predecessors operation)) in
  schedulables := !schedulables@new_schedulables

(* Debug function to print the result of the adequation *)
let print_adequation_result () =
  ps "Data :";
  Hashtbl.iter
    (fun data l ->
      let available_on =
	List.fold_left
	  (fun s (operator,opn,prt,eefs) -> s^" and "^
	    (name_of_operator operator)^" "^(name_of_data (opn,prt)))
	  " on " l in
  ps ((name_of_data data)^available_on))
    datas;
  ps "Schedules :";
  Hashtbl.iter (fun oprname (_,oprschedule) ->
    ps ("On opr "^oprname^"..........................................");
    let opns =
      Hashtbl.fold
	(fun _ cond_sched opns -> opns@(cond_sched.t_sch_operations))
	oprschedule [] in
    let opns_sorted =
      List.sort
	(fun ({t_opn_rank=rnk1},_,_) ({t_opn_rank=rnk2},_,_) ->
	  compare rnk1 rnk2)
	opns in
    List.iter (fun (opn,esfs,eefs) ->
      ps ((name_of_operation opn)^" "^(string_of_float esfs)^"->"^
	  (string_of_float eefs)))
      opns_sorted) schedules

(* Checks the adequation result. *)
let check_schedule graph =
  (* Check that no operation is scheduled before one of its
     predecessors (except if these are delays) *)
  Hashtbl.iter 
    (fun _ opn -> 
      List.iter
	(fun prec -> 
	  match prec.t_opn_class with
	  | Communication (Receive ((o,_),_,_,_)) -> 
	      (match is_memory o with
	      | true -> ()
	      | false -> 
		  (match eefs prec <= opn.t_opn_esfs with
		  | true -> ()
		  | false ->
		      failwith ((identifier_of_operation opn)^" predecessor "^
				(identifier_of_operation prec)^" schedule error")))
	  | Communication _ -> ()
	  | _ -> match is_memory prec with
	    | true -> ()
	    | false ->
		match eefs prec <= opn.t_opn_esfs with
		| true -> ()
		| false ->
		    failwith ((identifier_of_operation opn)^" predecessor "^
			      (identifier_of_operation prec)^" schedule error"))
	opn.t_opn_predecessors) graph;
  (* Check that all operations are scheduled *)
  Hashtbl.iter
    (fun _ opn ->
      match opn.t_opn_operator with
      | None ->
	  failwith ((identifier_of_operation opn)^
		    " is not scheduled on any operator.")
      | _ -> ())
    graph;
  ps "Schedule ok"

(* Returns [p] if p is a calcul operation otherwise the full sender
path to producer of communication p *)
let rec transitive_predecessors p = 
  match p.t_opn_class with  
  | Communication (Receive _) -> let send = List.hd (predecessors p) in
    send::(union (List.map
		    (fun op -> transitive_predecessors op)
		    (predecessors send)))
  | _ -> [p]

(* Returns [opn] if opn is a calcul operation otherwise the full
   sender paths to receivers of communication opn *)
let rec transitive_successors opn =
  let succs = successors opn in
  match opn.t_opn_class with
  | Communication (Send _) ->
      let succs = union (List.map successors (successors opn)) in
      opn::(union (List.map transitive_successors succs))
  | _ -> [opn]

(* Returns condition in a prettier format, that is easier to use for
   display *)
let pretty_conv_cond condition = match condition with
| None, None -> "",1
| (Some (opn,prt)),(Some vl) ->
    (identifier_of_operation opn)^"."^prt.t_prt_name,vl
| _ -> raise (Failure "Adequation.pretty_conv_opn : error 1")

(* Returns condition list cl in a prettier format, that is easier to
   use for display *)
let pretty_conv_cond_list cl =
  List.map pretty_conv_cond cl

(* Returns opn in a prettier format, that is easier to use for
   display *)
let pretty_conv_opn opn =
  let rank = match opn.t_opn_class with
    (* Each communication takes 2 ranks, one for Receive & one for Send *)
  | Communication (Send _) -> opn.t_opn_rank/2
  | _ -> opn.t_opn_rank in
  let opn_id = identifier_of_operation opn in
  let opn_name = name_of_operation opn in
  let opr = pi opn in
  let opn_cond_list = pretty_conv_cond_list opn.t_opn_condition in
  (opn_id, opn_name, opn.t_opn_esfs, eefs opn, (opn.t_opn_weefs-.(delta opn.t_opn_class opr)),  opn_cond_list, rank)

(* Adds operation to schedules. Operation is added in either the
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

(* Returns adequation_graph in a prettier format, that is easier to
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
	| _ -> (*ps (name_of_operation opn);*) 
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
