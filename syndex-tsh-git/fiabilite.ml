(************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            Ismail ASSAYAD                             *)
(*		                                                         *)
(*                                                                       *)
(*                     Projet POP ART, INRIA Rhone-Alpes                 *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

module Make (Progress_box : Progress_box.PROGRESS_BOX_SIG) =
struct

open Types
open Adequationtypes
open Args
open Transformation

let debug_level = 0

let name_procs = Hashtbl.create 10


let stop_adequation = ref false

let last_len = ref 0.0

let last_rel = ref 1.0

let len = ref 0.0

let rel = ref 1.0   


let name_of_operator o = match o with
| Operator opr -> opr.oprref_name
| Media mda -> mda.mdaref_name

let lsfe_table = Hashtbl.create 30

let datas = Hashtbl.create 30

let save_dpds = Hashtbl.create 10

let replicas_operations = Hashtbl.create 10

let schedules = Hashtbl.create 30

let failure_rates = Hashtbl.create 30


let xsc_constraints = Hashtbl.create 30

let archilib = ref ""

let archiname = ref ""

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

(*Returns operator on which opn has been scheduled. Fails if the operation is not scheduled yet. *)
let pi opn = match opn.t_opn_operator with
| Some opr -> opr
| _ -> raise (Failure ("Reliability.pi : error ("^(name_of_operation opn)^")"))

(* Returns the list of operators compatible with constraints. All operators if constraints are empty. *)
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

(* Returns operators on which ref has been constrained (for old versions, direct constraints) *)
let constraints reference_path =
  let constraints = List.filter (function refpath,_ -> refpath=reference_path) Application.application.app_constraints in
  operators_of_constraints constraints

(* Returns the list of operators on which xscname is constrained *)
let absoluteconstraints xscname =
  let constraints = List.filter (function xsc,_ -> xsc=xscname) Application.application.app_xsc_absolute_constraints in
  operators_of_constraints constraints

(* Returns the list of operators on which operation is constrained *)
let operators_constraint operation =
  let xsc = operation.t_opn_xsc_name in
  match xsc with
  | "" -> (*let path = path operation in
	    (match constraints path with
	    | [] -> *)Application.operators_list !archilib !archiname
      (*        | oprlist -> oprlist) *)
  | _ -> match Hashtbl.find xsc_constraints xsc with
    | [] -> raise (Failure ("Constraints : impossible to find an operator for "^(identifier_of_operation operation)))
    | oprs -> oprs
	  
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

(* Returns operators on which a duration for operation has been defined *)
let operators_able_to_execute operation =
  let opnlib,opnname = deflibname operation in
  let oprs = Architecture.operators_able_to_execute !archilib !archiname opnlib opnname in
  match oprs with
  | [] -> raise (Failure ("No operator able to execute :\nOperation : "^(name_of_operation operation)^"\nDefinition : "^opnlib^"/"^opnname))
  | _ -> List.map (function opr -> Operator opr) oprs

(* Adds data produced by opn as an available data on operator after time eefs *)
let update_datas operator data opn prt eefs =
  let newoprs = (operator,opn,prt,eefs)::(try Hashtbl.find datas data with Not_found -> []) in
    Hashtbl.replace datas data newoprs

(* Adds operation at the end of operator schedule, of corresponding condition, with corresponding esfs and eefs *)
let update_schedules operator operation esfs eefs =
  operation.t_opn_esfs <- esfs;
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
  let (oldschedule,lastopn) = try 
    Hashtbl.find oprschedule operation.t_opn_condition
  with Not_found -> ([],None) in
  let newschedule = oldschedule@[(operation,esfs,eefs)] in
    Hashtbl.replace oprschedule operation.t_opn_condition (newschedule,(Some operation))

(* Calculates routing tables of all operators and media *)
let route oprlist mdalist =
  (* Count Oprs, initialize their id, allocate and initialize their routings: *)
  let nboprs = List.fold_left (fun maxid opr -> opr.oprref_id <- maxid;maxid+1) 0 oprlist in
  let archi = Application.architecture !archilib !archiname in
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
	  | _ -> failwith "Reliability.route" in
	    match opr_cnc.oprref_links2.(dst.oprref_id) = !dist with
	    | false -> process_oprs_connected_opr rest dst mda
	    | true -> mda.mdaref_links2.(dst.oprref_id) <- !dist + 1;
		maxdist := !dist + 1 in
      let rec process_oprs_connected_mda oprs dst opr =
	match oprs with
	| [] -> ()
	| ((_,_),(mda_cnc,_))::rest -> let mda_cnc = match mda_cnc with
	  | Media mda -> mda
	  | _ -> failwith "Reliability.route" in
	    match mda_cnc.mdaref_links2.(dst.oprref_id) = !dist with
	    | false -> process_oprs_connected_mda rest dst opr
	    | true -> opr.oprref_links2.(dst.oprref_id) <- !dist + 1;
		maxdist := !dist + 1 in
	while dist = maxdist do
	  (* Media routing tables *)
	  List.iter (fun mda -> let oprs_connected = Architecture.links !archilib !archiname (Media mda) in
		       List.iter (fun dst -> 
				    match mda.mdaref_links2.(dst.oprref_id) < 0 with
				    | false -> ()
				    | true -> process_oprs_connected_opr oprs_connected dst mda) oprlist) mdalist;
	  (* Operators routing tables *)
	  List.iter (fun opr -> let mda_connected = Architecture.links !archilib !archiname (Operator opr) in
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
	
(* Add a dependence between sopn.sprt and dopn.dprt *)
let add_dependence sopn sprt dopn dprt dpdclass cond status =
  let dpd = {t_dpd_sopn=sopn;t_dpd_sprt=sprt;t_dpd_dopn=dopn;t_dpd_dprt=dprt;t_dpd_class=dpdclass;t_dpd_condition=cond;t_dpd_status=status} in
    sopn.t_opn_dependences_successors <- dpd :: sopn.t_opn_dependences_successors;
    sopn.t_opn_successors <- dopn::sopn.t_opn_successors;
    dopn.t_opn_dependences_predecessors <- dpd :: dopn.t_opn_dependences_predecessors;
    dopn.t_opn_predecessors <- sopn::dopn.t_opn_predecessors
      

(* Remove a dependence of a graph *)
let remove_dependence dpd =
  let sopn,dopn = dpd.t_dpd_sopn,dpd.t_dpd_dopn in
    sopn.t_opn_dependences_successors   <- List.filter (function d -> d<>dpd) sopn.t_opn_dependences_successors;
    dopn.t_opn_dependences_predecessors <- List.filter (function d -> d<>dpd) dopn.t_opn_dependences_predecessors;
    sopn.t_opn_successors   <- List.filter (function opn -> opn<>dopn) sopn.t_opn_successors;    
    dopn.t_opn_predecessors <- List.filter (function opn -> opn<>sopn) dopn.t_opn_predecessors

(* Remove an operation of a graph *)
let remove_operation graph operation =
  List.iter remove_dependence operation.t_opn_dependences_predecessors;
  List.iter remove_dependence operation.t_opn_dependences_successors;
  Hashtbl.remove graph (identifier_of_operation operation)

(* Initializes the route table, data available on operators, schedules and operator_constraints *)
let architecture_init graph =

    last_len :=  0.0;
    last_rel :=  1.0;
    len :=  0.0;
    rel :=  1.0;
    let alib,aname = Application.archi_main_get () in
   (*Hashtbl.clear  failure_rates;*)

    Hashtbl.clear datas;
    Hashtbl.clear schedules;
    Hashtbl.clear save_dpds;
    Hashtbl.clear replicas_operations;
    archilib := alib;
    archiname := aname;
    let oprlist = List.map (Architecture.operator_reference !archilib !archiname ) (Architecture.operators_list !archilib !archiname) in
    let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
      List.iter (function o -> Hashtbl.add schedules o.oprref_name ((Operator o),Hashtbl.create 10)) oprlist;
      List.iter (function m -> Hashtbl.add schedules m.mdaref_name ((Media m),Hashtbl.create 10)) mdalist;
      route oprlist mdalist;
      operators_constraints_init graph

(* Notice that we don't update schedule itself yet. This is for optimum calculation of esfs : we don't need to update schedules when we successively schedule and deschedule the operation *)
let schedule operation operator status graph =
  operation.t_opn_operator <- Some operator;
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
  let (schedule,lastopn) = try 
    Hashtbl.find oprschedule operation.t_opn_condition
  with Not_found -> ([],None) in
    operation.t_opn_pred_scheduled <- lastopn;
    Hashtbl.replace oprschedule operation.t_opn_condition (schedule,Some operation)

(* Same remark as for schedule : this doesn't update the schedule of operator *)
let deschedule operation operator =
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in
  let (schedule,_) = Hashtbl.find oprschedule operation.t_opn_condition in
    Hashtbl.replace oprschedule operation.t_opn_condition (schedule,operation.t_opn_pred_scheduled); (* again, we don't need to update schedule itself, see function schedule for details *)
    operation.t_opn_pred_scheduled <- None;
    operation.t_opn_operator <- None;
    operation.t_opn_esfs <- 0.

(* Deletes the communication corresponding to dpd. Deschedules all communication operations. *)
let rec delete_communication graph opr_dst dpd =
(*   ps ("deleting com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))); *)
  let sopn = dpd.t_dpd_sopn in
    match (is_communication sopn) with
    | true -> let successor = match (is_communication dpd.t_dpd_dopn) with
      | true -> 1(* This com has previously been deleted, we must yet count it in the successors *)
      | false -> 0 in
	(match (((List.length sopn.t_opn_dependences_successors) + successor) > 1) with
	   (* Diffuse data for several operations *)
	 | true ->
	     let sopn = dpd.t_dpd_sopn in
	     let data = data_of_communication sopn in
	       (match sopn.t_opn_class with
		| Communication (Send (_,opr,oprs)) ->
		    Hashtbl.remove graph (identifier_of_operation sopn);
		    let receivers = List.filter (fun opr -> opr <> opr_dst) oprs in
		      (* Changing receiver list in other receive operations *)
		      (* Should change this. Won't work if two identical receive exist (with different operation number #0 #1 etc). This may happen with conditionning *)
		      List.iter (function rcv -> try 
				   let comclass = Receive (data,opr,oprs,rcv) in
				   let opnid = identifier_of_path [(name_of_comclass comclass)] in
				   let opnreceive = Hashtbl.find graph opnid in
				     Hashtbl.remove graph opnid;
				     let recvs = List.filter (fun opr -> opr <> opr_dst) oprs in
				     opnreceive.t_opn_class <- Communication (Receive (data,opr,receivers,rcv));
				     Hashtbl.add graph opnid opnreceive
				 with Not_found -> () (* Operation has already been deleted *)) oprs;
		      (* Changing sopn receiver list and that's all. No recursive deletion as predecessors are used by other operations *)
		      let comclass = Send (data,opr,receivers) in
			sopn.t_opn_class <- Communication comclass;
			let opnname = name_of_comclass comclass in
			  sopn.t_opn_path <- [opnname];
			  Hashtbl.add graph (identifier_of_operation sopn) sopn
		| _ ->  remove_dependence dpd) (* For instance : Receive having several successors on the same operator (reusing a com, nothing to delete except the dependence)*)
	 | false -> 
(* 	     ps ("deleting"^(name_of_operation sopn)); *)
	     let dpds = sopn.t_opn_dependences_predecessors in (* Need to store dpds (which will be deleted by Hashtbl.remove) to enable terminal recursivity *)
	   let opr_dst = match sopn.t_opn_class with
	     | Communication (Send (_,sopr,_)) -> sopr
	     | _ -> opr_dst in
	     deschedule sopn (pi sopn);
	      remove_operation graph sopn;
	     List.iter (delete_communication graph opr_dst) dpds)
    | false -> ()

(* Deletes all predecessors coms of operation *)
let delete_coms operation graph =
  let delete_one_com dpd =
    match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) || (pi dpd.t_dpd_sopn = pi operation) || (match dpd.t_dpd_sprt.t_prt_class with | Precedence_Port -> true | _ -> false) with
    | true -> ()
    | false -> 
	(*ps ("delete com "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_operation operation)^" (on "^(name_of_operator (pi operation)));*)
	let opr_dst = operator_of_operator_class (pi operation) in
	  delete_communication graph opr_dst dpd in
    List.iter delete_one_com operation.t_opn_dependences_predecessors

(* Returns true if operation0 and operation1 are simultaneous *)
let simultaneous s0 e0 s1 e1 = (s0<>e0) && (s1<>e1) && ((s1<=s0 && s0<e1) || (s1<e0 && e0<=e1) || (s0<=s1 && s1<e0) || (s0<e1 && e1<=e0))

(* Returns esfs (of any operation) on operator after time t0*)
let esfs_cond operation operator t0 cond =
  (*ps"Entering esfs_cond procedure";*)
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator operator) in 
  let topr = (*match cond with
    | [(None,None)] ->
	let pred_on_opr = operation.t_opn_pred_scheduled in
	  (match pred_on_opr with
	   | None -> 0.
	   | Some opn -> eefs opn)
    | _ -> *)(* Notice that this calculation assumes that there is no gap in the schedule *)
	  Hashtbl.fold (fun cd (_,lastopn) esfs -> let lastopn = match cd = cond with
			| true ->   operation.t_opn_pred_scheduled
			| false ->   lastopn in
			  match lastopn with
			  | None ->   esfs
			  | Some opn ->  match exclusion_condition opn.t_opn_condition cond with
			    | true ->  esfs
			    | false ->  max esfs (eefs opn)) oprschedule 0. in
    max topr t0
    
(* Place in graph the communication corresponding to dependence dpd after instant tcond with status status *)
let place_communication graph status tcond dpd =
  (* Place the communication of operation_src.port_src between operator_src and operator_dst after instant t0 *)
  let place_com (data,(operator_src,operation_src,port_src,t0)) operation_dst port_dst operator_dst dpdclass cond place status =

    (* Place the operation of communication opncom on the media *) 
    let place_com_media media opncom opnpred prtpred eefspred place status =
      (*      ps("place_com_media "^(name_of_operator (Media media))^" "^(name_of_operation opnpred));*)
      let portdim = prtpred.t_prt_dim in
      let portin = {t_prt_name="in";t_prt_dir=In;t_prt_typename=prtpred.t_prt_typename;t_prt_dim=portdim;t_prt_class=Data_Port;t_prt_order=1}
      and portout = {t_prt_name="out";t_prt_dir=Out;t_prt_typename=prtpred.t_prt_typename;t_prt_dim=portdim;t_prt_class=Data_Port;t_prt_order=1} in
	
      let opnname = unique_identifier graph [] (name_of_comclass opncom) in
      let ports = [portin;portout]@(precedence_ports ()) in
      let opn = new_opn [opnname] (Communication opncom) [] ports [] [] cond (Some (Media media)) 0 0. status 
                  (Ihm [opnname]) None "" "" "" [InitSeq;LoopSeq;EndSeq] in 
      let delta = delta (Communication opncom) (Media media) in
 	schedule opn (Media media) status graph;
	let esfs = esfs_cond opn (Media media) eefspred cond in
	  (match place with
	   | true -> opn.t_opn_esfs <- esfs
	   | false -> ());
	  let eefs = esfs +. delta in
	  let opn,portout = match place with
	    | true ->
		Hashtbl.add graph (identifier_of_operation opn) opn;
		 add_dependence opnpred prtpred opn portin dpdclass cond status;
		(* As routes don't cross, updating data and schedules is not needed if status is false. *)
		(match status with
		 | true -> update_schedules (Media media) opn esfs eefs;
		     (* Update list of data and schedules *)
      		     (match opncom with
		      | Send (_,_,oprs) ->
			  (match Architecture.bustype media with
			   | SamMP,true -> update_datas (Media media) data opn portout eefs
			   | _ -> ())
		      | Receive (_,_,_,executor) -> update_datas (Operator executor) data opn portout eefs
		      | Write _ -> update_datas (Media media) data opn portout eefs
		      | Read (_,opr) -> update_datas (Operator opr) data opn portout eefs
		      | _ -> raise (Failure "Reliability.place_com_media error"));
		 | false -> ());
		opn,portout
	    | false -> deschedule opn (Media media);
		opnpred,prtpred in
	    opn,portout,eefs in
      
    let read s d sopn sprt esfs place status =
      place_com_media s (Read (data,d)) sopn sprt esfs place status in

    let write s d sopn sprt esfs place status =
      place_com_media d (Write (data,s)) sopn sprt esfs place status in

    let transfer s sam d sopn sprt esfs place status =
      place_com_media sam (Transfer (data,s,[d])) sopn sprt esfs place status in

    let send s media d sopn sprt esfs place status =
      place_com_media media (Send (data,s,[d])) sopn sprt esfs place status in

    let recv s media d executor sopn sprt esfs place status =
      place_com_media media (Receive (data,s,d,executor)) sopn sprt esfs place status in

    let diffuse media operator opn prt place status =
      let eefs = eefs opn in
	match place with
	| true -> (match opn.t_opn_class with
		   | Communication (Send (_,opr,oprs)) ->
		       let receivers = match List.mem operator oprs with
			 | true -> oprs
			 | false -> operator::oprs in
			 (match place with
			  | true -> Hashtbl.remove graph (identifier_of_operation opn);
			      (match List.mem operator oprs with
			       | true -> ()
			       | false ->
				   (* Changing receiver list in other receive operations *)
				   List.iter (function rcv -> let comclass = Receive (data,opr,oprs,rcv) in
					      let opnid = identifier_of_path [(name_of_comclass comclass)] in
					      let opnreceive = Hashtbl.find graph opnid in
						Hashtbl.remove graph opnid;
						opnreceive.t_opn_class <- Communication (Receive (data,opr,operator::oprs,rcv));
						Hashtbl.add graph opnid opnreceive) oprs;
				   let comclass = Send (data,opr,receivers) in
				     opn.t_opn_class <- Communication comclass;
				     let opnname = name_of_comclass comclass in
				       opn.t_opn_path <- [opnname];
				       Hashtbl.add graph (identifier_of_operation opn) opn);
			  | false -> ());
			 recv opr media receivers operator opn prt eefs place status
		   | _ -> raise (Failure "Reliability.diffuse : error"))
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
	    | SamMP,true -> diffuse media operator sopn sprt place status
	    | Ram,_ -> read media operator sopn sprt eefs place status
	    | _ -> raise (Failure "Reliability.place_com_aux error 1") in
	    (opn,prt,eefs),onext
      | (Operator operator1,_)::(Media media,_)::[(((Operator operator2) as onext),gateopr2)] ->
	  let opn,prt,eefs = match Architecture.bustype media with
	    | SamPP,_ | SamMP,_ ->
		(*transfer operator1 media operator2 sopn sprt eefs place status*)
		let opn,prt,eefs = send operator1 media operator2 sopn sprt eefs place status in
		  recv operator1 media [operator2] operator2 opn prt eefs place status
	    | Ram,_ ->
		let opn,prt,eefs = write operator1 media sopn sprt eefs place status in
		  read media operator2 opn prt eefs place status in
	    (opn,prt,eefs),onext
      | _ -> raise (Failure "Reliability.place_com_aux error 2") in
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
       | true ->  add_dependence opnpred prtpred operation_dst port_dst dpdclass cond status
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
    
  (* Place the communication between each operator and the destination to determine the best route *)
  (*     let f e = place_com (data,e) dopn dprt operator_dst dpdclass cond false status in *)
  let best_operator_src = List.hd operators_src_list in (*((pi sopn),sopn,sprt,tcond)*)(*List.hd (list_min_elements f operators_src_list 0.)*)
    (* Place the communication *)
    ignore(place_com (data,best_operator_src) dopn dprt operator_dst dpdclass cond true status)

(* Returns eefs of dpd (eefs of dpd.sopn). *)
let dpd_EEFS dpd =
  (* I'm not sure for memories... *)
  match (is_constant dpd.t_dpd_sopn) || (is_memory dpd.t_dpd_sopn) with
  | true -> 0.
  | false -> (*ps ("eefs dpd "^(name_of_operation dpd.t_dpd_sopn)^" is "^(string_of_float (eefs dpd.t_dpd_sopn)));*)
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
	let level_list, otherlevel_list = List.partition (function dpd -> level dpd = levelmin) dpds in
	let tc = List.fold_left (function t -> function dpd -> max t (place_com_fun tc dpd;dpd_EEFS dpd)) tc level_list in
	  dpdlevel otherlevel_list tc in
    dpdlevel dpds 0.

(* Builds operation predecessors coms *)
let make_coms operation graph status =
  let make_one_com tcond dpd =
    debug_ps 3 debug_level ("making com "^(string_of_dpd dpd));
    match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) || (pi dpd.t_dpd_sopn = pi operation) || (dpd.t_dpd_sprt.t_prt_class = Precedence_Port) with
    | true -> ()
    | false -> place_communication graph status tcond dpd;
	match status with
	| true ->  remove_dependence dpd
	| false -> () in
  
  let dpds_data,dpds_condition = List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) operation.t_opn_dependences_predecessors in
    debug_ps 3 debug_level ((name_of_operation operation)^" coms are : "^(List.fold_left (fun s dpd -> s^", "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))) "" dpds_data));
  let tc = make_cond_dpds (make_one_com) dpds_condition in
    List.iter (make_one_com tc) dpds_data

(* Build memory successor coms *)
let make_memory_succ_coms graph opn_eefs operation =
  let dpds_data,dpds_condition = List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) operation.t_opn_dependences_successors in
    ignore (make_cond_dpds (fun tc dpd -> match (pi operation = pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Precedence_Port)  with
			    | true -> ()
			    | false -> place_communication graph true tc dpd) dpds_condition);
    List.iter (fun dpd -> match (pi operation = pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Precedence_Port)  with
	       | true -> ()
	       | false ->  remove_dependence dpd) dpds_condition;
    List.iter (fun dpd -> match (pi operation = pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Precedence_Port)  with
	       | true -> ()
	       | false -> place_communication graph true opn_eefs dpd;
		  remove_dependence dpd) dpds_data

(* Returns operation esfs on operator on which it has been scheduled (taking communication time into account).*)
let esfs operation =
  (*ps "Entering esfs procedure";*)
  let operator = pi operation in
  let delta = delta operation.t_opn_class operator in
  let tdpds = list_max_value dpd_EEFS operation.t_opn_dependences_predecessors 0. in
  let tcond = esfs_cond operation operator tdpds operation.t_opn_condition in
    (*ps ("esfs for "^(name_of_operation operation)^" is "^(string_of_float tcond)^" (dpdps :"^(string_of_float tdpds));*) 
(* ps "Exiting esfs procedure";*)
    tcond

(* Returns the lsfe of operation (average estimation)*)
let rec lsfe operation = 
 let identifier = identifier_of_operation operation in
    match Hashtbl.mem lsfe_table identifier with
    | true -> Hashtbl.find lsfe_table identifier
    | false ->
	(* I'm not sure for memories... *)
      let lsfe_sub = match (is_memory operation) || (operation.t_opn_dependences_successors=[]) with
	| true -> 0.
	| false ->
	    let successors = remove_copies (successors operation) in
	      list_max_value lsfe successors 0. in
      let lsfe = lsfe_sub +. (delta_average operation.t_opn_class) in
	Hashtbl.add lsfe_table identifier lsfe;
	lsfe 

(* Returns the lsfe of operation (average estimation)*)
(*let rec coLevel operation operator=
 let identifier = identifier_of_operation operation in
    match Hashtbl.mem lsfe_table identifier with
    | true -> Hashtbl.find lsfe_table identifier
    | false ->
	(* I'm not sure for memories... *)
      let lsfe_sub = match (is_memory operation) || (operation.t_opn_dependences_successors=[]) with
	| true -> 0.
	| false ->
	    let successors = remove_copies (successors operation) in
	      list_max_value lsfe successors 0. in
      let lsfe = lsfe_sub +. (delta_average operation.t_opn_class)  in
	Hashtbl.add lsfe_table identifier lsfe;
	(lsfe -. (delta  operation.t_opn_class operator))
*)
let coLevel operation operator =
  let lsfe = (lsfe operation) in
   ( lsfe -. (delta_average operation.t_opn_class) )
(* Schedules constants on all operators on which they will be needed (possible duplications) *)
let place_constant graph progress_box =
  let rec place opn dpds = match dpds with
  | [] ->  remove_operation graph opn
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
      update_schedules opr newopn 0. 0.;
      List.iter (function dpd ->
	let prt = List.find (function {t_prt_name=prtname;t_prt_dir=dir} -> prtname=dpd.t_dpd_sprt.t_prt_name && dir=Out) newopn.t_opn_ports in
	 add_dependence newopn prt dpd.t_dpd_dopn dpd.t_dpd_dprt dpd.t_dpd_class dpd.t_dpd_condition dpd.t_dpd_status) dpds_opr_list;
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

(* Returns the optimal esfs, schedule pressure and operator for operation. Fails if no operator is able to execute this operation *)
let best_opr_esfs_sp operation operators graph =
  let opnlib,opnname = deflibname operation in
  let (opr_opt,esfs_opt,_) = 
    List.fold_left (fun (opr_m,esfs_m,spfastmin) operator -> 
  		      debug_ps 2 debug_level ("Trying "^(name_of_operation operation)^" on "^(name_of_operator operator));
		      match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
		      | true -> let delta = delta operation.t_opn_class operator in
			  schedule operation operator false graph;
 			  make_coms operation graph false;
			  let esfs = esfs operation in
			  let spfast = esfs +. delta in
			    (* update_schedules is not required. The schedule on an operator is actually only used after the operation has definitely been scheduled. *)
 			    delete_coms operation graph;
			    deschedule operation operator;
			    (match spfast < spfastmin with
			     | true -> (operator,esfs,spfast)
			     | false -> (opr_m,esfs_m,spfastmin))
		      | false -> (opr_m,esfs_m,spfastmin)) (List.hd operators,max_float,max_float) operators in
    match esfs_opt = max_float with
    | true -> failwith ("No operator able to execute :\nOperation : "^(name_of_operation operation)^"\nDefinition : "^opnlib^"/"^opnname)
    | false -> let sp = (lsfe operation) +. esfs_opt in
	(*       ps ("For operation "^(name_of_operation operation)^" esfs "^(string_of_float esfs_opt)^" sp "^(string_of_float sp)); *)
	(opr_opt,(esfs_opt,sp))



(*****************************************************)          
(*   get original operation name of backup operation *)
(*****************************************************)         
let get_original opn =
  let name = name_of_operation opn in
  try 
    let original =(String.sub name 0 (String.rindex name '#')) in (String.sub original 0 (String.rindex original '_'))
  with _ ->  name

let get_original_string name_opn =
  try 
    let original =(String.sub name_opn 0 (String.rindex name_opn '#')) in (String.sub original 0 (String.rindex original '_'))
  with _ -> name_opn

(*************************************)     
(**  test if operation is original  **)
(*************************************)
let is_original opn =
 let name = name_of_operation opn in
 try
   if (String.contains name '#')
   then false
   else true
 with _  -> true

(****************************************)
(*  update the list of copies operation *)    
(****************************************)

let update_replicas replica  =
let original = (get_original replica) in
  let replicas = (match (Hashtbl.mem replicas_operations original) with
       	         | true  -> replica::(Hashtbl.find replicas_operations original)  
		 | false -> [replica]) in
  (Hashtbl.replace replicas_operations original replicas);;

(****************************************)
(*  update the list of copies operation *)    
(****************************************)
let remove_replicas replica  =
  let original = (get_original replica) in
   Hashtbl.remove replicas_operations original
(*
  let replicas = (match (Hashtbl.mem replicas_operations original) with
       	         | true  -> List.tl (Hashtbl.find replicas_operations original)  
		 | false -> (Hashtbl.find replicas_operations original)) in
  (Hashtbl.replace replicas_operations original replicas)*);;

(*************************************************************)
(* Adds copies of scheduled operation into mould_operations  *)
(*************************************************************)
let update_dpds operation old_dpd new_dpd =
  let dpds = (old_dpd,new_dpd)::(try (Hashtbl.find save_dpds operation) with Not_found -> []) in
    Hashtbl.replace save_dpds operation dpds

(*************************************************************************)
(************ Delete Changes performed by change_dpds_pred  **************)
(*************************************************************************)
let delete_change_dpds_pred operation =
 (try 
    let dpds = (match (Hashtbl.mem save_dpds operation) with
       	         | true  -> (Hashtbl.find save_dpds operation)  
		 | false -> [] ) in              
    List.iter (function ((sopn,sprt),dpd) -> dpd.t_dpd_sopn <- sopn;dpd.t_dpd_sprt <- sprt) dpds;
    Hashtbl.remove save_dpds operation;  
   with _ -> ()) 


(*************************************************************)
(* Adds copies of scheduled operation into mould_operations  *)
(*************************************************************)
let update_dpds operation old_dpd new_dpd =
  let dpds = (old_dpd,new_dpd)::(try (Hashtbl.find save_dpds operation) with Not_found -> []) in
    Hashtbl.replace save_dpds operation dpds

(***************************************************************)
(**** Change dpds if operation preds copies is in operator *****)
(***************************************************************)
let change_dpds_pred operation operator status =
  List.iter (function dpd -> 
   try 
    let copies = (Hashtbl.find replicas_operations (get_original dpd.t_dpd_sopn)) in         
    let pred = List.find (function lip -> (pi lip)=operator) copies in   
    let sprt = List.find (function prt -> prt.t_prt_name=dpd.t_dpd_sprt.t_prt_name && prt.t_prt_dir=Out) pred.t_opn_ports in
    if status 
    then begin
          pred.t_opn_successors <- operation :: pred.t_opn_successors;
          operation.t_opn_predecessors <- pred :: operation.t_opn_predecessors;
           remove_dependence dpd;
         end;
    let old_dpd = (dpd.t_dpd_sopn,dpd.t_dpd_sprt) in
    let new_dpd = (pred,sprt) in
      dpd.t_dpd_sopn <- pred;dpd.t_dpd_sprt <- sprt;
    update_dpds operation  old_dpd dpd;
   with _ -> ()) operation.t_opn_dependences_predecessors



(********************************************************)
(******  GET A COPIE FOR A CREATION IN OPERATOR ********)
(********************************************************)
let get_copie_operation operation status size =
  (* let operation = Hashtbl.find mould_operations (get_original operation) in*)
  (* let size = (replicas_size operation) in*)
  let name_copie = ("_"^(string_of_int size)^"#") in
  let backup = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values operation.t_opn_ports
	          [] [] operation.t_opn_condition None 0 0. status operation.t_opn_origin None  operation.t_opn_xsc_name 
                  operation.t_opn_referencing_alglib operation.t_opn_referencing_algname operation.t_opn_code_phases) in

  backup.t_opn_path <- (List.rev (List.tl (List.rev backup.t_opn_path)))@[(List.hd (List.rev backup.t_opn_path))^name_copie]; 
  backup.t_opn_ports <- List.map (function p -> match ((p.t_prt_dir=Out)&(p.t_prt_name<>"precedence_out")) with
       | false  -> p
       | true -> {t_prt_name=(p.t_prt_name(*^"_"^(string_of_int size)^"_C"*));t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order}) backup.t_opn_ports;

  backup.t_opn_ports <- List.map (function p -> match ((p.t_prt_dir=In)&(p.t_prt_name<>"precedence_in")) with
       | false  -> p
       | true -> {t_prt_name=(p.t_prt_name(*^"_"^(string_of_int size)^"_C"*));t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order})  backup.t_opn_ports;

  backup.t_opn_dependences_predecessors <- List.map (function dpd ->    
    let dprt = (List.find (function prt -> prt.t_prt_name=(((dpd.t_dpd_dprt).t_prt_name)(*^"_"^(string_of_int size)^"_C"*))) backup.t_opn_ports) in {t_dpd_sopn=dpd.t_dpd_sopn;t_dpd_sprt=dpd.t_dpd_sprt;t_dpd_dopn=backup;t_dpd_dprt=dprt;t_dpd_class=dpd.t_dpd_class;t_dpd_condition=dpd.t_dpd_condition; t_dpd_status=dpd.t_dpd_status}) operation.t_opn_dependences_predecessors;

 
  backup.t_opn_predecessors <- List.map (function pred -> pred) operation.t_opn_predecessors;
  backup.t_opn_xsc_name <- operation.t_opn_xsc_name;
  backup 


(**********************************************)
(******* Schedule all npf operation copies ****)
(**********************************************)
let schedule_npf_opn_copies original copies_opts graph status=
 let size = ref 1 in
 List.iter (function operator_optimal,(esfs_optimal,_,lips_optimal) ->
    let copie = (get_copie_operation original status !size) in
    size := !size+1;
    Hashtbl.add graph (identifier_of_operation copie) copie;
    change_dpds_pred copie operator_optimal status;
    schedule copie operator_optimal status graph;
    let delta_opt = delta copie.t_opn_class operator_optimal in
       make_coms copie graph true;
       let esfs_optimal = (esfs copie) in
       update_schedules operator_optimal copie esfs_optimal (esfs_optimal+.delta_opt);
    ps ("schedule "^(name_of_operation copie)^" into "^(name_of_operator operator_optimal));
       (* Data becoming available on operator *)
       List.iter (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
           | Out,Data_Port | Out,Init_Memory_Port -> 
                      debug_ps 4 debug_level ("Adding "^(name_of_data (copie,prt))^" on "^(name_of_operator operator_optimal)^"(with cond "^(string_of_condlist copie.t_opn_condition)^")");
			   update_datas operator_optimal (copie,prt) copie prt (eefs copie)
		       | _,_ -> ()) copie.t_opn_ports; ) copies_opts

(*
####################################################
# Compute the reliability of the scheduling graph using cut set
####################################################*)
let compute_reliability failures old =  
  match old with
    |true ->  
       let  reliability =  
	 (Hashtbl.fold ( 
	    fun _  copies rel_tmp -> 
	      let opn_reliability = 
		(List.fold_left (
		   function rel_aux -> 
		     function copy -> 
		       let opr = (pi copy) in 			 
			 rel_aux *.  
			   (1. -. (exp(-.((Hashtbl.find failure_rates (name_of_operator opr)) *. ((eefs copy)-. copy.t_eefs_opn_pred)))))) 1. copies) in    
		(rel_tmp *. ( 1. -. opn_reliability )) 
	  ) replicas_operations 1.) in  reliability
    |false -> (* Compute the reliability of the scheduling graph : using "arbre"*)
	 let ordo = 
	   Hashtbl.fold 
	     (fun opn replicas ordo_tmp ->    
		(List.fold_left (function oprlst  -> function copie -> (Hashtbl.find name_procs (name_of_operator (pi copie)))::oprlst) [] replicas)::ordo_tmp) replicas_operations [] in
	   (valeur (construire_arbre_fiabilite1 ordo failures))


(*
#################################
# Conversion of a decimal integer
# to its binary representation
################################
*)
 let conversion x = 
  let l = ref [] in 
   while !x <> 0 do
    let r = !x mod 2 in
     l := r :: !l; 
     x := ( !x - r )/ 2  
   done; l
 
(*
#################################
# The number of 1s in a binary 
# array
################################
*)

let number_of_procs bit_array =
 let number = (Array.fold_left (fun num b -> if b = 1 then num := !num+1; num ) (ref 0)  bit_array) in 
 ps ("The number of processors is  = "^(string_of_int (!number)))

(*
######################################
# The  list  of  processors  from  the 
# operators     constraints    of   an 
# operation. This  list   is  obtained
# from the binary array representation
######################################
*)
let list_of_procs bit_array operators_constraints =

  let list_invalide = ref false in
 let all_operators =  (List.rev (Application.operators_list !archilib !archiname )) in
 
 let (list_procs,_) = 
     Array.fold_right ( fun   b (procs,indice) -> 
			                        if b = 1 then begin
					           let new_proc = (List.nth all_operators !indice) in
						   let name_new_proc = (name_of_operator new_proc) in
						   try   
						   if (List.exists (fun proc -> 
						       if name_new_proc = (name_of_operator proc) 
						       then true else false) operators_constraints) = true 
						   then begin
						         procs := (new_proc :: !procs)                              		                                              end else list_invalide := true
                                                   with exn ->  print_string ("The operators_constraints is empty :");
						   (*indice := !indice + 1;*)
                                                 end;
                                                 indice := !indice + 1;
                                                
						 (procs, indice)
					
      ) bit_array ((ref []), (ref 0 ))
 in 
   if (!list_invalide = true) then ref [] else
    list_procs




(*
######################################################
# Returns the optimal compromise and the corresponding
# operators for operation considering replicas.  Fails 
# if no operator is able to execute this operation 
#####################################################
*)
let best_oprs_compromise  operation operators graph combinaisons_b_list_array step failures old =
  
  (* We begin with a loop on the all possible  combinaisons  on  the  procs
     of the architectures in order to find the best  implementation  of the 
     operation on  the  architecture. The size  of  the  found  combinaison 
     must be  small than the replication level required by the user *)
  
  let colevel = (coLevel operation (List.hd operators)) in
    
  let comp_opt = ref (max_float) in 
  let gain_opt = ref ( max_float) in
  let loss_opt = ref ( max_float) in
  let esfs_opt = ref max_float in
  let delta_opt = ref 0. in
  let len_opt = ref max_float in 
  let rel_opt = ref 0. in 
  let list_opr_opt = ref [] in
  let no_operator = ref true in
    
  let next_len_obj = ref max_float in (*For simulation purposes *)
  let next_rel_obj = ref min_float in (*For simulation purposes *)
  let i = ref 1 in
    while (!stop_adequation = true || (!i= 1))do      
      i:=1;               	
      while (!i  <= (Array.length combinaisons_b_list_array) ) do
	let b_list = (Array.get combinaisons_b_list_array (!i-1)) in  
	let procs_list =  list_of_procs (Array.of_list !b_list) operators in	  
	  let exec_max = ref 0. in 
	  let esfs_list = ref max_float in
	  let list_length = ( List.length !procs_list ) in
            
            if (list_length <> 0 ) then begin no_operator := false  end;
	    
            if (0 < list_length && list_length  <= !Args.replication_level ) then begin 
              (*ps (List.fold_left (function s -> function opr -> s^"  "^(name_of_operator opr)) ("    Trying "^(name_of_operation operation)^" on processors : ") !procs_list);*)
              for j = 1 to  list_length do 
		(*ps ("Calling the nth procedure on the procs_list. Length is "^(string_of_int (List.length !procs_list))) ;
		  let delta = delta operation.t_opn_class (List.nth  !procs_list (j-1)) in     
		  ps ("Scheduling");  *)     
		let operator = (List.nth  !procs_list (j-1))  in
		let copie = (get_copie_operation operation false j) in
		  (* size := !size+1; *)
		let delta_copie = delta copie.t_opn_class operator in
		  change_dpds_pred copie operator false;
		  schedule copie operator false graph;
		  make_coms copie graph false;
		  update_replicas copie;
		  let eefs_copy = delta_copie +. (esfs copie) in
		  let esfs_copy = (esfs copie) in
		    if esfs_copy < !esfs_list then begin
		      esfs_list := esfs_copy
		    end;
		    if (eefs_copy > !exec_max) then
		      exec_max := eefs_copy
              done;
	      rel := compute_reliability failures old;

              (*ps ("temporary reliability = "^(string_of_float !rel));*) 

	      len := !exec_max;
	      (*let gain = (!len -. !last_len) /. (!Args.obj_len -. !last_len)  in*)
	      let gain = ((colevel+. !len) -. !last_len) /. (!Args.obj_len -. !last_len)  in
		(*let gain = (((lsfe operation)+. !esfs_list) -. !last_len) /. (!Args.obj_len -. !last_len)  in*)
	      let loss = (!rel -. !last_rel) /. (!Args.obj_rel -. !last_rel)  in 
		(*  let comp = ((cos ((!Args.theta *. 3.14) /. 180.)) *. (abs_float gain)) +. ((sin ((!Args.theta *. 3.14 ) /. 180.)) *. (abs_float loss)) *)
	      let comp = ((cos ((!Args.theta *. 3.14) /. 180.)) *. (gain)) +. ((sin ((!Args.theta *. 3.14 ) /. 180.)) *. ( loss)) 
	      in
		try 
		  let copies = (Hashtbl.find replicas_operations (name_of_operation operation)) in
		    for j = 1 to List.length copies do
		      let copy = (List.nth copies (j-1)) in
			delete_coms copy graph;
			deschedule copy (pi copy);
			delete_change_dpds_pred copy    
		    done; 


		    if (comp < !comp_opt && !len <= !Args.obj_len && !rel >= !Args.obj_rel)  then  begin
		      comp_opt := comp;
		      loss_opt := loss;
		      gain_opt := gain;
		      esfs_opt := !esfs_list;
		      len_opt := !len;
		      rel_opt := !rel;
		    
		      list_opr_opt := !procs_list 
		
end;
		    (* for simulation purposes *)
		    if (!next_rel_obj < !rel && list_length = 1 ) then begin
		      next_len_obj := !len;
		   (* if (!next_rel_obj < !rel) then*)
		      next_rel_obj := !rel;
		    end;
		    remove_replicas operation
		with Not_found -> ps "???? Not_found here";() 
	    end;
	    i:=!i+1
	done;

	match (!comp_opt = max_float && !no_operator= true) with (* anyway if no_operator is true then comp_opt is max_float ! *)
	  | true -> 
	      ps ("\n  Execution_Exception : No operator able to execute :\nOperation : "^(name_of_operation operation)^"\n");
	      failwith ("No operator able to execute :\nOperation : "^(name_of_operation operation)^"\n")
	  | false -> ();
	      
	      
	      if (!rel_opt < !Args.obj_rel)  && (!len_opt > !Args.obj_len  ) then begin
		ps ("\n  Objectives_Not_achieved Exception : Reliability and length objectives  not achieved. rel and len are: "^(string_of_float !rel_opt)^"  "^(string_of_float !len_opt)^"  rel_obj and len_obj are: "^(string_of_float !Args.obj_rel)^"  "^(string_of_float !Args.obj_len));
		Args.obj_rel :=  !next_rel_obj;
	
		Args.replication_level := 1;
		stop_adequation := true
	      end
	      else 
		if (!rel_opt < !Args.obj_rel) then begin
		  ps ("\n  Objectives_Not_achieved Exception : Reliability objective not achieved. rel is: "^(string_of_float !rel_opt)^ "  rel_obj and len_obj are: "^(string_of_float !Args.obj_rel)^"  "^(string_of_float !Args.obj_len));
		  Args.obj_rel :=  !next_rel_obj;
		  Args.replication_level := 1;
		  stop_adequation := true
		end
		else if   (!len_opt > !Args.obj_len  ) then begin
		  ps ("\n  Objectives_Not_achieved Exception : Length objective not achieved. Length is: "^(string_of_float !len_opt)^ "  rel_obj and len_obj are: "^(string_of_float !Args.obj_rel)^"  "^(string_of_float !Args.obj_len));
		  Args.obj_len := !next_len_obj ;
		  Args.replication_level := 1;
		  stop_adequation := true
		end else   stop_adequation := false;
	      if (!stop_adequation = true) then begin
	      comp_opt :=  (max_float) ;
	      gain_opt :=  ( max_float); 
	      loss_opt :=  ( max_float) ;
	      esfs_opt :=  max_float ;
	      delta_opt :=  0.; 
	      len_opt := max_float  ;
	      rel_opt := 0. ;
	      list_opr_opt := []; 
	      no_operator := true ;
	      
	      next_len_obj := max_float;  (*For simulation purposes *)
	      next_rel_obj := min_float ; (*For simulation purposes *)
	      i:= 1;
	      end;
	    
      done;
      ((list_opr_opt,!comp_opt),!esfs_opt)
      
	    
	    
(* Returns true if operation has already been scheduled *)
let is_scheduled operation = (is_constant operation) || (operation.t_opn_operator <> None)
			       
(* Returns true if operation is schedulable *)
let is_schedulable operation =
  match (is_scheduled operation) with
  | true -> false
  | false ->
      let preds = match (is_memory operation) with
	| true -> (predecessors operation)@(successors operation)
	| false -> predecessors operation in
      List.for_all (fun opn -> (is_scheduled opn) || (is_memory opn) || (is_constant opn)) preds

(* Updates the list of schedulables, considering that operation has just been scheduled *)
let update_schedulables operation schedulables =
  schedulables := List.filter ((<>) operation) !schedulables;

  let new_schedulables = List.filter (fun opn -> (is_schedulable opn) && (not (is_constant opn))) ((successors operation)@(predecessors operation)) in
    schedulables := !schedulables@new_schedulables

(* Selects the best candidate (considering reliability critical path schedule compromise) among operations_operatoropt *)
let find_best_candidate operations_operatoropt =

  let (best_cand,best_comp) = match (List.length operations_operatoropt) = 1 with
  | true -> ((List.hd operations_operatoropt),-1.)
  | false ->  let  (opnh,((oprh,comph),esfsh)) =   (List.hd operations_operatoropt) in

let (opn_min,((opr_min,comp_min), esfs_min)) = List.hd (list_min_elements (fun (opn,((opr,comp),esfs)) -> esfs ) operations_operatoropt 0.) in
            
              let operator_min = (List.hd (list_min_elements (fun op -> (delta opn_min.t_opn_class op) ) !opr_min 0.)) in
              let eefs_min =  esfs_min +. (delta opn_min.t_opn_class operator_min) in
              List.fold_left (fun ((((opn_max,((opr_max,cmax),es))) as cand_max) ,comp_max) ((opn,((opr,comp),esfs)) as cand) -> 
               
                match ( esfs <= eefs_min &&  comp > comp_max) with 
               
	         | true ->(*ps ("     Update opn "^(name_of_operation opn)^" comp_max = "^(string_of_float comp_max)^" comp = "^(string_of_float comp)^" esfs = "^(string_of_float esfs)^" eefs_min = "^(string_of_float eefs_min));*)  remove_replicas  opn_max; (cand,comp) 
           
	        | false -> (*ps (" not update opn "^(name_of_operation opn)^" comp_max = "^(string_of_float comp_max)^" comp = "^(string_of_float comp)^" esfs = "^(string_of_float esfs)^" eefs_min = "^(string_of_float eefs_min));*)remove_replicas  opn; (cand_max,comp_max)) 
                           (List.hd operations_operatoropt,(-1. *. max_float))
                           ( operations_operatoropt) in 
    best_cand




let initialize_failure_rates_table   file_name  =     
        let (algo_lib,algo_name,_),(archilib,archiname) = Application.algo_main_get (), Application.archi_main_get () in
        let all_operators = Application.operators_list archilib archiname in

        let name_fileRel = (String.sub file_name 0 (String.rindex file_name '.'))^".rel" in
        let fileRel = open_in name_fileRel  in
        let ch = ref "" in
        List.iter ( fun   opr -> ch := (input_line fileRel); Hashtbl.add failure_rates (name_of_operator opr) (float_of_string !ch) ) all_operators;
        close_in fileRel          
         (* random failures *)
        (*ps  ( Hashtbl.fold (fun  opr f s -> (s^opr^"  "^ (string_of_float f)^" ; ")) failure_rates "   "  )*)
       
let max_eefs opn =
  let (_,oprschedule) = Hashtbl.find schedules (name_of_operator (pi opn)) in
  let eefs =  (Hashtbl.fold (fun  _  (lst , _) old_best -> 
      let best =  List.fold_left ( fun  last (_,_,eefs)   -> if eefs >= last then last else eefs   )  opn.t_opn_esfs lst in  
  best  
 ) oprschedule  0.) in eefs

(*
#########################################
# Computes the adequation of graph. Fails
# with  various  exception  if  something 
# went wrong 
#########################################
*)
let reliability_adequation old graph =
  stop_adequation := false;  
  let s = Hashtbl.fold (fun _ opn s -> s^"\n"^(identifier_of_operation opn)^(string_of_argsvalues opn.t_opn_arguments_values)) graph "Initial graph :" in
    debug_ps 1 debug_level s;
    architecture_init graph;
    
    Hashtbl.clear lsfe_table;
    let all_media = Application.media_list !archilib !archiname in
    let all_operators = Application.operators_list !archilib !archiname in   

    let failures = Array.create (List.length all_operators) 0. in

    let index = ref 1 in
      List.iter (function opr -> (*ps ((name_of_operator opr)^" = "^(string_of_int !index));*) 
		   failures.(!index-1) <- 1. -. (Hashtbl.find failure_rates  (name_of_operator opr));
		   Hashtbl.add name_procs (name_of_operator opr) !index;index := !index+1;) all_operators;
      
      let combinaisons = (int_of_float ((2. ** (float_of_int (List.length  all_operators)))) - 1) in
	
      (* List.iter (fun p ->  match p with
	 | Operator op ->  ps ("processor failure rate property of  is: "^(string_of_float op.oprref_failure_rate))
	 | Media op ->  ps  ("media failure rate property of  is: "^(string_of_float op.mdaref_failure_rate) )) (all_operators@all_media);
      *)
	
      let schedulables = ref (hashtbl_filter (fun o -> (is_schedulable o) && (not (is_constant o))) graph) in
      let step total = match total<100 with
	| true -> 1
	| false -> total/100 in
	
      let total_adequation = hashtbl_length graph in
      let step_adequation = step total_adequation in
	
      let operators_number = List.length all_operators in
	
      let progress_box = Progress_box.create total_adequation  in
	try
	  let step = ref (0) in 
	  let combinaisons_b_list_array  = Array.make combinaisons (ref []) in
	    for i = 1 to combinaisons do
	      let b_list = conversion (ref i) in
		Array.set combinaisons_b_list_array (i-1) b_list;
		(*ps (List.fold_left (function s -> function opr -> s^"  "^(string_of_int opr)) "num processors : " !b_list);*)
	    done;
	    while !schedulables <> [] do
	      step := !step + 1;
	      (* ps ("");
		 ps ((string_of_int !step)^" "); *)
	      (* Calcul of the optimal  operators list for each operation *)
	      
	      let valid_operators opn  = 
		let opnlib,opnname = (deflibname opn) in 
		  List.filter (fun opr ->   (List.exists (fun opr2 -> 
		      if (name_of_operator opr) = (name_of_operator opr2) 
                      then true else false)( List.map (fun opr -> Operator opr) 
                          (Architecture.operators_able_to_execute !archilib !archiname opnlib opnname))))
		    (operators_constraint opn)
              in 
	      let operations_operatoropt = List.map (function opn -> (opn,best_oprs_compromise opn (valid_operators opn) graph  combinaisons_b_list_array !step failures old)) !schedulables in
		(* Determine the most urgent candidate *)
		if (!stop_adequation = true) then begin (*Never happen*)
		  schedulables := []; last_rel := 1.; last_len:=1. end
		else begin
		  let operation_optimal,((operators_optimal,operator_optimal),esfs_min) = 
		    find_best_candidate operations_operatoropt in
		    debug_ps 2 debug_level (List.fold_left (function s -> function opn -> s^(identifier_of_operation opn)^" ") "schedulables : " !schedulables);   
                    (* Calcul of the optimal operator for each operation *)
		    let operator_optimal = List.hd !operators_optimal in
		    let operators_optimal_tl = List.tl !operators_optimal in
		      (*let operator =  (List.hd !operators_optimal) in *)
		      (*ps (List.fold_left (function s -> function opr -> s^(name_of_operator opr)^" ") (" -> Operation \""^(name_of_operation  operation_optimal)^"\" is scheduled on : ") !operators_optimal);*)
		      
		    let  last_len_aux = ref min_float in
		      for j = 1 to (List.length operators_optimal_tl) do		     
			let copy = (get_copie_operation operation_optimal true j) in
			let operator = (List.nth  operators_optimal_tl (j-1))  in
			  Hashtbl.add graph (identifier_of_operation copy) copy;			 
			  change_dpds_pred copy operator true;
			  schedule copy operator true graph;
			  let delta_copy = delta copy.t_opn_class operator in
			    (* ("Making coms for a copie");*)
			    make_coms copy graph true; update_replicas copy; 
			    let esfs_copy = (esfs copy) in
			    let delta_copy = (delta copy.t_opn_class operator) in
			      update_schedules operator copy esfs_copy (esfs_copy+.delta_copy);			     		
			      if  ((esfs_copy +. delta_copy  )> !last_len_aux) then begin
				last_len_aux := (esfs_copy +. delta_copy)
			      end;			     		   			     
		      done;  
		      change_dpds_pred operation_optimal operator_optimal true;
		      schedule operation_optimal operator_optimal true graph;
		      let delta_o = delta operation_optimal.t_opn_class operator_optimal in
			(*ps ("Making coms for the optimal operation");*)
			make_coms operation_optimal graph true;
			update_replicas operation_optimal;
			
			last_rel := compute_reliability failures old;
			
			let esfs_o = (esfs operation_optimal) in
			let delta_o = (delta operation_optimal.t_opn_class operator_optimal) in
			  update_schedules operator_optimal operation_optimal esfs_o (esfs_o+.delta_o);			 		
			  
			  if ( (esfs_o +.  delta_o )> !last_len_aux) then
			    last_len_aux := ( esfs_o +.  delta_o);
			  (*ps ("last_len_aux = "^(string_of_float !last_len_aux));
			    ps ("last_len = "^(string_of_float !last_len));*)
			  if  (!last_len_aux  > !last_len ) then last_len := !last_len_aux ;  
			  
			  (* Data becoming available on operator *)
			  List.iter (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
				       | Out,Data_Port | Out,Init_Memory_Port -> debug_ps 4 debug_level ("Adding "^(name_of_data (operation_optimal,prt))^" on "^(name_of_operator operator_optimal)^"(with cond "^(string_of_condlist operation_optimal.t_opn_condition)^")");
					   update_datas operator_optimal (operation_optimal,prt) operation_optimal prt (eefs operation_optimal)
				       | _,_ -> ()) operation_optimal.t_opn_ports;
			  
			  (* We need to place memory successor coms now (this is not needed to determine operation esfs). *)
			  (* Yet successor dependences will increase the total latency. So this should be taken into account (improve this) to choose best operator. *)
			  (match is_memory operation_optimal with
			     | true -> ps "is_memory !"; exit 0; make_memory_succ_coms graph (eefs operation_optimal) operation_optimal
			     | false -> ());
  			  debug_ps 1 debug_level ("adequation place_calcul : "^(name_of_operation operation_optimal)^" on "^(name_of_operator operator_optimal)^"..............ok");
			  
			  (* Update the list of schedulables operations *)
			  update_schedulables operation_optimal schedulables;
			  operation_optimal. t_eefs_opn_pred <- max_eefs operation_optimal;                      	                       
			  Progress_box.tick progress_box;
		end       	      
	    done;
	    (*ps "";
	      ps (" ___________________________________________________ ");
	      ps ("|                                                   |");
	      ps ("| > Cycle time is "^(string_of_float  !last_len )^"                                ");
	      ps ("| > Reliability is "^(string_of_float  !last_rel)^"                   "); 
	    (* ps ("| > Robj is "^(string_of_float  !Args.obj_rel)^"                          "); 
	      ps ("| > Lobj is "^(string_of_float  !Args.obj_len)^"                                      ");    
	      ps ("| > Theta is "^(string_of_float  !Args.theta)^"                                    ");*)
	      ps ("|___________________________________________________|");*)
	    
	    
	    
	    (*  ps "Exiting the adequation with reliability";  *)
	    place_constant graph progress_box;
	    
	    (* We need to have the list of operations sorted by esfs (for code generation) *)
	    if (!stop_adequation = false)
	    then begin 
	      Hashtbl.iter (
		fun _ (_,oprschedule) ->
		  let opns = Hashtbl.fold (fun _ (cond_opns,_) opns -> opns@cond_opns) oprschedule [] in
		  let opns_sorted = List.stable_sort (function _,esfs1,eefs1 -> function _,esfs2,eefs2 -> match compare esfs1 esfs2 with
							| 0 -> compare eefs1 eefs2
							| x -> x) opns in
		    ignore 
		      (List.fold_left 
			 (function i -> function opn,_,_ ->
			    (match is_communication opn with
			       | false ->
				   if (is_original opn)
				   then opn.t_opn_path <- 
				     (List.rev (List.tl (List.rev opn.t_opn_path)))@[(List.hd  (List.rev opn.t_opn_path))^"_1"]
  				   else  opn.t_opn_path <- (List.rev (List.tl (List.rev opn.t_opn_path)))@
				     [(String.sub (List.hd (List.rev opn.t_opn_path)) 0 
					 (String.rindex (List.hd (List.rev opn.t_opn_path)) '#'))];
			    | true -> 
				if (is_original opn)
				then opn.t_opn_path <- (List.rev (List.tl (List.rev opn.t_opn_path)))@
				  [(String.sub (List.hd (List.rev opn.t_opn_path)) 0 
				      (String.rindex (List.hd (List.rev opn.t_opn_path)) ')'))^"_1)"]
  				else opn.t_opn_path <- (List.rev (List.tl (List.rev opn.t_opn_path)))@
				  [(String.sub  (List.hd (List.rev opn.t_opn_path)) 0 
				      (String.rindex (List.hd (List.rev opn.t_opn_path)) '#'))^")"]); 


				opn.t_opn_rank <- i; i+1) 0 opns_sorted)) schedules end;
	    (*      ps "Data :";
		    Hashtbl.iter (fun data l -> ps ((name_of_data data)^(List.fold_left (fun s (operator,opn,prt,eefs) -> s^" and "^(name_of_operator operator)^" "^(name_of_data (opn,prt))) " on " l))) datas;*)
	    (*            ps "Schedules :";
			  Hashtbl.iter (fun oprname (_,oprschedule) ->
			  ps oprname;
			  let opns = Hashtbl.fold (fun _ (cond_opns,_) opns -> opns@cond_opns) oprschedule [] in
			  List.iter (fun (opn,_,_) -> ps (name_of_operation opn)) opns) schedules;*)
	    
	    graph, (Hashtbl.create 0)
	with exn ->
	  Progress_box.close progress_box; raise exn
       
	
       
	

let schedule_view_info adequation_graph =
  let operation_title opn = name_of_operation opn in
  let conv_cond condition = match condition with
  | None, None -> "",1
  | (Some (opn,prt)),(Some vl) -> (identifier_of_operation opn)^"."^prt.t_prt_name,vl
  | _ -> raise (Failure "Adequation.schedule_view_info : error 1") in
  let conv_cond_list cl = List.map conv_cond cl in
  let conv_opn opn = 
    let rank = match opn.t_opn_class with
      | Communication (Send _) -> opn.t_opn_rank/2 (* Each communication takes 2 ranks, one for Receive & one for Send *)
      | _ -> opn.t_opn_rank in
      (identifier_of_operation opn,operation_title opn,opn.t_opn_esfs,eefs opn,(conv_cond_list opn.t_opn_condition),rank) in
  let f schedule operation = match operation.t_opn_class with
  | Communication (Receive _) -> schedule
  | _ ->
      let operator = name_of_operator (pi operation) in
      let constants,operations = List.assoc operator schedule in
      let operator_schedule = match is_constant operation with
      | true -> (conv_opn operation)::constants,operations
      | false -> constants,(conv_opn operation)::operations in
      (operator,operator_schedule)::(List.remove_assoc operator schedule) in

  let oprs,mda = Architecture.operators_list !archilib !archiname,Architecture.media_list !archilib !archiname in
  let oprs = List.map (function n -> n,([],[])) (oprs@mda) in
  let opns = list_of_hashtbl adequation_graph in
  let schedules = List.fold_left f oprs opns in
  let preds = List.fold_left (function preds -> function opn -> match opn.t_opn_class with
  | Communication (Receive _) -> preds
  | _ ->
      let preds_opn = List.map (function p -> match p.t_opn_class with
      | Communication (Receive _) -> List.hd (predecessors p)
      | _ -> p) (predecessors opn) in
      (identifier_of_operation opn,(List.map identifier_of_operation preds_opn))::preds) [] opns in
  let succs = List.fold_left (function succs -> function opn -> match opn.t_opn_class with
  | Communication (Receive _) -> succs
  | Communication (Send _) ->
      let send_succs = List.map identifier_of_operation (List.concat (List.map successors (successors opn))) in
      (identifier_of_operation opn,send_succs)::succs
  | _ -> (identifier_of_operation opn,(List.map identifier_of_operation (successors opn)))::succs) [] opns in
  schedules,preds,succs

let string_of_schedule () =
  let indent = " " in
  let string_of_operation operation esfs eefs cond =
    match operation.t_opn_class with
    | Communication (Receive _) -> ""
    | _ -> let cond_string = match cond with
      | [(None,None)] -> ""
      | l -> " ("^(string_of_condlist l)^")" in
      let time_string = (match is_constant operation with
			 | true -> ": const"
			 | false -> ": "^(string_of_float esfs)^" .. "^(string_of_float eefs)) in
	indent^(identifier_of_operation operation)^time_string^cond_string^";\n" in
  let string_of_operator opr =
    match opr with
    | Operator o -> "operator "^o.oprref_name
    | Media m -> "media "^m.mdaref_name in
  let string_of_oprschedule oprschedule =
    let opns = Hashtbl.fold (fun _ (cond_opns,_) opns -> opns@cond_opns) oprschedule [] in
    let opns_sorted = List.stable_sort (function opn1,_,_ -> function opn2,_,_ -> compare opn1.t_opn_rank opn2.t_opn_rank) opns in
      List.fold_left (fun s (operation,esfs,eefs) ->
			s^(string_of_operation operation esfs eefs operation.t_opn_condition)) "" opns_sorted in
    Hashtbl.fold (fun _ (opr,oprschedule) total ->
		    total^(string_of_operator opr)^":\n"^(string_of_oprschedule oprschedule)) schedules "schedules:\n"





let pretty_conv_graph adequation_graph =
  let operation_title opn = name_of_operation opn in
  let conv_cond condition = match condition with
  | None, None -> "",1
  | (Some (opn,prt)),(Some vl) -> (identifier_of_operation opn)^"."^prt.t_prt_name,vl
  | _ -> raise (Failure "Adequation.pretty_conv_graph : error 1") in
  let conv_cond_list cl = List.map conv_cond cl in
  let conv_opn opn = 
    let rank = match opn.t_opn_class with
      | Communication (Send _) -> opn.t_opn_rank/2 (* Each communication takes 2 ranks, one for Receive & one for Send *)
      | _ -> opn.t_opn_rank in
      (identifier_of_operation opn,operation_title opn,opn.t_opn_esfs,eefs opn,(conv_cond_list opn.t_opn_condition),rank) in
  let f schedule operation = match operation.t_opn_class with
  | Communication (Receive _) -> schedule
  | _ ->
      let operator = name_of_operator (pi operation) in
      let constants,operations = List.assoc operator schedule in
      let operator_schedule = match is_constant operation with
      | true -> (conv_opn operation)::constants,operations
      | false -> constants,(conv_opn operation)::operations in
      (operator,operator_schedule)::(List.remove_assoc operator schedule) in

  let oprs,mda = Architecture.operators_list !archilib !archiname,Architecture.media_list !archilib !archiname in
  let oprs = List.map (function n -> n,([],[])) (oprs@mda) in
  let opns = list_of_hashtbl adequation_graph in
  let schedules = List.fold_left f oprs opns in
  let preds = List.fold_left (function preds -> function opn -> match opn.t_opn_class with
  | Communication (Receive _) -> preds
  | _ ->
      let preds_opn = List.map (function p -> match p.t_opn_class with
      | Communication (Receive _) -> List.hd (predecessors p)
      | _ -> p) (predecessors opn) in
      (identifier_of_operation opn,(List.map identifier_of_operation preds_opn))::preds) [] opns in
  let succs = List.fold_left (function succs -> function opn -> match opn.t_opn_class with
  | Communication (Receive _) -> succs
  | Communication (Send _) ->
      let send_succs = List.map identifier_of_operation (List.concat (List.map successors (successors opn))) in
      (identifier_of_operation opn,send_succs)::succs
  | _ -> (identifier_of_operation opn,(List.map identifier_of_operation (successors opn)))::succs) [] opns in
  schedules,preds,succs

let string_of_schedule () =
  let indent = " " in
  let string_of_operation operation esfs eefs cond =
    match operation.t_opn_class with
    | Communication (Receive _) -> ""
    | _ -> let cond_string = match cond with
      | [(None,None)] -> ""
      | l -> " ("^(string_of_condlist l)^")" in
      let time_string = (match is_constant operation with
			 | true -> ": const"
			 | false -> ": "^(string_of_float esfs)^" .. "^(string_of_float eefs)) in
	indent^(identifier_of_operation operation)^time_string^cond_string^";\n" in
  let string_of_operator opr =
    match opr with
    | Operator o -> "operator "^o.oprref_name
    | Media m -> "media "^m.mdaref_name in
  let string_of_oprschedule oprschedule =
    let opns = Hashtbl.fold (fun _ (cond_opns,_) opns -> opns@cond_opns) oprschedule [] in
    let opns_sorted = List.stable_sort (function opn1,_,_ -> function opn2,_,_ -> compare opn1.t_opn_rank opn2.t_opn_rank) opns in
      List.fold_left (fun s (operation,esfs,eefs) ->
			s^(string_of_operation operation esfs eefs operation.t_opn_condition)) "" opns_sorted in
    Hashtbl.fold (fun _ (opr,oprschedule) total ->
		    total^(string_of_operator opr)^":\n"^(string_of_oprschedule oprschedule)) schedules "schedules:\n"

end

























