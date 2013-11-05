(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            Hamoudi Kalla                              *)
(*			                                                 *)
(*                  Projet Pop Art, INRIA Rhône-Alpes                    *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)


(* WARNING : be cautious when changing this module. *)
(* There are very tough performance requirements, especially when dealing with communications. *)

module Make = functor (Progress_box : Progress_box.PROGRESS_BOX_SIG) -> 

struct 

  open Types
  open Adequationtypes 
  open Adequation_core

  type failure_type = { mutable npf : int;    mutable nmf : int;}
      (* NPF = Number of Processor Failures and NMF = Number of communication media Failures *)

  let old_len = ref 0.
  let old_rel = ref 1.
  let file_name = ref ""
  let rate_replication = ref 0.
  let failures_rate = Hashtbl.create 30


  let debug_level = 0

  let path = ref []
 
  let failures = ref {npf=0; nmf=0}      

  let get_npf f = (match !failures with
  | {npf=x; nmf=y} -> x)

  let get_nmf f = (match !failures with
  | {npf=x; nmf=y} -> y)

  let is_SamMP = ref false

  let is_SamPP = ref false

  let len = ref 0.

  let exclusives_SamMP = Hashtbl.create 10

  let exclusives_routing = Hashtbl.create 10

  let exclusives_routes = ref []

  let exclusive_route = ref []

  let exclusive_rtopn = ref []

  let split_data = ref false

  let originals_operations = Hashtbl.create 30

  let replicas_operations = Hashtbl.create 10

  let route_table = Hashtbl.create 30

let initialize_failure_rates_table  file_name  =     
  let (algo_lib,algo_name,_),(archilib,archiname) = 
     Application.algo_main_get (), Application.archi_main_get () in
     let all_operators = Application.operators_list archilib archiname in
        let name_fileRel = (String.sub file_name 0 (String.rindex file_name '.'))^".rel" in
        let fileRel = open_in name_fileRel  in
        let ch = ref "" in
        List.iter (fun   opr -> ch := (input_line fileRel); 
           Hashtbl.add failures_rate (name_of_operator opr) (float_of_string !ch)) all_operators;
        close_in fileRel

(*****************************************************************)
(************* AFFICHAGE PREDS AND SUCCS OFF OPERATIONS **********)
(*****************************************************************)
  let ps_dpds operation_AN = 
    ps (List.fold_left (function s -> function dpd -> s^(name_of_operation dpd.t_dpd_sopn)^" , ") ("preds dpd "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_dependences_predecessors));
    ps (List.fold_left (function s -> function dpd -> s^(name_of_operation dpd.t_dpd_dopn)^" , ") ("succs dpd "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_dependences_successors))

(*****************************************************************)
(************* AFFICHAGE PREDS AND SUCCS OFF OPERATIONS **********)
(*****************************************************************)
let ps_preds operation_AN = 
  ps (List.fold_left (function s -> function sopn -> s^(name_of_operation sopn)^" , ") ("preds : "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_predecessors));
  ps (List.fold_left (function s -> function dopn -> s^(name_of_operation dopn)^" , ") ("succs : "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_successors))

(* psr *)
let name_of_og (o,g) = 
  (name_of_operator o)^(match g with | Some g -> "."^g.gte_name | _ -> "")

let psr r = match r with
| [] -> ()
| hd::tl -> 
    ps (List.fold_left
	  (function s -> function og -> s^"->"^(name_of_og og))
	  (name_of_og hd) tl)


  (* Calculate the list of shortest exclusive routes between o1 and o2 *)
  let routes o1 o2 exclusives_routes =
    (*ps ("Search route from "^(name_of_operator o1)^" to "^(name_of_operator o2));  
    ps (List.fold_left (fun s (opr) -> s^" "^(name_of_operator opr)) "Exclusives routes =  " exclusives_routes);*) 
    let no_crossing r1 r2 = 
      let r1 = remove_copies (List.map fst r1)
      and r2 = remove_copies (List.map fst r2) in
	(intersection r1 r2)=[] in
    let archilib,archiname = Application.archi_main_get () in
    let links o = Architecture.links archilib archiname o in
    let rec r o1 o2 route =
      match Hashtbl.mem route_table (o1,o2) with
	| true -> (* this route already calculated *)
	    let subroutes = Hashtbl.find route_table (o1,o2) in
	      List.filter (no_crossing route) subroutes
	| false ->
	    let links = List.filter (function (og1,og2) -> no_crossing [og2] route) (links o1) in
              (* links : list of links from o1 not in ll *)
	    let subroutes = List.map (function (og1,((opr2,_) as _og2)) as lnk -> lnk,(r opr2 o2 (og1::route))) links in
	    let subroutes = List.concat (List.map (function (og1,og2),rlist -> List.map (function r -> og1::og2::r) rlist) subroutes) in
              (* subroutes : list of routes from o1 to o2 *)
	      (*let subroutesmin = list_min_elements List.length subroutes 0 in*)
	    let subroutesmin = subroutes in
              (* list of shortest routes from o1 to o2 *)
	      List.map remove_copies subroutesmin in
    let routes = (match Hashtbl.mem route_table (o1,o2) with
		    | true  -> (Hashtbl.find route_table (o1,o2))
		    | false -> let routes = r o1 o2 [] in
			Hashtbl.add route_table (o1,o2) routes;
			Hashtbl.add route_table (o2,o1) (List.map List.rev routes);routes) in
        (*List.iter psr routes;*)
	(*ps (List.fold_left (fun s (opr) -> s^" "^(name_of_operator opr)) "for exclusives routes =  " exclusives_routes);*)
    let routes = List.filter (function route ->     
                                let opr_route = (List.map fst route) in (intersection opr_route exclusives_routes)=[]) routes in
      (*ps ("find route for "^(string_of_int (List.length (list_min_elements List.length routes 0))));*) 
    let route = try List.hd (list_min_elements List.length routes 0) with  _ -> raise (Failure ("No "^(string_of_int (!failures.nmf+ !failures.npf +1))^" disjoint paths from "^(name_of_operator o1)^" to "^(name_of_operator o2)^" !!")) in  
      (*ps ("Find route for "^(name_of_operator o1)^" -> "^(name_of_operator o2));  
      ps (List.fold_left (fun s (opr) -> s^" "^(name_of_operator opr)) "for exclusives routes =  " exclusives_routes);*)
      (*print_string "Found route is : ";List.iter psr [route];*)  
      route  

  (* Remove an operation from a graph *)
  let remove_operation graph operation =
    List.iter dependence_remove operation.t_opn_dependences_predecessors;
    List.iter dependence_remove operation.t_opn_dependences_successors;
    Hashtbl.remove graph (identifier_of_operation operation)

  (* get k values from a list *)     
  let rec keep_npf_elements k list = 
    if (k=0)
    then []
    else match list with
      | [] -> []
      | v1::slist -> v1::(keep_npf_elements (k-1) slist);;

  (*  Return the number of operation copies which are scheduled *)    
  let replicas_size opn =
    match (Hashtbl.mem replicas_operations (name_of_operation opn)) with  
      | true  -> (List.length (Hashtbl.find replicas_operations (name_of_operation opn)))+1
      | false -> 1
	  
  (*  test if operation opn is original  *)  
  let is_original opn = 
    let name = name_of_operation opn in
      try
	if (String.contains name '#')
	then false
	else true
      with _  -> true


  (* Check if opn is a routing operation *) 
  let is_routing_operation opn =
    let name = name_of_operation opn in
      try
	if (String.contains name '%')
	then true
	else false
      with _  -> true;;


  (*   get original operation name *)
  let get_original opn =
    let name = name_of_operation opn in
      try 
	let original =(String.sub name 0 (String.rindex name '#')) in (String.sub original 0 (String.rindex original '_'))
      with _ -> name

  (*   get original src opn name *)
  let get_original_dpd opn =
    let name = name_of_operation opn in
      try 
        let name = (String.sub name 0 (String.rindex name ')')) in
	let original = (String.sub name 0 (String.rindex name '#')) in (String.sub original 0 (String.rindex original '_'))
      with _ -> name

  (*   get original port name *)
  let get_original_prt prt =
    let name = prt.t_prt_name in
      try 
	let original =(String.sub name 0 (String.rindex name '#')) in (String.sub original 0 (String.rindex original '_'))
      with _ -> name

  (*   get original name *)
  let get_original_string name_opn =
    try 
      let original =(String.sub name_opn 0 (String.rindex name_opn '#')) in (String.sub original 0 (String.rindex original '_'))
    with _ -> name_opn

  (*  update the list of operation copies : add *)    
  let update_replicas replica  = 
    let original = (get_original replica) in
    let replicas = (match (Hashtbl.mem replicas_operations original) with
       	              | true  -> replica::(Hashtbl.find replicas_operations original)  
		      | false -> [replica]) in
      (Hashtbl.replace replicas_operations original replicas)

 (*  update the list of operation copies : add *)    
  let update_exclusives_routing sopn_dopn = 
    let exclusives = (match (Hashtbl.mem exclusives_routing sopn_dopn) with
       	              | true  -> !exclusive_rtopn@(Hashtbl.find exclusives_routing sopn_dopn)  
		      | false -> !exclusive_rtopn) in
      (Hashtbl.replace exclusives_routing sopn_dopn exclusives)

  (* update the list of operation copies : remove *)    
  let remove_replicas replica  = 
    let original = (get_original replica) in
    let replicas = (match (Hashtbl.mem replicas_operations original) with
       	              | true  -> List.tl (Hashtbl.find replicas_operations original)  
		      | false -> (Hashtbl.find replicas_operations original)) in
      (Hashtbl.replace replicas_operations original replicas)

  (*  update exclusive SamMP *)  
  let update_exclusives_SamMP sopn sprt bus =
    let exc_bus = (match (Hashtbl.mem exclusives_SamMP (sopn,sprt)) with
       	             | true  -> bus::(Hashtbl.find exclusives_SamMP (sopn,sprt))  
		     | false -> [bus]) in
      (Hashtbl.replace exclusives_SamMP (sopn,sprt) exc_bus)

  (* Place the communication of operation_src.port_src between operator_src and operator_dst after instant t0 *)
  let place_com (data,(operator_src,operation_src,port_src,t0)) operation_dst port_dst operator_dst graph tcond dpdclass cond place status =  
    (* place coms in the case of SamMP *)  
    let rec place_SamMP_com_aux sopn sprt eefs place status opr_src =  
      (*ps(" from "^(name_of_operator opr_src)^" to "^(name_of_operator operator_dst)^" placing "^(name_of_operation sopn));*)
      match (opr_src =@ operator_dst) with     
	| true  -> sopn,sprt,eefs,eefs
	| false ->
    	    let lnks = shortest_links opr_src operator_dst in     
	      (*ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^" "^(name_of_operator opr)) "All Possible next oprs are " lnks);*)
	    let exc_SamMP = (try (Hashtbl.find exclusives_SamMP ((get_original sopn),(get_original_prt sprt))) with  _ -> []) in
	      (*ps (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) "All forbidden oprs are " exc_SamMP);*)
	    let lnks = List.filter (function (_,(bus,_))-> not (List.mem bus exc_SamMP)) lnks in 
	      (*ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^" "^(name_of_operator opr)) "Possible next oprs are " lnks);*)
	    let best_lnk = find_best_link lnks [] max_float sopn sprt eefs data graph cond status dpdclass operator_dst false in      
	      (*ps (List.fold_left (fun s (opr,_) -> s^" "^(name_of_operator opr)) "best links is " best_lnk);*)
	    let best_bus = fst (List.hd (try (List.tl best_lnk) with  _ -> raise (Failure "Exclusive SamMP : best link !!"))) in 
	      if(is_media best_bus) then update_exclusives_SamMP (get_original sopn) (get_original_prt sprt) best_bus;
	      (*This time, place it if place is true*)
	      let (opn,prt,eefs),next_opr = reliable_place_link best_lnk place sopn sprt eefs data graph cond status dpdclass in
		place_SamMP_com_aux opn prt eefs place status next_opr in

      (* place coms in the case of SamPP *)
    let rec place_SamPP_com_aux sopn sprt eefs place status opr_src route = 
      match (opr_src=@operator_dst) with
	| true  -> sopn,sprt,eefs,sopn.t_opn_weefs
	| false -> match route with 
	    | [] -> raise (Failure "place_copie_com_aux no exclusive route ...") 
	    | og1::og2::og3::rest -> let best_lnk = (og1::og2::[og3]) in                 
	      let (opn,prt,eefs),next_opr = 
                reliable_place_link best_lnk place sopn sprt eefs data graph cond status dpdclass in
                place_SamPP_com_aux opn prt eefs place status next_opr rest 
	    |  _ -> raise (Failure "place_copie_com_aux no exclusive route")  in

    let eefs = max t0 tcond in
    let _weefs = operation_src.t_opn_weefs in
    let opnpred,prtpred,eefs,weefs =
      (match ((!is_SamPP)) with
	 | true  ->  
	     let route = (routes operator_src operator_dst !exclusives_routes) in 
	       exclusive_route := List.map fst route;  
	       place_SamPP_com_aux operation_src port_src eefs place status operator_src route
	 | false ->  place_SamMP_com_aux operation_src port_src eefs place status operator_src) in 
      (match place with
	   (* Add dependence between last opn created and dopn (operation which required the communication) *)
	 | true  ->  dependence_add opnpred prtpred operation_dst port_dst dpdclass cond status
	 | false -> ());
      eefs,weefs


  (* add passively backups dependencies in the case of SamMP *)
  let place_backups_sammp_com f copies sopn sprt dpdclass cond graph = 
    List.iter 
      (function dpd ->
         let copie = dpd.t_dpd_sopn in
	 let oprs_srcs_copie = (Hashtbl.find datas (sopn,sprt)) in
	 let _media,_receive_opn,_sprt,_tcond = List.hd (list_min_elements f oprs_srcs_copie 0)  in
	 let _send_opn = (List.hd _receive_opn.t_opn_predecessors) in
	 let _dprt = (List.hd _send_opn.t_opn_ports) in
	 let _dpd = {t_dpd_sopn=copie;t_dpd_sprt=sprt;t_dpd_dopn=_send_opn;t_dpd_dprt=_dprt;
		     t_dpd_backups=None;t_dpd_routing=None;t_dpd_class=dpdclass;t_dpd_condition=cond;t_dpd_status=true} in  
	     dependence_remove _dpd; 
	     dependence_add copie sprt _send_opn _dprt dpdclass cond true;
             update_senders (operator_of_operator_class (pi copie)) _send_opn graph;
      ) copies 

 
  (* there is a bug for implementing exclusive routing operation : see sampp_fault_tolerance.sdx for npf=0 and nmf=2 *)
  (*    this case is impossible but the heuristic give a solution *)
  (* schedule actively backups dependencies in the case of SamPP 
  let place_backups_sampp_com f copies sopn sprt dopn dprt operator_dst graph tcond dpdclass cond status =
    exclusives_routes := (pi sopn) :: !exclusives_routes;
    List.iter
      (function copie ->
	 let sprt = List.find (function prt -> prt.t_prt_name=sprt.t_prt_name && prt.t_prt_dir=Port.Out) copie.t_opn_ports in
	 let oprs_srcs_copie = (Hashtbl.find datas (copie,sprt)) in
	 let oprs_srcs_copie = List.filter (function opr,_,_,_ -> not (is_media opr)) oprs_srcs_copie in 
	 let _opr,_opn,_sprt,_tcond = List.hd (list_min_elements f oprs_srcs_copie 0)  in
	   dependence_add copie sprt dopn dprt dpdclass cond true;
	   exclusives_routes := !exclusive_route @ !exclusives_routes;
	   exclusives_routes := List.filter (function opr -> (pi copie)!=opr) !exclusives_routes;
	   exclusives_routes := List.filter (function opr -> (pi dopn)!=opr) !exclusives_routes;
	   ignore(place_com ((copie,sprt),(_opr,_opn,_sprt,_tcond)) dopn dprt operator_dst graph tcond dpdclass cond true status);
	   dependence_remove (List.find (function dpd -> dpd.t_dpd_dopn = dopn) copie.t_opn_dependences_successors);
	   exclusives_routes := (pi copie) :: !exclusives_routes;
      ) copies *)

  let place_copies_coms_sampp f org_opn operator_dst graph status tcond dpds best_eefs best_weefs =
    debug_ps 2 debug_level ("..Place copie "^(name_of_operation org_opn));
    let best_eefs = ref best_eefs in
    let best_weefs = ref best_weefs in 
      exclusives_routes := (pi org_opn) :: !exclusives_routes;      
      List.iter 
	(function dpd ->
	   let copie,sprt,dopn,dprt,dpdclass,cond = 
	     dpd.t_dpd_sopn,dpd.t_dpd_sprt,dpd.t_dpd_dopn,dpd.t_dpd_dprt,dpd.t_dpd_class,dpd.t_dpd_condition in
	     debug_ps 2 debug_level ("Place copie "^(name_of_operation copie)^" --> "^(name_of_operation dopn));
	     let oprs_srcs_copie = [(pi copie),copie,sprt,(eefs copie)](*Hashtbl.find datas (copie,sprt)*) in 
	     let oprs_srcs_copie = List.filter (function opr,_,_,_ -> not (is_media opr)) oprs_srcs_copie in 
	     let _opr,_opn,_sprt,_tcond = List.hd (list_min_elements f oprs_srcs_copie 0)  in
	       exclusives_routes := !exclusive_route @ !exclusives_routes;
	       exclusives_routes := List.filter (function opr -> ((pi copie)<>@opr)) !exclusives_routes;
	       exclusives_routes := List.filter (function opr -> ((pi dopn)<>@opr)) !exclusives_routes;
	       let new_eefs,new_weefs = 
		 place_com ((copie,sprt),(_opr,_opn,_sprt,_tcond)) dopn dprt operator_dst graph tcond dpdclass cond true status in
		 best_eefs := min new_eefs !best_eefs;
		 best_weefs := max new_weefs !best_weefs;
		 exclusives_routes := (pi copie) :: !exclusives_routes;
	) dpds;
      !best_eefs,!best_weefs 


  (* Place in graph the primary communication corresponding to dependence dpd *) 
  let place_communications graph status tcond dpds rt_opns =
    debug_ps 2 debug_level 
      (List.fold_left (fun s dpd -> s^(name_of_operation dpd.t_dpd_sopn)^" --> "^(name_of_operation dpd.t_dpd_dopn)^" , ") 
	 ("place dpds  = ") dpds);
    exclusive_route := [];exclusives_routes := [];
    let dpd = List.hd dpds in    
    let copies_dpds = (try  List.tl dpds with _ -> []) in
    debug_ps 2 debug_level 
      (List.fold_left (fun s dpd -> s^(name_of_operation dpd.t_dpd_sopn)^" --> "^(name_of_operation dpd.t_dpd_dopn)^" , ") 
	 ("copies dpds  are = ") copies_dpds);
    let sopn,sprt,dopn,dprt,dpdclass,cond = 
      dpd.t_dpd_sopn,dpd.t_dpd_sprt,dpd.t_dpd_dopn,dpd.t_dpd_dprt,dpd.t_dpd_class,dpd.t_dpd_condition in
      (*ps ("Place "^(name_of_operation sopn)^" --> "^(name_of_operation dopn));*)
      debug_ps 2 debug_level ("\n place_primary_communication..."^(name_of_data (sopn,sprt))^" -> "^(name_of_data (dopn,dprt))); 
      debug_ps 2 debug_level (" place_primary_communication..."^(name_of_operator (pi sopn))^" -> "^(name_of_operator (pi dopn)));  
      (* if SAMPP architecture : compute exclusives routes to schedule (sopn.sprt --> dopn.dprt) *)
      (*  exclusives routes are all oprs that implement sopn copies *)       
      (*let copies = (match (!failures.npf>=1) with
	| true  -> Hashtbl.find replicas_operations (get_original sopn)
	| false -> [] ) in 
	let _,dpd_rt_opns = (try (List.find (function (sop,_) -> sop=sopn) rt_opns) with _ -> sopn,[]) in
	exclusives_routes := (List.map (function copie -> pi copie) (copies@dpd_rt_opns));*)
       exclusives_routes := (List.map (function {t_dpd_sopn=sopn} -> pi sopn) copies_dpds); 
      (* Calculate list of operators containing data *)
      let data,operator_dst  = (sopn,sprt),(pi dopn) in
      let operators_src_list = (Hashtbl.find datas data) in
      let operators_src_list = 
	(List.filter 
	   (function (operator_src,opn,port_src,t0) ->
	      match conditioned_operation opn with
		| false -> true
		| true -> List.for_all(function c-> List.mem c dpd.t_dpd_condition) opn.t_opn_condition)operators_src_list) in
      let operators_src_list = 
	(match !is_SamPP with
	   | true -> 
	       List.filter 
	       (function (operator_src,opn,port_src,t0) -> 
		  ((List.mem operator_src ((pi dopn)::(pi sopn)::
					     !exclusives_routes))&&(not (is_communication opn)))) operators_src_list
	   | false -> operators_src_list) in 

      let f (o,_,_,_) = match o with
	| Media mda -> mda.mdaref_links2.((operator_of_operator_class operator_dst).oprref_id)
	| Operator opr -> opr.oprref_links2.((operator_of_operator_class operator_dst).oprref_id) in
      let operators_src_list = (list_min_elements f operators_src_list 0) in
	(* operators_src_list : list of operators containing data and nearest of operator_dst *)
      let s = List.fold_left (function s -> function (operator_src,operation_src,port_src,t0) -> 
				s^(name_of_operator operator_src)^" ") 
		(" (looking for cond"^(string_of_condlist dpd.t_dpd_condition)^") ") operators_src_list in
	debug_ps 4 debug_level ((name_of_data data)^" on "^s);    
	(* Place the communication between each operator and the destination to determine the best route *)
	(*     let f e = place_com (data,e) dopn dprt operator_dst dpdclass cond false status in *)
	let best_operator_src = ref (try (List.hd operators_src_list) 
				     with  _ -> raise (Failure "Add disjoints links to architecture!!!"))  in 
	  (*((pi sopn),sopn,sprt,tcond)*)(*List.hd (list_min_elements f operators_src_list 0.)*) 	  
	  (*  let (opr_,sopn_,sprt_,_)= !best_operator_src in
	      ps("from ... "^(name_of_operator opr_)^" to "^(name_of_operator operator_dst)^" placing "^(name_of_operation sopn)); *)    
	  (* Place the communication *)

	let best_eefs,best_weefs = 
	  place_com (data, !best_operator_src) dopn dprt operator_dst graph tcond dpdclass cond true status in    
	  (* add passively backups dependencies in the case of SamMP 
	     if ((!is_SamMP)&&(status)&&(!failures.npf>=1)) 
	     then place_backups_sammp_com f copies sopn sprt dpdclass cond;*)             
	  (* schedule actively backups dependencies in the case of SamPP *)

          if (!is_SamPP) 
	  then place_copies_coms_sampp f sopn operator_dst graph status tcond copies_dpds best_eefs best_weefs
          else if ((status)&&(!is_SamMP)&&(!failures.npf>=1))
          then begin place_backups_sammp_com f copies_dpds sopn sprt dpdclass cond graph;
                    best_eefs,best_eefs 
              end
          else best_eefs,best_eefs
(*place_copies_coms_sampp f copies sopn sprt dopn dprt operator_dst graph tcond dpdclass cond status; *)
	    (* schedule actively routing operations dependencies in the case of SamPP          
	    if ((!is_SamPP)&&(status)&&(!failures.nmf>=1)&&(not (is_routing_operation dopn)))
	      (*if ((!is_SamPP)&&(!failures.nmf>=1)&&(not (is_routing_operation dopn)))*)
            then place_routing_com f copies sopn sprt dopn dprt operator_dst graph tcond dpdclass cond status rt_opns *)
          
      
  (* Remove inter dependences copies when intra dependences exists *)
  let delete_dependences_copies operation graph dpds_data status =
    (*let org_dpds = List.filter (fun {t_dpd_sopn=sopn} -> (is_original sopn)) dpds_data in*)
    let intra_dpds,inter_dpds = List.partition (fun {t_dpd_sopn=sopn} -> (pi sopn =@ pi operation)) dpds_data in
    let intra_orgs,intra_bks  = List.partition (fun {t_dpd_sopn=sopn} -> is_original sopn) intra_dpds in  
    let rm_bks = List.fold_left (fun lst dpd -> match dpd.t_dpd_backups with 
				   | None -> lst 
				   | Some dpd_bks -> dpd_bks@lst) [] intra_orgs in
    let rm_routing = List.fold_left (fun lst dpd -> match dpd.t_dpd_routing with 
				   | None -> lst 
				   | Some rts -> rts@lst) [] intra_orgs in
    let rm_orgs_bks = List.fold_left (fun lst intra_bk -> 
					let bk = intra_bk.t_dpd_sopn in
					let name_bk = get_original bk in  
					let orgs_bks = List.filter (fun {t_dpd_sopn=org} -> (get_original org)=name_bk) inter_dpds in       
					  orgs_bks@lst  
				     ) [] intra_bks in
    let rm_dpds = (rm_bks@rm_orgs_bks) in
    let filtered_inter_dpds = List.fold_left (fun lst dpd -> 
						if List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=$dpd.t_dpd_sopn) rm_dpds 
						then lst else dpd::lst) [] inter_dpds (*dpds_data*) in
      (match status with
	 |false -> () 
	 |true  -> 
	    (*ps (List.fold_left (function s -> function dpd -> s^(name_of_operation dpd.t_dpd_sopn)^" , ") 
	      ("to remove  "^(name_of_operation operation)^" = ") rm_dpds); 
	    ps (List.fold_left (function s -> function opn -> s^(name_of_operation opn)^" , ") 
	      ("to remove  "^(name_of_operation operation)^" = ") rm_routing); *)

            List.iter (fun rt -> Adequationtypes.operation_remove graph rt) rm_routing;
       
	    operation.t_opn_dependences_predecessors <-
	    List.filter (fun {t_dpd_sopn=sopn} -> 
			   not (List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=$sopn) rm_dpds)  
		 	) operation.t_opn_dependences_predecessors; 
	     operation.t_opn_predecessors <-
	     List.filter (fun sopn -> 
			    not (List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=$sopn) rm_dpds) 
			 ) operation.t_opn_predecessors; 
	     List.iter (fun {t_dpd_sopn=sopn} -> 
			  sopn.t_opn_dependences_successors <-
			  List.filter (fun {t_dpd_dopn=dopn} -> dopn<>$operation) sopn.t_opn_dependences_successors;  
			  sopn.t_opn_successors <- List.filter (fun dopn ->  dopn<>$operation) sopn.t_opn_successors;  
		       ) rm_dpds; 
      );
      let inter_orgs,inter_bks = List.partition (fun {t_dpd_sopn=sopn} -> (is_original sopn)) filtered_inter_dpds in
	(intra_orgs@intra_bks),inter_orgs,inter_bks


   (* Builds operation predecessors coms *)
  let make_coms operation graph status rt_opns =
    Hashtbl.clear exclusives_SamMP;
    debug_ps 3 debug_level ("making comm "^(name_of_operation operation));
    let make_one_com tcond dpds = 
      let dpds = 
	List.filter 
	  (fun dpd -> not ((is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn))) dpds in
      let dpd = List.hd dpds in 
	match (pi dpd.t_dpd_sopn =@ pi operation) with
	  | true  -> 
	      eefs dpd.t_dpd_sopn,(dpd.t_dpd_sopn).t_opn_weefs
	  | false -> 
	      let best_eefs,worst_eefs = 
		place_communications graph status tcond dpds rt_opns in 
		(match status with 
		   | true  -> List.iter dependence_remove dpds
		   | false -> ());
		best_eefs,worst_eefs in 
    let new_make_one_com tcond dpd =
      make_one_com tcond [dpd] in

    let dpds_data,dpds_condition = 
      List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) 
	operation.t_opn_dependences_predecessors in                      (*not(is_routing_operation sopn)) dpds_data in *) 
    let dpds_data,dpds_comms = List.partition (fun {t_dpd_sopn=sopn}-> not(is_communication sopn)) dpds_data in   
    let dpds_data,dpds_routing = List.partition (fun {t_dpd_sopn=sopn}-> not(is_routing_operation sopn)) dpds_data  in
      debug_ps 1 debug_level ((name_of_operation operation)^" coms are : "
			      ^(List.fold_left (fun s dpd -> s^", "^(name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" to "
						  ^(name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))) "" dpds_data));
      let tc = make_cond_dpds (new_make_one_com) dpds_condition in 
	(* schedule min eefs dpd first *)
      let dpds_data = 
	List.filter 
	  (fun dpd -> not ((is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn))) dpds_data in
      let intra_dpds,inter_orgs_dpds,inter_bks_dpds =
	delete_dependences_copies operation graph dpds_data status in
      let eefs_intra,weefs_intra =
	List.fold_left 
	  (fun (eefs,weefs) intra_dpd ->
             let tmp_eefs,tmp_weefs = (make_one_com tc [intra_dpd]) in
               (max eefs tmp_eefs),(max weefs tmp_weefs)
	  ) (0.,0.) intra_dpds in

      let eefs_inter,weefs_inter = 	
	List.fold_left (fun (eefs,weefs) org_dpd -> 
                          let routing_opns = 
			    (match org_dpd.t_dpd_routing with
			       | None -> []
                               | Some rt_opns -> rt_opns) in    
			  let inter_dpds = 
			    org_dpd
			    ::(List.filter 
				 (fun {t_dpd_sopn=bk}-> (get_original bk)=(get_original org_dpd.t_dpd_sopn)
				 ) inter_bks_dpds)
                            @(List.filter 
				(fun {t_dpd_sopn=rt}-> 
				   (List.exists (fun rt_opn -> (name_of_operation rt)=(name_of_operation rt_opn)) routing_opns)
				) dpds_routing) in
			  let tmp_eefs,tmp_weefs = (make_one_com tc inter_dpds) in
			    (max eefs tmp_eefs),(max weefs tmp_weefs)
		       ) (eefs_intra,weefs_intra) inter_orgs_dpds in

      (*let eefs_routing,weefs_routing =
	List.fold_left (fun (eefs,weefs) org_dpd -> 
	let tmp_eefs,tmp_weefs = (make_one_com tc [org_dpd]) in
	(max eefs tmp_eefs),(max weefs tmp_weefs) 
	) (eefs_inter,weefs_inter) dpds_routing in*)

      let operator = pi operation in   
	debug_ps 3 debug_level ("compute esfs for "^(name_of_operation operation));
	let tcond, worst_tcond = 
          match (!is_SamPP) with
            | true  -> fault_esfs_cond operation operator eefs_inter weefs_inter operation.t_opn_condition
            | false -> let _esfs = esfs_cond operation operator eefs_inter operation.t_opn_condition in 
                       _esfs, _esfs in           
	  debug_ps 3 debug_level ("end making comm "^(name_of_operation operation));
	  tcond, worst_tcond  



  (* Build memory successor coms 
  let make_memory_succ_coms graph opn_eefs operation rt_opns=
    let dpds_data,dpds_condition = 
      List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) operation.t_opn_dependences_successors in
      ignore(make_cond_dpds 
	       (fun tc dpd -> 
		  match (pi operation = pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Port.Precedence_Port)  with
		    | true -> ()
		    | false -> ignore(place_communications graph true tc [dpd] rt_opns) dpds_condition));
      List.iter (fun dpd -> 
		   match (pi operation = pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Port.Precedence_Port)  with
		     | true -> ()
		     | false -> dependence_remove dpd) dpds_condition;
      List.iter (fun dpd -> 
		   match (pi operation =pi dpd.t_dpd_dopn) || (dpd.t_dpd_sprt.t_prt_class = Port.Precedence_Port)  with
		     | true -> ()
		     | false -> place_communications graph true opn_eefs [dpd] rt_opns;
			 dependence_remove dpd) dpds_data *)


  (* Returns operation esfs on operator on which it has been scheduled *)
  (* here we take into account the primary replica of each predecessor  
  let esfs operation =  
    let operator = pi operation in    
    let delta = delta operation.t_opn_class operator in   
    let tdpds = list_max_value dpd_EEFS operation.t_opn_dependences_predecessors  0. in 
    let tcond = esfs_cond operation operator tdpds operation.t_opn_condition in 
      (*ps ("esfs for "^(name_of_operation operation)^" is"^(string_of_float tcond)^" (dpdps :"^(string_of_float tdpds));*)
      tcond  *)

(* Schedules constants on all operators on which they will be needed (possible duplications) *)
    let place_constant graph progress_box =
      let rec place opn dpds = match dpds with
      | [] -> Adequationtypes.operation_remove graph opn
      | hd::tl ->
	  let opr = pi hd.t_dpd_dopn in
	  let dpds_opr_list,dpds_other_opr_list = List.partition (function {t_dpd_dopn=dopn} -> pi dopn =@ opr) dpds in
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
	  |	Port.Out, Port.Data_Port | Port.Out, Port.Init_Memory_Port -> update_datas opr (newopn,prt) newopn prt 0.
	  |	_,_ -> ()) newopn.t_opn_ports;
	  schedule newopn opr true 0. 0.;
	  List.iter (function dpd ->
	    let prt = List.find (function {t_prt_name=prtname;t_prt_dir=dir} -> prtname=dpd.t_dpd_sprt.t_prt_name && dir=Port.Out) newopn.t_opn_ports in
	    Adequationtypes.dependence_add newopn prt dpd.t_dpd_dopn dpd.t_dpd_dprt dpd.t_dpd_class dpd.t_dpd_condition dpd.t_dpd_status) dpds_opr_list;
	  let opns = hashtbl_filter (function o -> (not (is_constant o)) && ((execution_operator_of_operation o)=@@(operator_of_operator_class opr))) graph in
	  List.iter (function o -> o.t_opn_condition <- List.map (function (v,vl) -> (match v with
	  | Some (opncond,prtcond) -> (match opncond=$opn with
	    | true -> (Some (newopn,prtcond))
	    | false -> v)
	  | None -> v),vl) o.t_opn_condition) opns;
	  place opn dpds_other_opr_list in
      let constants = List.filter is_constant (list_of_hashtbl graph) in
      List.iter (function opn ->
	place opn opn.t_opn_dependences_successors;
	Progress_box.tick progress_box) constants


(* Initializes the route table, data available on operators, schedules and operator_constraints *)
  let fault_architecture_init graph = 
    architecture_init graph;
    Hashtbl.clear route_table;
    Hashtbl.clear originals_operations;     
    Hashtbl.clear replicas_operations;
    Hashtbl.clear exclusives_routing;
    let oprlist = List.map (Architecture.operator_reference !archilib !archiname ) (Architecture.operators_list !archilib !archiname) in
    let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
      List.iter (function o -> Hashtbl.add route_table (Operator o, Operator o) [[]]) oprlist; 
      List.iter (function m -> Hashtbl.add route_table (Media m, Media m) [[]]) mdalist 


(* Adds a dependence between sopn.sprt and dopn.dprt *)
  let backup_dependence_add sopn sprt dopn dprt dpdclass cond status =
    let dpd = {t_dpd_sopn=sopn;t_dpd_sprt=sprt;t_dpd_dopn=dopn;t_dpd_dprt=dprt;
	       t_dpd_backups= None;t_dpd_routing= None;t_dpd_class=dpdclass;t_dpd_condition=cond;t_dpd_status=status} in
      sopn.t_opn_dependences_successors <-
      dpd :: sopn.t_opn_dependences_successors;
      sopn.t_opn_successors <-
      dopn::sopn.t_opn_successors;
      dopn.t_opn_dependences_predecessors <-
      dpd :: dopn.t_opn_dependences_predecessors;
      dopn.t_opn_predecessors <-
      sopn::dopn.t_opn_predecessors;
      dpd

  (*  create a routing operation operation *)
  let create_copie_routing operation pos = 
    let name_copie = ("_"^(string_of_int pos)^"#") in   
    let rtopn = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values operation.t_opn_ports 
      	            [] [] operation.t_opn_condition None 0 0. true operation.t_opn_origin None operation.t_opn_xsc_name
                    operation.t_opn_referencing_alglib operation.t_opn_referencing_algname operation.t_opn_code_phases) in       
      rtopn.t_opn_path <- (List.rev (List.tl (List.rev rtopn.t_opn_path)))@[(List.hd (List.rev rtopn.t_opn_path))^name_copie]; 
      rtopn.t_opn_ports <- [];
      rtopn.t_opn_xsc_name <- operation.t_opn_xsc_name; 
      rtopn

  (*  create a synchronisation copie operation *)
  let create_synch_ack_copie graph operation opr pos = 
    let name_copie = ("_"^(string_of_int pos)) in   
    let newopn = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values operation.t_opn_ports  
      	            [] [] operation.t_opn_condition (Some opr) 0 0. true operation.t_opn_origin None operation.t_opn_xsc_name
                    operation.t_opn_referencing_alglib operation.t_opn_referencing_algname operation.t_opn_code_phases) in
      newopn.t_opn_path <- (List.rev (List.tl (List.rev newopn.t_opn_path)))@[(List.hd (List.rev newopn.t_opn_path))^name_copie]; 
      newopn.t_opn_ports <- [];
      Hashtbl.add graph (identifier_of_operation newopn) newopn;
      newopn.t_opn_xsc_name <- operation.t_opn_xsc_name; 
      newopn


 (* add routing operation  between two dependent opns A and B. 
    Exp. A --> B .... become .... A --> R_{AB} ---> B *)
  let create_routing_opn dpd rtopn pos =   
    let sprt, sopn, dprt, dopn = dpd.t_dpd_sprt, dpd.t_dpd_sopn, dpd.t_dpd_dprt, dpd.t_dpd_dopn in    
    let rt_opn = (create_copie_routing rtopn pos) in
    let name = (List.hd (List.rev rt_opn.t_opn_path)) in 
    let name = String.sub name 1  ((String.length name)-1) in 
      String.set name ((String.length name)-1) '%';
      let name_dopn =  (name_of_operation dopn) in
	rt_opn.t_opn_path <- ["R("^(name_of_operation sopn)^(name_dopn)^")"^name];   	
	let prt_in =
	  {t_prt_name=dprt.t_prt_name;t_prt_dir=dprt.t_prt_dir;t_prt_typename=dprt.t_prt_typename;
           t_prt_class=dprt.t_prt_class;t_prt_dim=dprt.t_prt_dim;t_prt_order=dprt.t_prt_order}  in  
	  (*sopn.t_opn_ports <- prt_out :: sopn.t_opn_ports;*)  
	  rt_opn.t_opn_ports <- prt_in  :: rt_opn.t_opn_ports; 
	  let new_dpd = (backup_dependence_add sopn sprt rt_opn prt_in dpd.t_dpd_class dpd.t_dpd_condition true) in
            (match dpd.t_dpd_backups with
               | None -> ();
               | Some bk_dpds ->
		   new_dpd.t_dpd_backups <- 
		   Some ( 
		     List.fold_left 
			    (fun dpds dpd -> 
			       let sprt, sopn, dprt, dopn = dpd.t_dpd_sprt, dpd.t_dpd_sopn, dpd.t_dpd_dprt, dpd.t_dpd_dopn in
			       let prt_in =
				 {t_prt_name=dprt.t_prt_name;t_prt_dir=dprt.t_prt_dir;t_prt_typename=dprt.t_prt_typename;
				  t_prt_class=dprt.t_prt_class;t_prt_dim=dprt.t_prt_dim;t_prt_order=dprt.t_prt_order}  in    
				 rt_opn.t_opn_ports <- prt_in  :: rt_opn.t_opn_ports;  
				 (backup_dependence_add sopn sprt rt_opn prt_in dpd.t_dpd_class dpd.t_dpd_condition true)::dpds;
			    ) [] bk_dpds));
	    (* add dependences between routing operation and succ *)
	    let prt_out = 
              {t_prt_name=sprt.t_prt_name;t_prt_dir=sprt.t_prt_dir;t_prt_typename=sprt.t_prt_typename;  
	       t_prt_class=sprt.t_prt_class;t_prt_dim=sprt.t_prt_dim;t_prt_order=sprt.t_prt_order}  in   
	      rt_opn.t_opn_ports <- prt_out :: rt_opn.t_opn_ports; 
              ignore(backup_dependence_add rt_opn prt_out dopn dprt dpd.t_dpd_class dpd.t_dpd_condition true);    
	      (* schedule routing operation *)
              (*ps ("create routing operation");ps_dpds rt_opn;*)
              rt_opn	      

  (* create nmf routing_operation *)
  let rec create_routing_opns dpd graph rt_opn count =   
    match count with
      | 0 -> []
      | _ -> 
	  let new_rtopn = (create_routing_opn dpd rt_opn count) in
	    Hashtbl.add graph (identifier_of_operation new_rtopn) new_rtopn;
            new_rtopn::(create_routing_opns dpd graph rt_opn (count-1)) 


  (*  create a new operation from its mould operation *)
  let create_copie operation graph routing_opn status =
    (* REMOVE +1 to give exact indice of replica *)
    let size = (replicas_size operation) (*+ 1*) in 
    let name_copie = ("_"^(string_of_int size)^"#") in   
    let backup = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values operation.t_opn_ports 
      	            [] [] operation.t_opn_condition None 0 0. status operation.t_opn_origin None operation.t_opn_xsc_name
                    operation.t_opn_referencing_alglib operation.t_opn_referencing_algname operation.t_opn_code_phases) in
      backup.t_opn_path <- (List.rev (List.tl (List.rev backup.t_opn_path)))@[(List.hd (List.rev backup.t_opn_path))^name_copie]; 
      backup.t_opn_ports <- 
      List.map (function p -> match ((p.t_prt_dir=Port.Out)&&(p.t_prt_name<>"precedence_out")) with
		  | false  -> p
		  | true -> {t_prt_name=p.t_prt_name;t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;
			     t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order}) backup.t_opn_ports;
      backup.t_opn_ports <-  
      List.map (function p -> match ((p.t_prt_dir=Port.In)&&(p.t_prt_name<>"precedence_in")) with 
		  | false  -> p
		  | true -> {t_prt_name=p.t_prt_name;t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;
			     t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order})  backup.t_opn_ports;
      List.iter (function dpd -> 
		   let dprt = (List.find (function prt -> prt.t_prt_name=((dpd.t_dpd_dprt).t_prt_name)) backup.t_opn_ports) in 
		   let new_dpd = 
		     backup_dependence_add dpd.t_dpd_sopn dpd.t_dpd_sprt backup dprt dpd.t_dpd_class dpd.t_dpd_condition status in 
		     new_dpd.t_dpd_backups <- dpd.t_dpd_backups;
		) operation.t_opn_dependences_predecessors;            
      backup.t_opn_predecessors <-  List.map (fun {t_dpd_sopn=sopn} -> sopn) backup.t_opn_dependences_predecessors;   
      List.iter (function dpd ->  
		   let sprt = (List.find (function prt -> prt.t_prt_name=((dpd.t_dpd_sprt).t_prt_name)) backup.t_opn_ports) in
		   let dopn_dpd_bk = 
		     backup_dependence_add backup sprt dpd.t_dpd_dopn dpd.t_dpd_dprt dpd.t_dpd_class dpd.t_dpd_condition status in
                   let dopn_dpd = List.find (fun {t_dpd_sopn=sopn} -> sopn=$operation)
				    (dpd.t_dpd_dopn).t_opn_dependences_predecessors in
		     dopn_dpd.t_dpd_backups <-  
		     match dopn_dpd.t_dpd_backups with
                       | None -> Some [dopn_dpd_bk]
                       | Some backups -> Some (dopn_dpd_bk::backups) 
		) operation.t_opn_dependences_successors;    
      backup.t_opn_successors <- List.map (fun {t_dpd_dopn=dopn} -> dopn) backup.t_opn_dependences_successors; 
      backup.t_opn_xsc_name <- operation.t_opn_xsc_name; 
      backup  

  (* get a not communication lip of a given operation lip, where lip is  Latest Immediat Predecessor *) 
  let rec get_lip_not_communication lip=  
    match (is_communication lip) with 
      | false -> lip 
      | true  -> get_lip_not_communication (List.hd lip.t_opn_predecessors) 

  (* get a lip of a given operation lip *) 
  let get_lip operation operator _LIPs = 
    let operator = pi operation in
    let dpds_lips = list_max_elements dpd_EEFS operation.t_opn_dependences_predecessors 0. in 
      match((List.exists (function dpd_lip ->((dpd_lip.t_dpd_sopn).t_opn_operator=(Some operator))) dpds_lips)||(dpds_lips=[])|| 
            (List.exists (function {t_dpd_sopn=sopn}-> (List.exists (function lip -> (get_original lip)=(get_original sopn)) _LIPs)) dpds_lips)) with
	| true  -> None 
	| false -> let lip = (get_lip_not_communication (List.hd dpds_lips).t_dpd_sopn) in  
          let operators_able = (operators_able_to_execute lip) in
            match (List.mem operator operators_able) with
              | true  -> Some lip
              | false -> None 

(* get best operator for routing operation *)
  let get_best_opr excl_oprs =
    (*ps "";ps (List.fold_left (fun s opr -> s^" , "^(name_of_operator opr)) "exclusion oprs = " excl_oprs);*)
    let all_operators = Application.operators_list !archilib !archiname in 
      (*ps (List.fold_left (fun s opr -> s^" , "^(name_of_operator opr)) "all oprs = " all_operators);*)
    let excls =  
      List.fold_left (fun lst opr -> 
			if (List.exists (fun excl_opr -> (name_of_operator excl_opr)=(name_of_operator opr)) excl_oprs) 
			then lst else opr::lst) [] all_operators in
    let best_opr = 
      try List.hd excls 
      with _ -> raise (Failure ("No more hardware to tolerates "^(string_of_int !failures.npf)
				^" processor(s) failures"^" and "^(string_of_int !failures.npf)^" link(s) failures")) in
      (*ps (" we choose :  "^(name_of_operator best_opr));*) 
      best_opr  

(* schedule routing operations *)
  let schedule_routing_opns operation operator graph status =
    exclusive_rtopn := [];
    let org_dpds =
      List.filter (fun {t_dpd_sopn=sopn}-> ((is_original sopn)&&(not (is_routing_operation sopn)))
		  ) operation.t_opn_dependences_predecessors in     
      (*let org_dpds,others = 
	List.partition (fun {t_dpd_sopn=sopn}-> (pi sopn)<>operator
	) org_dpds in *)  	        	
    let org_dpds,intra_dpds =   
      List.partition
	(fun {t_dpd_sopn=sopn}-> 
	   let sopn_copies = sopn::(try (Hashtbl.find
	   replicas_operations (name_of_operation sopn)) with _ -> []) in
	     not (List.exists (fun sopn_copie -> try (pi sopn_copie)=@operator with _ -> false) sopn_copies)
	) org_dpds in
      List.iter (fun dpd ->      
		   match dpd.t_dpd_routing with  
		     |None -> ()
		     | Some rts -> 
			 (List.iter (fun rt -> match status with
                                       | false -> rt.t_opn_operator <- Some operator
				       | true  -> Adequationtypes.operation_remove graph rt) rts)) intra_dpds;
      List.iter 
	(fun dpd ->
           let sopn,dopn = (name_of_operation dpd.t_dpd_sopn), (name_of_operation dpd.t_dpd_dopn) in
           let rt_copies =
	     match dpd.t_dpd_routing with
	       | None -> []
	       | Some opns -> opns in
           let oprs_sopn_copies = 
	     (match dpd.t_dpd_backups with 
                |None -> [(pi dpd.t_dpd_sopn)]
                | Some copies -> (pi dpd.t_dpd_sopn)::(List.map (fun {t_dpd_sopn=copie} -> pi copie) copies)) in
	   let oprs_dopn_copies = [operator]
				    (*match dpd.t_dpd_backups with 
				      |None -> [operator]
				      | Some copies -> operator::[]
				      (List.fold_left (fun oprs {t_dpd_dopn=copie} -> try (pi copie)::oprs with _ -> oprs) 
				      [] copies)*) in 
	     exclusive_rtopn := (oprs_sopn_copies)@(oprs_dopn_copies); 
	     List.iter   
	       (fun rt_copie ->                                   
                  let excl_oprs =  (!exclusive_rtopn)@(try (Hashtbl.find exclusives_routing (sopn,dopn)) with _ -> []) in
                  let best_opr = get_best_opr excl_oprs in                    
		    exclusive_rtopn := best_opr:: !exclusive_rtopn;
		    debug_ps 2 debug_level ("Trying "^(name_of_operation rt_copie)^" on "^(name_of_operator best_opr));
		    match status with
		      | true ->						  
			  schedule rt_copie best_opr false 0. 0.; 
			  let delta_rt =
			    delta rt_copie.t_opn_class best_opr in
			  let esfs, wesfs = 
			    make_coms rt_copie graph true [] in
			    schedule rt_copie best_opr true esfs (esfs+.delta_rt);
			    rt_copie.t_opn_weefs <- wesfs +. delta_rt; 
			    (* update exclusives routes for to schedule backups and athers routing operations for operation *)
			    update_exclusives_routing (sopn,dopn)
		      | false -> 
			  schedule rt_copie best_opr false 0. 0.; 
			  let delta_rt =
			    delta rt_copie.t_opn_class best_opr in
			  let esfs, wesfs = 
			    make_coms rt_copie graph false [] in
			    rt_copie.t_opn_weefs <- wesfs +. delta_rt; 
			    update_exclusives_routing (sopn,dopn)
	       ) rt_copies;
	) org_dpds


(* deschedule routing operations *)
  let delete_coms_deschedule_routing_opns operation operator graph =  
    let org_dpds =
      List.filter (fun {t_dpd_sopn=sopn}-> (is_original sopn)) operation.t_opn_dependences_predecessors in  
    let org_dpds =
      List.filter (fun {t_dpd_sopn=sopn}-> (pi sopn)<>@operator
		  ) org_dpds in  
      List.iter
	(fun dpd ->
           let sopn,dopn = (name_of_operation dpd.t_dpd_sopn), (name_of_operation dpd.t_dpd_dopn) in
           let rt_copies =
	     match dpd.t_dpd_routing with
               | None -> []
               | Some opns -> opns in
             List.iter 
	       (fun rt_copie ->
                  (*ps " ----------------------------------"; 
                    ps " --------------- Before remove coms -----------------"; 
                    ps (name_of_operation rt_copie); 
                    ps_dpds rt_copie;   
                    ps " --------------- After remove coms  -----------------";*)
		  delete_coms rt_copie graph; 
		  deschedule rt_copie (pi rt_copie) false;
                  (*ps_dpds rt_copie;
                    ps " ----------------------------------";*) 
		  Hashtbl.remove exclusives_routing (sopn,dopn);
               ) (List.rev rt_copies);
	) org_dpds


  (* schedule a lip of an operation to minimize the esfs of an operation *)
  let rec try_schedule_LIP operation operator graph _LIPs status = 
    (*ps("Try minimize start time of "^(name_of_operation operation));*)    
    debug_ps 3 debug_level "schedule routing operation.....";
    if ((!failures.nmf>0)&&(!is_SamPP)) then schedule_routing_opns operation operator graph false;
    debug_ps 3 debug_level "schedule operation....................";
    schedule operation operator false 0. 0.;  
    debug_ps 3 debug_level "make coms operation...................."; 
    if (!is_SamMP) then create_gaps_backups ();
    let old_esfs,old_wesfs =  
      make_coms operation graph false [] in
      (*let old_esfs = esfs operation in*) 
      debug_ps 3 debug_level "delete coms operation ...................";
      (*let lip = None (*get_lip operation operator _LIPs*) in 	 *)
	delete_coms operation graph; 
	if (!is_SamMP) then restore_gaps ();
        debug_ps 3 debug_level  "deschedule operation .....................";       
	deschedule operation operator false;
	debug_ps 3 debug_level "delete coms / deschedule routing operations ..........."; 
	if ((!failures.nmf>0)&&(!is_SamPP)) then delete_coms_deschedule_routing_opns operation operator graph;
	debug_ps 3 debug_level "finish try";
	(old_esfs,old_wesfs,_LIPs) 
      (*match lip with 
	| None     -> (old_esfs,_LIPs)
	| Some lip ->
      (*ps ("Lip of "^(name_of_operation operation)^" is "^(name_of_operation lip)^" in "^(name_of_operator operator));*)
	let copie_lip = (create_copie_operation lip false) in 
	let _,_LIPs_LIP = try_schedule_LIP copie_lip operator graph _LIPs in
	let new_LIPs = (_LIPs @ _LIPs_LIP)@[copie_lip] in
	List.iter (function _lip -> change_dpds_pred _lip operator false;schedule _lip operator false graph;
        make_coms _lip graph false;update_replicas _lip;_lip.t_opn_esfs <- (esfs _lip)) new_LIPs;
	change_dpds_pred operation operator false;schedule operation operator false graph;
	make_coms operation graph false;(*update_replicas operation;*)
	let new_esfs = esfs operation in 
      (*ps ("Old esfs of "^(name_of_operation operation)^" = "^(string_of_float old_esfs)^", New esfs = "^(string_of_float new_esfs));*)
	delete_coms operation graph;deschedule operation operator;
	delete_change_dpds_pred operation;(*remove_replicas operation;*)
	List.iter (function lip -> delete_coms lip graph;deschedule lip operator;
        delete_change_dpds_pred lip;remove_replicas lip;) (List.rev new_LIPs);
	match (new_esfs>old_esfs) with
	| true  -> old_esfs,_LIPs
	| false -> new_esfs,new_LIPs*) 
      

  (* try to minimize start time of an operation *)
  let minimize_esfs operation operator graph status = 
    let esfs_operation,wesfs_operation,lips = try_schedule_LIP operation operator graph [] status in 
      (*ps (List.fold_left (function s -> function lip -> s^(name_of_operation lip)^" , ") 
	    ("Final Dups = "^(name_of_operation operation)^" on "^(name_of_operator operator)^" = ") lips);*)
      esfs_operation,wesfs_operation,(List.map (function lip -> (identifier_of_operation lip)) lips) 

  (* Schedule all the operation lips *)
  let schedule_LIPs operator status graph rt_opn lips = 
    let _LIPs = List.map (function lip -> create_copie (Hashtbl.find graph (get_original_string lip)) graph rt_opn false) lips in  
      List.iter (function lip ->  Hashtbl.add graph (identifier_of_operation lip) lip;  
		   (*change_dpds_pred lip operator true;*)
		   schedule_routing_opns lip operator graph true;
		   schedule lip operator true 0. 0.; 
		   (*let _esfs, _wesfs = make_coms lip graph true [] in                    
		   let delta_opt = delta lip.t_opn_class operator in*)
		     (*let esfs = (esfs lip) in*) 
		     update_replicas lip;
		     (*update_schedules operator lip esfs (esfs+.delta_opt);*)
		     (* Data becoming available on operator *)
		     List.iter (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
				  | Port.Out, Port.Data_Port | Port.Out, Port.Init_Memory_Port ->
                                      debug_ps 4 debug_level ("Adding "^(name_of_data (lip,prt))^" on "^(name_of_operator operator)^"(with cond "^(string_of_condlist lip.t_opn_condition)^")");
				      update_datas operator (lip,prt) lip prt (eefs lip)
				  | _,_ -> ()) lip.t_opn_ports;) _LIPs

  (* get k values from a list *)    
  let rec keep_npf_elements k list = 
    if (k=0) 
    then [] 
    else match list with 
      | [] -> [] 
      | v1::slist -> v1::(keep_npf_elements (k-1) slist);; 

 (*  compare sp first and esfs next *)
  let compares opt_sp opt_esfs sp esfs =
    if opt_sp>sp 
    then 1
    else if opt_sp=sp 
         then if opt_esfs > esfs
              then 1
              else 0 
         else 0
 
  (* Returns the optimal esfs, schedule pressure and operator for operation. Fails if no operator is able to execute this operation *)
  let best_oprs_esfs_sp original graph =
    let operators = (operators_constraint original) in
    let opnlib,opnname = deflibname original in 
    let opns_sp = List.fold_left (fun opts operator -> 
  				    debug_ps 2 debug_level ("\nTrying "^(name_of_operation original)^" on "^(name_of_operator operator));
				    match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
				      | true -> let delta = delta original.t_opn_class operator in
					let esfs,wesfs,lips = minimize_esfs original operator graph false in
					let spfast = match (!is_SamPP) with
                                          | true  -> wesfs +. delta 
                                          | false -> esfs +. delta in (operator,esfs,wesfs,spfast,lips)::opts
				      | false -> opts) [] operators in
    let oprs_opts = (keep_npf_elements (!failures.npf+1)
                       (List.sort (function _,opt_esfs,_,opt_sp,_ -> function _,esfs,_,sp,_ 
				     -> (compares opt_sp opt_esfs sp esfs)) opns_sp))  in
      match ((List.length oprs_opts)< !failures.npf) with  
	| true -> failwith ("There is less than "^(string_of_int (!failures.npf+1))^" operators able to execute "
			    ^(string_of_int (!failures.npf+1))^" replicas of "^(name_of_operation original))
	| false -> 
	    List.map (function opr_opt,esfs_opt,wesfs_opt,sp,lips -> 
			let sp = match (!is_SamPP) with
                          | true  -> (lefe original)+. wesfs_opt+. (delta original.t_opn_class opr_opt)
			  | false -> (lefe original)+. esfs_opt+. (delta original.t_opn_class opr_opt) in  
			  (opr_opt,(esfs_opt,wesfs_opt,sp,lips))
		     ) oprs_opts

  (* Selects the best candidate (considering schedule pressure) among operations_operatoropt *)

(*  let find_best_candidate operations_operatoropt =
    let opns_min,opr_opts =  
      (List.hd 
	 (list_min_elements (fun (_,opr_opts) -> 
			       let (_,(_,wesfs,_,_)) = (List.hd (List.rev opr_opts)) in wesfs
			    ) operations_operatoropt 0.)) in
    let opn_min,(opr_min,(_,wesfs_min,_,lips_min)) = (List.hd (List.rev opns_min)), (List.hd (List.rev opr_opts)) in
    let weefs_min = wesfs_min +. (delta opn_min.t_opn_class opr_min) in
    let (best_cand,_) = 
      List.fold_left (fun (cand_max,sp_max) (opns,opr_opts) -> 
			let opr,(esfs,wesfs,sp,lips) = List.hd (List.rev opr_opts) in 
			  match (wesfs <= weefs_min && sp > sp_max) with
			    | true  -> ((opns,opr_opts),sp)
			    | false -> (cand_max,sp_max)
		     ) (List.hd operations_operatoropt,-1.) operations_operatoropt in
      best_cand *)

  let find_best_candidate operations_operatoropt =
    let opns_min,opr_opts =  
      (List.hd 
	 (list_min_elements (fun (_,opr_opts) -> 
			       let (_,(_,wesfs,_,_)) = (List.hd opr_opts) in wesfs
			    ) operations_operatoropt 0.)) in
    let opn_min,(opr_min,(_,wesfs_min,_,lips_min)) = (List.hd opns_min), (List.hd opr_opts) in
    let weefs_min = wesfs_min +. (delta opn_min.t_opn_class opr_min) in
    let (best_cand,_) = 
      List.fold_left (fun (cand_max,sp_max) (opns,opr_opts) -> 
			let opr,(esfs,wesfs,sp,lips) = List.hd opr_opts in 
			  match (wesfs <= weefs_min && sp > sp_max) with
			    | true  -> ((opns,opr_opts),sp)
			    | false -> (cand_max,sp_max)
		     ) (List.hd operations_operatoropt,-1.) operations_operatoropt in
      best_cand


					       
  (* SPLIT DEPENDENCIES GRAPH *)
  let split_all_dependencies graph = 
    Hashtbl.iter (
      function n -> function opn ->
	let dpds = opn.t_opn_dependences_predecessors in       
	  List.iter 
	    (function {t_dpd_sopn=sopn;t_dpd_sprt=sprt;t_dpd_dopn=dopn;t_dpd_dprt=dprt;
		       t_dpd_class=dpdclass;t_dpd_condition=cond;t_dpd_status=status} ->               
	       let buses = match (!split_data) with
                 | true  -> !failures.nmf+1
		 | false -> 1 in
	       let dim_prt = sprt.t_prt_dim in 
		 sprt.t_prt_dim <-  dim_prt/buses;   
		 let dim_prt = dprt.t_prt_dim in 
		   dprt.t_prt_dim <-  dim_prt/buses;  			
		   for index=1 to !failures.nmf do (*=2  , +1*)
		     let prt_out = {t_prt_name=(sprt.t_prt_name^"_"^(string_of_int index)^"#");t_prt_dir=sprt.t_prt_dir;t_prt_typename=sprt.t_prt_typename;t_prt_class=sprt.t_prt_class;t_prt_dim=sprt.t_prt_dim;t_prt_order=sprt.t_prt_order}  in
		     let prt_in =  {t_prt_name=(dprt.t_prt_name^"_"^(string_of_int index)^"#");t_prt_dir=dprt.t_prt_dir;t_prt_typename=dprt.t_prt_typename;t_prt_class=dprt.t_prt_class;t_prt_dim=dprt.t_prt_dim;t_prt_order=dprt.t_prt_order}  in
		       sopn.t_opn_ports <- prt_out :: sopn.t_opn_ports; 
		       dopn.t_opn_ports <- prt_in  :: dopn.t_opn_ports;
		       dependence_add sopn prt_out dopn prt_in dpdclass cond status;
		   done;
		   (* Compute the new t_prt_dim for the firt dpd in the case where old prt_dim is not a multiple of nmf+1 *)
		   (match (sprt.t_prt_dim * (buses))<dim_prt with 
		      |  true -> let new_dim = dim_prt - (sprt.t_prt_dim * (buses-1)) in
			   sprt.t_prt_dim <- new_dim; dprt.t_prt_dim <- new_dim
		      |  false -> ());
	    ) dpds) graph


  let set_routing_operations operation graph routing_opn =
    List.iter (fun dpd ->
                 dpd.t_dpd_routing <- Some (create_routing_opns dpd graph routing_opn !failures.nmf) 
	      )(List.filter (fun {t_dpd_sopn=sopn} -> is_original sopn) operation.t_opn_dependences_predecessors);
    operation.t_opn_predecessors <-  List.map (fun {t_dpd_sopn=sopn} -> sopn) operation.t_opn_dependences_predecessors

  (* Create npf replicas of opn *)
  let rec create_copies operation graph routing_opn count =   
    match count with
      | 0 -> 
	  if ((!failures.nmf>0)&&(!is_SamPP)) 
          then set_routing_operations operation graph routing_opn;
          []
      | _ ->  
	  let copie = (create_copie operation graph routing_opn true) in
            if ((!failures.nmf>0)&&(!is_SamPP)) 
	    then  set_routing_operations copie graph routing_opn;
            (*ps_dpds copie;*)
	    Hashtbl.add graph (identifier_of_operation copie) copie;  
            update_replicas copie;
            copie::(create_copies operation graph routing_opn (count-1)) 

(* Updates the list of schedulables, considering that operation has
    just been scheduled *)
  let successors_ones opn = 
     List.fold_left (fun succs succ -> if (List.mem succ succs) then succs else succ::succs) [] opn.t_opn_successors

  let predecessors_ones opn = 
     List.fold_left (fun preds pred -> if (List.mem pred preds) then preds else pred::preds) [] opn.t_opn_predecessors

  let update_schedulables graph operation routing schedulables =


   (* ps (List.fold_left (fun s opns -> s^(name_of_operation (List.hd opns))^" ") 
			  "schedulables  : " !schedulables);*)

   schedulables := List.filter (fun opns -> (List.hd opns)<>$operation) !schedulables;

   (*ps (List.fold_left (function s -> function opn -> s^(name_of_operation opn)^" ") 
			  "succes befor : " (successors operation));
    ps (List.fold_left (function s -> function opn -> s^(name_of_operation opn)^" ") 
			  "succs after : " (successors_ones operation));*)

    let new_schedulables =
      List.filter (fun opn -> 
                          (*ps (List.fold_left (function s -> function opn -> 
                         (match ((opn.t_opn_operator <> None) || (is_memory opn) || (is_constant opn)) with
                         | true -> ps "ok"
                         | false -> ps "no");

                          s^(name_of_operation opn)^" ") 
			  "preds of succs : " (predecessors opn));*)

                         (is_schedulable opn) && (not (is_constant opn)))
                        	((successors_ones operation)@(predecessors_ones operation)) in

    (*ps (List.fold_left (function s -> function opn -> s^(name_of_operation opn)^" ") 
			  "new schulables  : " new_schedulables);*)

      schedulables := !schedulables@(List.map 
				       (fun opn -> opn::(create_copies opn graph routing !failures.npf)
				       ) new_schedulables)

  (* Schedule all the copies of an optimal operation *)
  let schedule_copiesopts copies oprs_opts graph = 
    let pos = ref 0 in 
      List.iter 
	(function opr_opt ->
	   let copie = (List.nth copies !pos) in 
             pos := !pos + 1;	     
             (* schedule lips of optimal operation               
  	        schedule_LIPs opr_opt true graph rt_opn lips_opt;
	        let esfs_opt,wesfs_opt,lips = minimize_esfs copie opr_opt graph true in *)
	     if ((!failures.nmf>0)&&(!is_SamPP)) then schedule_routing_opns copie opr_opt graph true;  
	     (* schedule optimal operation *)                         
	     schedule copie opr_opt false 0. 0.;
	     let delta_opt = 
	       delta copie.t_opn_class opr_opt in 
	     let esfs_opt,wesfs_opt = 
	       make_coms copie graph true [] in	    
               old_len := max !old_len (esfs_opt+.delta_opt);

               (********************************************************)
               (** routes are not all exclusives :                    **) 
               (**   for A->B :  {A -> B and R -> B}  are exclusives  **)
               (**        but not A -> R with {A -> B and R -> B}     **)          
               (********************************************************)
	       schedule copie opr_opt true (esfs_opt) ((esfs_opt+.delta_opt)); 
               copie.t_opn_weefs <- wesfs_opt +. delta_opt;     
	       (*ps ("\n schedule "^(name_of_operation copie)^" into "^(name_of_operator opr_opt)
		   ^"  at  "^(string_of_float copie.t_opn_esfs)^"\n");*)
	       (* Data becoming available on operator *)  
	       List.iter 
		 (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
		    | Port.Out, Port.Data_Port | Port.Out, Port.Init_Memory_Port -> 
			debug_ps 4 debug_level 
			("Adding "^(name_of_data (copie,prt))^" on "^(name_of_operator opr_opt)^"(with cond "
			 ^(string_of_condlist copie.t_opn_condition)^")");
			update_datas opr_opt (copie,prt) copie prt (eefs copie)
		    | _,_ -> ()
		 ) copie.t_opn_ports; 
  	       debug_ps 1 debug_level 
		 ("adequation place_calcul : "^(name_of_operation copie)^" on "^(name_of_operator opr_opt)^"...ok");
	) oprs_opts

(* SAMPP : synchronize all processor' horloges in the aim to start next loop *)
 let horloge_synchronization_sampp graph =  
   let oprs = Application.operators_list !archilib !archiname in
   let name_root = Architecture.operator_main_get !archilib !archiname in     
   let root = List.find (fun opr -> name_root=(name_of_operator opr)) oprs in
   let synch_opn = new_opn ["Syn"] (Calcul (Operation,"","Syn")) [] (precedence_ports ()) [] [] 
		     [(None,None)] (Some root) 0 0. true (Ihm ["Syn"]) None 
		     "" "" "" [InitSeq;LoopSeq;EndSeq] in
   let ack_opn   = new_opn ["Ack"] (Calcul (Operation,"","Ack")) [] (precedence_ports ()) [] [] 
		     [(None,None)] None 0 0. true (Ihm ["Ack"]) None 
		     "" "" "" [InitSeq;LoopSeq;EndSeq] in
     List.iter 
       (function operator -> Hashtbl.add operator.oprdef_operation_durations ("","Syn") 1.;
          Hashtbl.add operator.oprdef_operation_durations ("","Ack") 1.) Application.application.app_operator_definitions;
     let oprs_no_root = List.filter (fun opr -> opr <>@ root) oprs in
     let pos = ref 0 in
     let send_acks = List.map 
		       (fun opr -> pos := !pos+1;create_synch_ack_copie graph ack_opn opr !pos
		       ) oprs_no_root in 
     let send_synch = create_synch_ack_copie graph synch_opn root 1 in
     let recv_synch = create_synch_ack_copie graph synch_opn root 2 in
       List.iter 
	 (function send_ack -> 
            (* send_synch ---> send_ack *)
	    let sprt = {t_prt_name="ack_"^(name_of_operator (pi send_ack));t_prt_dir = Port.Out; t_prt_typename = "int" 
                       ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=1} in 
	    let dprt = {t_prt_name="ack_"^(name_of_operator (pi send_ack));t_prt_dir = Port.In; t_prt_typename = "int"
                       ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=1} in 
              send_synch.t_opn_ports <- sprt :: send_synch.t_opn_ports; 
	      send_ack.t_opn_ports   <- dprt :: send_ack.t_opn_ports;
	      dependence_add send_synch sprt send_ack dprt (Data,Strong_Precedence_Data) [] true;
              (* send_ack ---> recv_synch *)
	      let sprt = {t_prt_name="ack_"^(name_of_operator (pi send_ack));t_prt_dir = Port.Out; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=1} in 
	      let dprt = {t_prt_name="ack_"^(name_of_operator (pi send_ack));t_prt_dir = Port.In; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=1} in 
		recv_synch.t_opn_ports <- dprt :: send_synch.t_opn_ports; 
		send_ack.t_opn_ports   <- sprt :: send_ack.t_opn_ports;
		dependence_add send_ack sprt recv_synch dprt (Data,Strong_Precedence_Data) [] true;
         ) send_acks;
       schedule_copiesopts [send_synch] [pi send_synch] graph;
       List.iter 
	 (function send_ack -> 
                schedule_copiesopts [send_ack] [pi send_ack] graph;
         ) send_acks;
       schedule_copiesopts [recv_synch] [pi recv_synch] graph         


(* SAMMP : synchronize all processors in the aim to start next loop *)
 let horloge_synchronization graph =  
   let oprs = Application.operators_list !archilib !archiname in
   let name_root = Architecture.operator_main_get !archilib !archiname in     
   let root = List.find (fun opr -> name_root=(name_of_operator opr)) oprs in
   let synch_send_opn = new_opn (!path@["synch_send"]) (Calcul (Operation,"faults","synch_send")) [] (precedence_ports ()) [] [] 
			  ([(None,None)]) None  0 0. true (Ihm ["synch_send"]) None 
			  "" "faults" "synch_send" [InitSeq;LoopSeq;EndSeq] in
   let ack_recv_opn = new_opn (!path@["ack_recv"]) (Calcul (Operation,"faults","ack_recv")) [] (precedence_ports ()) [] [] 
			([(None,None)]) None  0 0. true (Ihm ["ack_recv"]) None 
			"" "faults" "ack_recv" [InitSeq;LoopSeq;EndSeq] in
   let ack_opn   = new_opn (!path@["ack"]) (Calcul (Operation,"faults","ack")) [] (precedence_ports ()) [] [] 
		     [(None,None)] (Some root) 0 0. true (Ihm ["ack"]) None 
		     "" "faults" "ack" [InitSeq;LoopSeq;EndSeq] in
     List.iter  
       (function operator -> 
	  Hashtbl.add operator.oprdef_operation_durations ("faults","synch_send") 1.;
          Hashtbl.add operator.oprdef_operation_durations ("faults","ack") 1.;
	  Hashtbl.add operator.oprdef_operation_durations ("faults","ack_recv") 1.;
       ) Application.application.app_operator_definitions;
     let oprs_no_root = List.filter (fun opr -> opr <>@ root) oprs in
     let pos = ref 0 in
     let synch_sends = List.map (fun opr -> pos := !pos+1;
				   create_synch_ack_copie graph synch_send_opn opr !pos			    
				) oprs_no_root in 
       pos :=0;
       let ack_recvs = List.map (fun opr -> pos := !pos+1;
				   create_synch_ack_copie graph ack_recv_opn opr !pos			    
				) oprs_no_root in 
       let ack = create_synch_ack_copie graph ack_opn root 1 in
       let ports_synch_send = ref [] in
       let ports_ack_recv = ref [] in
       let ports_ack = ref [] in
	 List.iter 
	   (function synch_send ->             
              (*** synch_send --> ack_opn ******)
	      let sprt = {t_prt_name=(name_of_operator (pi synch_send));t_prt_dir = Port.Out; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=(List.length !ports_synch_send)} in  
	      let dprt = {t_prt_name=(name_of_operator (pi synch_send));t_prt_dir = Port.In; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=(List.length !ports_ack)} in 
              let def_dprt = (dprt.t_prt_name,dprt.t_prt_dir,dprt.t_prt_typename,(Symbolic.Float 1.), 
			      dprt.t_prt_class,(List.length !ports_ack),(Coord.Coord2d(0,0)))   in
              let def_sprt = (sprt.t_prt_name,sprt.t_prt_dir,sprt.t_prt_typename,(Symbolic.Float 1.), 
			      sprt.t_prt_class,(List.length !ports_synch_send),(Coord.Coord2d(0,0)))   in
		ports_synch_send := def_sprt:: !ports_synch_send;
		ports_ack  := def_dprt:: !ports_ack;
		synch_send.t_opn_ports <- sprt :: synch_send.t_opn_ports;
		ack.t_opn_ports        <- dprt :: ack.t_opn_ports; 
		dependence_add synch_send sprt ack dprt (Data,Strong_Precedence_Data) [] true;
           ) synch_sends;

	 List.iter 
	   (function ack_recv ->             
              (*** ack_opn ---> ack_recv ******)
	      let sprt = {t_prt_name=(name_of_operator (pi ack_recv));t_prt_dir = Port.Out; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=(List.length !ports_ack)} in 
	      let dprt = {t_prt_name=(name_of_operator (pi ack_recv));t_prt_dir = Port.In; t_prt_typename = "int"
			 ;t_prt_class=Port.Data_Port; t_prt_dim = 1;t_prt_order=(List.length !ports_ack_recv)} in 
              let def_dprt = (dprt.t_prt_name,dprt.t_prt_dir,dprt.t_prt_typename,(Symbolic.Float 1.), 
			      dprt.t_prt_class,(List.length !ports_ack_recv),(Coord.Coord2d(0,0)))   in
              let def_sprt = (sprt.t_prt_name,sprt.t_prt_dir,sprt.t_prt_typename,(Symbolic.Float 1.), 
			      sprt.t_prt_class,(List.length !ports_ack),(Coord.Coord2d(0,0)))   in
		ports_ack_recv := def_dprt:: !ports_ack_recv;
		ports_ack  := def_sprt:: !ports_ack;
		ack_recv.t_opn_ports <- dprt :: ack_recv.t_opn_ports;
		ack.t_opn_ports      <- sprt :: ack.t_opn_ports; 
		dependence_add ack sprt ack_recv dprt (Data,Strong_Precedence_Data) [] true;
           ) ack_recvs;

	 Algorithm.operation_create "faults" "synch_send" ["length"] !ports_synch_send [] 
	   (Coord.Coord2d(200,200))  "synchronisation send" [InitSeq;LoopSeq;EndSeq];
	 Algorithm.operation_create "faults" "ack_recv" ["length"] !ports_ack_recv [] 
	   (Coord.Coord2d(200,200))  "synchronisation receive" [InitSeq;LoopSeq;EndSeq];
	 Algorithm.operation_create "faults" "ack" ["length"] !ports_ack  [] 
	   (Coord.Coord2d(200,200))  "synchronisation Ack" [InitSeq;LoopSeq;EndSeq];

	 List.iter 
	   (function send_synch -> 
              schedule_copiesopts [send_synch] [pi send_synch] graph;
           ) (synch_sends@[ack]@ack_recvs)
       

(* Compute the reliability *)
let compute_rel opn oprs = 
  (*List.iter (fun opr -> ps (name_of_operator opr)) oprs;*)
  (*let index = ref 0 in*)
  let non_rel = 
    List.fold_left (fun tmp_mul opr -> (*ps (string_of_float tmp_mul);*)
        let lambda = (Hashtbl.find failures_rate (name_of_operator opr)) in 
        let exec = (delta opn.t_opn_class opr) in
        (tmp_mul *. (1. -. (exp(-.(lambda*.exec)))))) 1. oprs in
   (1. -. non_rel)

(* Update reliability *)
let update_rel opn oprs =
  old_rel := !old_rel *. (compute_rel opn oprs)



       (* FAULT TOLERANCE ADEQUATION FOR : bus and point-to-point Architecture *)
    let fault_tolerance_adequation graph = 
       (*let nb_opns = ref 0 in*)
         old_len := 0. ;
         old_rel := 1.;
       let s = Hashtbl.fold 
		   (fun _ opn s -> 
		      s^"\n"^(identifier_of_operation opn)^(string_of_argsvalues opn.t_opn_arguments_values))
		   graph "Initial graph :" in  
	   debug_ps 1 debug_level s;
	   fault_architecture_init graph;      
	   Hashtbl.clear lefe_table;  
	   adequation_order := 0;
	   (* Create a new operation for routing operation R without preds and succs *)
	   let routing_opn = new_opn ["R"] (Calcul (Operation,"","R")) [] (precedence_ports ()) [] [] 
			       [(None,None)] None 0 0. true (Ihm ["R"]) None 
			       "" "" "" [InitSeq;LoopSeq;EndSeq] in 
	   let oprs = Application.application.app_operator_definitions in 
	     List.iter 
	       (function operator -> Hashtbl.add operator.oprdef_operation_durations ("","R") 1. )
	       oprs;
	     (*let all_operators = Application.operators_list !archilib !archiname in *)
	     let all_medias    = Application.media_list     !archilib !archiname in
	       (* test if all media are SamPP or SamMP *)
	       is_SamMP := false;
	       is_SamPP := false;
	       List.iter 
		 (function media -> match media with 
		    | Media mda -> 
			(match (Architecture.bustype mda) with
			   | SamMP,_ -> if not (!is_SamPP) then is_SamMP := true 
			     else raise (Failure "Fault tolerance adequation for only point-to-point or only multibus architecture !");
			   | SamPP,_ -> if not (!is_SamMP) then is_SamPP := true 
			     else raise (Failure "Fault tolerance adequation for only point-to-point or only multibus architecture !");
			   |  _   -> raise (Failure "Fault tolerance adequation for only point-to-point or only multibus architecture !"));
		    | _ -> ()
		 )	all_medias;
	       if ((!failures.nmf>0)&&(!is_SamMP)) then split_all_dependencies graph;
	       (*let step total = match total<100 with 
		 | true -> 1
		 | false -> total/100 in*)
	       (*let total_adequation = (hashtbl_length graph) in  *)
               (*let nb_oprs = List.length all_operators in     *)
               (***** initialize data for reliability  *****) 
               initialize_failure_rates_table  !file_name;                
               (**** END initialize data for reliability ****)   
	       (*let operators_number = List.length all_operators in*)
	       (*let progress_box = Progress_box.create total_adequation in*)
	       let schedulables_opns = (hashtbl_filter 
					  (fun o -> (is_schedulable o) && (not (is_constant o))
					  ) graph) in
	       let schedulables = ref (List.map 
					 (fun opn -> opn::(create_copies opn graph routing_opn !failures.npf)
					 ) schedulables_opns) in	    
               let opn = List.hd (List.hd !schedulables) in 
                 path := (List.rev (List.tl (List.rev opn.t_opn_path)));
		 try 
		   while !schedulables <> [] do
                     (*ps "--------------------------------------------------------";  *)
   		     debug_ps 1 debug_level 
		       (List.fold_left 
			  (function s -> function opns -> s^(identifier_of_operation (List.hd opns))^" ") 
			  "schedulables : " !schedulables); 
		     (* Calcul of the optimal operator for each operation *)
		     let operations_operatorsopts = 
		       List.map 
			 (function opns -> opns,(best_oprs_esfs_sp (List.hd opns) graph)
			 ) !schedulables in

		     let opns_opts,oprs_opts = find_best_candidate operations_operatorsopts in
		       (* schedule all copies of optimal operation *)
                     Hashtbl.add originals_operations (name_of_operation (List.hd opns_opts)) (List.hd opns_opts);

                     let oprs_opts = List.map (function opr_opt,(_,_,_,_) -> opr_opt) oprs_opts in
		       schedule_copiesopts opns_opts oprs_opts graph; 
		       (* We need to place memory successor coms now (this is not needed to determine operation esfs). *)  

		       let original = List.hd opns_opts in
			 (match is_memory original with
			    | true -> ()(*make_memory_succ_coms graph (eefs original) original []*)
			    | false -> ());

                         update_rel (List.hd opns_opts) oprs_opts;
			 (* Update the list of schedulables operations *)
			 update_schedulables graph original routing_opn schedulables;
			 (*Progress_box.tick progress_box;*)

                       
		   done;
		   (*place_constant graph progress_box;*)                   
                   len := Hashtbl.fold (fun  _  opn eefs_tmp -> (max eefs_tmp (eefs opn))) graph min_float; 
                   old_len := !len;
                   
		   (*horloge_synchronization graph;*)
		   (* We need to have the list of operations sorted by esfs (for code generation) *)
		   Hashtbl.iter
		     (fun _ (_,oprschedule) ->
			let opns = Hashtbl.fold (fun _ {t_sch_operations=cond_opns} opns -> opns@cond_opns) oprschedule [] in
			let opns_sorted = 
			  List.stable_sort 
			    (function opn1,esfs1,eefs1 -> function opn2,esfs2,eefs2 -> match compare esfs1 esfs2 with
			       | 0 -> (match (esfs1 = eefs1) && (esfs2=eefs2) with (* two successive null duration operations*)
					 | true -> compare opn1.t_opn_adequation_order opn2.t_opn_adequation_order 
					 | false -> compare eefs1 eefs2)
			       | x -> x
			    ) opns in 		
			  ignore 
			    (List.fold_left 
			       (function i -> function opn,_,_ -> 			       
				  opn.t_opn_rank <- i; i+1) 0 opns_sorted)
		     ) schedules;	      
		   (match debug_level > 0 with
		      | true -> print_adequation_result ()
		      | false -> ());

		   (*Hashtbl.iter (fun _ opn -> 
				   if (is_routing_operation opn)
				   then begin 
				     (* rename routing operation : remove the symbol % *)
				     let path = (List.rev (List.tl (List.rev opn.t_opn_path))) in
				     let name = (List.hd (List.rev opn.t_opn_path)) in 
				     let name = String.sub name 0  ((String.length name)-1) in
				       opn.t_opn_path <- path@[name]
				   end
				   else if not (is_original opn)
				   then 
				     begin                                          
                                       let original = Hashtbl.find originals_operations (get_original opn) in
                                         (*opn.t_opn_path <- original.t_opn_path;*) ()
				       (* rename copies operation : remove the symbol # *)
				       (*let path = (List.rev (List.tl (List.rev opn.t_opn_path))) in 
				       let name = (List.hd (List.rev opn.t_opn_path)) in  
				       let name = String.sub name 0  ((String.length name)-1) in 
					 opn.t_opn_path <- path@[name^"_"]   *)
				     end) graph;*) 
		   graph,schedules  
		 with exn -> (*Progress_box.close progress_box;*)raise exn
  			    

(* Returns [p] if p is a calcul operation otherwise the full sender
path to producer of communication p *)
let rec transitive_predecessors p = 
  match p.t_opn_class with  
  | Communication (Receive _) -> let send = List.hd (predecessors p) in
    send::(union (List.map
		    (fun op -> transitive_predecessors op)
		    (predecessors send)))
  | Communication (Reliable_Receive _) -> let send = List.hd (predecessors p) in
    send::(union (List.map
		    (fun op -> transitive_predecessors op)
		    (predecessors send))) 
  | _ -> [p] 

(* Returns [opn] if opn is a calcul operation otherwise the full
   sender paths to receivers of communication opn *) 
let rec transitive_successors opn =
(*  let succs = successors opn in*)
  match opn.t_opn_class with
  | Communication (Send _) ->
      let succs = union (List.map successors (successors opn)) in
      opn::(union (List.map transitive_successors succs))
  | Communication (Reliable_Send _) ->
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
  | Communication (Reliable_Send _) -> opn.t_opn_rank/2
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
  | Communication (Reliable_Receive _),_ -> schedules
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
	| Communication (Reliable_Receive _) -> preds
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
	| Communication (Reliable_Receive _) -> succs
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
	| Communication (Reliable_Send _) ->
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

