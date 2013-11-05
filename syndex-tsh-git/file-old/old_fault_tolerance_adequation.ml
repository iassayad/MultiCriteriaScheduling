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

  let debug_level = 0
 
  let level_max = ref 0      
  let old_len = ref 0.
  let old_rel = ref 1.
  let file_name = ref ""
  let rate_replication = ref 0.
  let failures_rate = Hashtbl.create 30

(*  let procs_name = Hashtbl.create 10
  let procs_comb = Hashtbl.create 10
  let originals_operations = Hashtbl.create 30
  let replicas_operations = Hashtbl.create 10*)

  let route_table = Hashtbl.create 30

(* Conversion of a decimal integer to its binary representation*)
let decimal2binary x comb_i = 
  let test = ref 0 in
  let index = ref 0 in 
   while ((!x <> 0)&&(!test<= (!level_max+1))) do
    let r = !x mod 2 in
     if (r=1) then test := !test+1;
     comb_i.(!index) <- r;
     index := !index +1; 
     x := ( !x - r )/ 2  
   done;
  if ((!test > (!level_max+1))||(!test=0)) then false else true

let get_combinaison_procs comb_i all_oprs size =
  (*ps (Array.fold_left (fun s -> fun opr -> s^"  "^(string_of_int opr)) "num processors : " comb_i);*)
  let index = ref 0 in 
  let oprs = ref [] in
   while (!index < size) do
     (match comb_i.(!index) with
      |  0  -> ();
      |  _  -> oprs := (List.nth all_oprs !index):: !oprs);
    index := !index +1; 
   done;
  (*ps (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) 
    "    processors  = " (List.rev !oprs));*)
  !oprs  

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
	    let subroutes = List.map (function (og1,((opr2,_) as og2)) as lnk -> lnk,(r opr2 o2 (og1::route))) links in
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
    let routes = List.filter (function route ->     
        let opr_route = (List.map fst route) in (intersection opr_route exclusives_routes)=[]) routes in
      (*ps ("find route for "^(string_of_int (List.length (list_min_elements List.length routes 0))));*) 
    let route = 
       try List.hd (list_min_elements List.length routes 0) 
      with  _ -> raise (Failure ("No route "^(name_of_operator o1)^" to "^(name_of_operator o2)^" !!")) in  
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

  (*  Return the number of operation copies which are scheduled *
  let replicas_size opn =
    match (Hashtbl.mem replicas_operations (name_of_operation opn)) with  
      | true  -> (List.length (Hashtbl.find replicas_operations (name_of_operation opn)))+1
      | false -> 1 *)
	  
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

  (*  update the list of operation copies : add 
  let update_replicas replica  = 
    let original = (get_original replica) in
    let replicas = (match (Hashtbl.mem replicas_operations original) with
       	              | true  -> replica::(Hashtbl.find replicas_operations original)  
		      | false -> [replica]) in
      (Hashtbl.replace replicas_operations original replicas)

  (* update the list of operation copies : remove *)    
  let remove_replicas replica  = 
    let original = (get_original replica) in
    let replicas = (match (Hashtbl.mem replicas_operations original) with
       	              | true  -> List.tl (Hashtbl.find replicas_operations original)  
		      | false -> (Hashtbl.find replicas_operations original)) in
      (Hashtbl.replace replicas_operations original replicas) *)


  (* Place in graph the primary communication corresponding to dependence dpd *) 
  let place_communications graph status tcond dpd rt_opns =

  let place_com (data,(operator_src,operation_src,port_src,t0))
	  operation_dst port_dst operator_dst dpdclass cond
	  place status =
	
	let rec place_com_aux sopn sprt eefs place status opr_src = 
	   (*ps ("from "^(name_of_operator opr_src)^" to "^
	     (name_of_operator operator_dst)^" placing "^
	     (name_of_operation sopn)); *)
	  match opr_src = operator_dst with
	  | true -> sopn,sprt,eefs
	  | false -> 
    	      let lnks = shortest_links opr_src operator_dst in
	      (* ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^
		 " "^(name_of_operator opr)) "Possible next oprs are " lnks); *)
	      let  best_lnk =
		find_best_link lnks [] max_float sopn sprt eefs data graph cond
		  status dpdclass operator_dst true in
	      (* This time, place it if place is true *)
	      let (opn,prt,eefs),next_opr =
		place_link best_lnk place sopn sprt eefs data graph cond
		  status dpdclass true in
	      (*  	    ps ("next opr "^name_of_operator next_opr); *)
	      place_com_aux opn prt eefs place status next_opr in 

      let rec place_SamPP_com_aux sopn sprt eefs place status opr_src route = 
       match (opr_src=operator_dst) with
	| true  -> sopn,sprt,eefs
	| false -> match route with 
	    | [] -> raise (Failure "place_copie_com_aux no exclusive route ...") 
	    | og1::og2::og3::rest -> let best_lnk = (og1::og2::[og3]) in                 
	      let (opn,prt,eefs),next_opr = 
                reliable_place_link best_lnk place sopn sprt eefs data graph cond status dpdclass in
                place_SamPP_com_aux opn prt eefs place status next_opr rest 
	    |  _ -> raise (Failure "place_copie_com_aux no exclusive route")  in
	
	let eefs = max t0 tcond in
	(*       ps ("placing com from "^(name_of_operator operator_src)); *)
	let opnpred,prtpred,eefs =
        let route = (routes operator_src operator_dst []) in 
	       place_SamPP_com_aux operation_src port_src eefs place status operator_src route in
	  (*place_com_aux operation_src port_src eefs place status operator_src in*)
	(match place with
	  (* Add dependence between last opn created and dopn (operation
	  which required the communication) *)
	| true ->
	    Adequationtypes.dependence_add opnpred prtpred operation_dst
	      port_dst dpdclass cond status
	| false -> ());
	eefs in

  (***** start here *****)
    debug_ps 2 debug_level ("place dpds  = "^(name_of_operation dpd.t_dpd_sopn)^" --> "^
                            (name_of_operation dpd.t_dpd_dopn)^" , ");
    let sopn,sprt,dopn,dprt,dpdclass,cond = 
      dpd.t_dpd_sopn,dpd.t_dpd_sprt,dpd.t_dpd_dopn,dpd.t_dpd_dprt,dpd.t_dpd_class,dpd.t_dpd_condition in
      debug_ps 2 debug_level ("placement ="^(string_of_bool status));
      debug_ps 2 debug_level ("place_primary_communication..."^(name_of_data (sopn,sprt))^" -> "^(name_of_data (dopn,dprt))); 
      debug_ps 2 debug_level ("place_primary_communication..."^(name_of_operator (pi sopn))^" -> "^(name_of_operator (pi dopn)));  
      (* Calculate list of operators containing data *)
      let data,operator_dst = (sopn,sprt),(pi dopn) in 
      let operators_src_list = try (Hashtbl.find datas data) with _ -> [] in
      let operators_src_list = (List.filter  
	                          (fun (operator_src,opn,port_src,t0) -> opn = sopn)operators_src_list) in
      let f (o,_,_,_) = match o with
	| Media mda -> mda.mdaref_links2.((operator_of_operator_class operator_dst).oprref_id)
	| Operator opr -> opr.oprref_links2.((operator_of_operator_class operator_dst).oprref_id) in
      let operators_src_list = (list_min_elements f operators_src_list 0) in
      let best_operator_src = ref 
        (try match operators_src_list with
           | [] ->  let rt,sopr,rt_eefs = List.find (fun (rt,opr,rt_eefs) -> rt=sopn) rt_opns in 
                                      (sopr,rt,sprt,rt_eefs)
           | src -> (List.hd src)   
	 with  _ -> raise (Failure "Add disjoints links to architecture!!!"))  in
	 (* Place the communication *)
	(*place_com (data, !best_operator_src) dopn dprt operator_dst graph tcond dpdclass cond true status*)
      let eefs = place_com (data, !best_operator_src) dopn dprt operator_dst dpdclass cond true status in
         eefs




 
  (* Remove inter dependences copies when intra dependences exists *)
  let delete_dependences_copies operation graph dpds_data status =
    let org_dpds = List.filter (fun {t_dpd_sopn=sopn} -> (is_original sopn)) dpds_data in
    let intra_dpds,inter_dpds = List.partition (fun {t_dpd_sopn=sopn} -> (pi sopn = pi operation)) dpds_data in
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
						if List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=dpd.t_dpd_sopn) rm_dpds 
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
			   not (List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=sopn) rm_dpds)  
		 	) operation.t_opn_dependences_predecessors; 
	     operation.t_opn_predecessors <-
	     List.filter (fun sopn -> 
			    not (List.exists (fun {t_dpd_sopn=rm_dpd} -> rm_dpd=sopn) rm_dpds) 
			 ) operation.t_opn_predecessors; 
	     List.iter (fun {t_dpd_sopn=sopn} -> 
			  sopn.t_opn_dependences_successors <-
			  List.filter (fun {t_dpd_dopn=dopn} -> dopn<>operation) sopn.t_opn_dependences_successors;  
			  sopn.t_opn_successors <- List.filter (fun dopn ->  dopn<>operation) sopn.t_opn_successors;  
		       ) rm_dpds; 
      );
      let inter_orgs,inter_bks = List.partition (fun {t_dpd_sopn=sopn} -> (is_original sopn)) filtered_inter_dpds in
	(intra_orgs@intra_bks),inter_orgs,inter_bks


   (* Builds operation predecessors coms *)
  let make_coms operation graph status rt_opns =
    debug_ps 3 debug_level ("making comm "^(name_of_operation operation));
    let make_one_com tcond dpd = 
	match (pi dpd.t_dpd_sopn = pi operation) with 
	  | true  -> eefs dpd.t_dpd_sopn
	  | false -> 
	      let best_eefs = 
		place_communications graph status tcond dpd rt_opns in 
		(match status with 
		   | true  -> dependence_remove dpd
		   | false -> ());
		best_eefs in 
    let dpds_data,dpds_condition = 
      List.partition (function {t_dpd_class=(edgetype,_)} -> edgetype=Data) 
	operation.t_opn_dependences_predecessors in  
      let tc = make_cond_dpds (make_one_com) dpds_condition in
      let _eefs = 
        List.fold_left (fun eefs dpd -> let tmp_eefs = make_one_com tc dpd in
                          max eefs tmp_eefs) tc dpds_data in
      let operator = pi operation in
        (*ps ("schedule it after "^(string_of_float _eefs));*)
      let _esfs = rel_esfs_cond operation operator _eefs operation.t_opn_condition in 
        _esfs 

(* fault_esfs_cond operation operator _eefs _eefs operation.t_opn_condition in 
   Initializes the route table, data available on operators, schedules and operator_constraints *)
  let fault_architecture_init graph = 
    architecture_init graph;
    Hashtbl.clear route_table;
    Hashtbl.clear failures_rate;

    (*Hashtbl.clear originals_operations;     
    Hashtbl.clear replicas_operations;*)
    let oprlist = List.map (Architecture.operator_reference !archilib !archiname ) (Architecture.operators_list !archilib !archiname) in
    let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
      List.iter (function o -> Hashtbl.add route_table (Operator o, Operator o) [[]]) oprlist; 
      List.iter (function m -> Hashtbl.add route_table (Media m, Media m) [[]]) mdalist 


(** Adds a dependence between sopn.sprt and dopn.dprt *)
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

  (*  create a new operation from its mould operation *)
  let create_copie operation graph status size =
    (* REMOVE +1 to give exact indice of replica *)
    (*let size = (replicas_size operation) (*+ 1*) in *)
    let name_copie = ("_"^(string_of_int size)^"#") in    
    let backup = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values 
                          operation.t_opn_ports [] [] operation.t_opn_condition None 0 0. status 
                          operation.t_opn_origin None operation.t_opn_xsc_name
                          operation.t_opn_referencing_alglib operation.t_opn_referencing_algname 
                          operation.t_opn_code_phases) in
      backup.t_opn_path <- (List.rev (List.tl (List.rev backup.t_opn_path)))@[(List.hd 
                           (List.rev backup.t_opn_path))^name_copie]; 
      backup.t_opn_ports <- List.map (function p -> match ((p.t_prt_dir=Port.Out)&&
                                    (p.t_prt_name<>"precedence_out")) with
		  | false  -> p
		  | true -> {t_prt_name=p.t_prt_name;t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;
			     t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order}) 
                    backup.t_opn_ports;
      backup.t_opn_ports <-  List.map (function p -> match ((p.t_prt_dir=Port.In)&&
                                     (p.t_prt_name<>"precedence_in")) with 
		  | false  -> p
		  | true -> {t_prt_name=p.t_prt_name;t_prt_dir=p.t_prt_dir;t_prt_typename=p.t_prt_typename;
			     t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order})  
                    backup.t_opn_ports;
      List.iter (function dpd -> let dprt = (List.find (function prt -> prt.t_prt_name=
                      ((dpd.t_dpd_dprt).t_prt_name)) backup.t_opn_ports) in  
                  ignore(backup_dependence_add dpd.t_dpd_sopn dpd.t_dpd_sprt backup dprt 
                     dpd.t_dpd_class dpd.t_dpd_condition status)
		) operation.t_opn_dependences_predecessors;            
      List.iter (function dpd ->  let sprt = (List.find (function prt -> prt.t_prt_name=
                      ((dpd.t_dpd_sprt).t_prt_name)) backup.t_opn_ports) in
		  ignore(backup_dependence_add backup sprt dpd.t_dpd_dopn dpd.t_dpd_dprt 
                     dpd.t_dpd_class dpd.t_dpd_condition status)
                ) operation.t_opn_dependences_successors;    
      backup.t_opn_predecessors<-List.map(fun {t_dpd_sopn=sopn}-> sopn)backup.t_opn_dependences_predecessors;
      backup.t_opn_successors <- List.map (fun {t_dpd_dopn=dopn} -> dopn)backup.t_opn_dependences_successors;
      backup.t_opn_xsc_name <- operation.t_opn_xsc_name; 
      backup    

(* schedule routing operations *)
  let schedule_routing_opns rt_opn rt_opr graph status =
    schedule rt_opn rt_opr false 0. 0.; 
    let delta_rt = delta rt_opn.t_opn_class rt_opr in
    let esfs = make_coms rt_opn graph false [] in
      esfs
   

  (* get esfs for operation *)
  let get_esfs operation operator graph rt_opns = 
    debug_ps 3 debug_level ("Try minimize start time of "^(name_of_operation operation));    
    debug_ps 3 debug_level "schedule operation....................";
    schedule operation operator false 0. 0.;  
    debug_ps 3 debug_level "make coms operation....................";
    let esfs_operation =  
      make_coms operation graph false rt_opns in
      debug_ps 3 debug_level "delete coms operation ...................";	 
      delete_coms operation graph;
      debug_ps 3 debug_level  "deschedule operation .....................";       
      deschedule operation operator false;
      esfs_operation 

 (*deschedule routing opns *) 
  let deschedule_routing_opns rt_opns graph =
    List.iter (fun (rt_opn, rt_opr, _) ->
		  delete_coms rt_opn graph; 
		  deschedule rt_opn rt_opr false;
               ) (List.rev rt_opns)  

  (* schedule routing opns *)
  let schedule_routing_opn rt_opn graph =
    let operators = (operators_constraint rt_opn) in
    let opnlib,opnname = deflibname rt_opn in
    let opns_sp = List.fold_left
       (fun opts operator -> 
  	 debug_ps 2 debug_level("\nTrying "^(name_of_operation rt_opn)^" on "^(name_of_operator operator));
	 match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
	 | true -> let delta = delta rt_opn.t_opn_class operator in
		   let esfs = get_esfs rt_opn operator graph [] in
		   let spfast =  esfs +. delta in  
                     (operator,esfs,spfast)::opts
	 | false -> opts) [] operators in
    let opr_opt,esfs_opt,sp = 
      (List.hd (list_min_elements (fun (opr,esfs,sp) -> sp) opns_sp 0.)) in
    schedule rt_opn opr_opt false 0. 0.; 
    let delta_rt = delta rt_opn.t_opn_class opr_opt in
    let esfs = make_coms rt_opn graph false [] in  
      rt_opn,opr_opt,(esfs +. delta_rt) 

 (*deschedule opns from oprs *) 
  let deschedule_opns oprs opns graph =
    let index = ref ((List.length oprs)-1) in
    List.iter (fun opr ->
		 let opn = List.nth opns !index in
                   delete_coms opn graph; 
		   deschedule opn opr false;
                   index := !index-1; 
               ) (List.rev oprs)  

  (* schedule opns into oprs *)
  let schedule_opns oprs opns graph rt_opns =
    let index = ref 0 in
    let eefs = List.fold_left
       (fun tmp_eefs opr ->
         let opn = List.nth opns !index in
           index := !index + 1;
	 schedule opn opr false 0. 0.;   
         let delta = delta opn.t_opn_class opr in
         let esfs = make_coms opn graph false rt_opns in 
         let eefs = esfs +. delta in 
         max eefs tmp_eefs) 0. oprs in
      eefs   

(* Compute the reliability *)
let compute_rel opn oprs =  
  let index = ref 0 in
  let non_rel = 
    List.fold_left (fun tmp_mul opr -> 
        let lambda = (Hashtbl.find failures_rate (name_of_operator opr)) in 
        let exec = (delta opn.t_opn_class opr) in
        (tmp_mul *. (1. -. (exp(-.(lambda*.exec)))))) 1. oprs in
   (1. -. non_rel)

  (* Returns the optimal esfs, schedule pressure and operator for operation. 
     Fails if no operator is able to execute this operation *)
(*  let best_oprs_comb opns graph =
    let rt_opns = List.fold_left  
                     (fun rts rt_opn -> (schedule_routing_opn rt_opn graph)::rts)
                     [] (predecessors (List.hd opns)) in
    let best_comb,best_comp = Hashtbl.fold  
       (fun _ oprs (tmp_comb,tmp_comp) ->
          (*ps ("\n start combainison ");
          ps (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) "  candidats  = " oprs);
          ps (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) 
                    ((string_of_float tmp_comp)^"  bests  = ") tmp_comb);*) 
          let eefs = (schedule_opns oprs opns graph rt_opns) in
            deschedule_opns oprs opns graph; 
          let new_len = max eefs !old_len in
          let new_rel = !old_rel *. (compute_rel (List.hd opns) oprs) in
          let new_comp = (compromise new_rel new_len) in   
           if (new_comp < tmp_comp)  
           then (oprs,new_comp)
           else (tmp_comb,tmp_comp)) procs_comb ([],max_float)  in 
    deschedule_routing_opns rt_opns graph;
(*    ps (List.fold_left (fun s opr -> s^" "^(name_of_operator opr))
       (""^(string_of_float best_eefs)^"  end bests  = ")  best_comb);*)
   (* ps ("opn = "^(name_of_operation (List.hd opns))^" nb oprs =  "^(string_of_int (List.length best_comb))
        ^" nb rts "^(string_of_int (List.length rt_opns)));ps "================";*) 
    (rt_opns,best_comb,best_comp) 
      *)

 (*  compare sp first and esfs next *)
  let compares opt_sp opt_esfs sp esfs =
    if opt_sp>sp 
    then 1
    else if opt_sp=sp 
         then if opt_esfs > esfs
              then 1
              else 0 
         else 0

(* Returns the optimal esfs, schedule pressure and operator for operation. 
     Fails if no operator is able to execute this operation *)
  let best_oprs_esfs_sp original graph =
    let operators = (operators_constraint original) in
    let opnlib,opnname = deflibname original in
    let rt_opns = List.fold_left  
                     (fun rts rt_opn -> ((schedule_routing_opn rt_opn graph))::rts)
                     [] (predecessors original) in
    let opns_sp = List.fold_left  
       (fun opts operator -> 
  	 debug_ps 2 debug_level("\nTrying "^(name_of_operation original)^" on "^(name_of_operator operator));
	 match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
	 | true -> let delta = delta original.t_opn_class operator in
		   let esfs = get_esfs original operator graph rt_opns in
		   let spfast = esfs +. delta in  
                      (operator,esfs,spfast)::opts
	 | false -> opts) [] operators in
      deschedule_routing_opns rt_opns graph;
      let oprs_opts = (keep_npf_elements (!level_max+1) (List.sort 
                             (function _,opt_esfs,opt_sp -> function _,esfs,sp -> 
                             (compares opt_sp opt_esfs sp esfs)) opns_sp))  in
           match ((List.length oprs_opts)< !level_max) with  
	   | true -> failwith ("less than "^(string_of_int (!level_max+1))^" operators able to execute "
			     ^(string_of_int (!level_max+1))^" replicas of "^(name_of_operation original))
	   | false -> 
                 (rt_opns,(List.map (function opr_opt,esfs_opt,sp -> 
		          let sp = (lefe original)+. esfs_opt+. (delta original.t_opn_class opr_opt) in  
			  (opr_opt)) oprs_opts),
                          (List.fold_left (fun new_sp (_,_,sp) -> max new_sp sp) min_float oprs_opts)
                 ) 

 let find_best_candidate operations_operatoropt = 
      List.fold_left (fun (opns_max,(rts_max,oprs_max,sp_max)) (opns,(rts,oprs,sp)) ->  
			  match (sp_max < sp) with
			    | true  -> (opns,(rts,oprs,sp))
			    | false -> (opns_max,(rts_max,oprs_max,sp_max))
		     ) (List.hd operations_operatoropt) operations_operatoropt

  (* Create level_max replicas of opn *)
  let rec create_copies operation graph count =   
    match count with
      | 0 -> []
      | _ -> let copie = (create_copie operation graph true count) in
	       Hashtbl.add graph (identifier_of_operation copie) copie;  
               (*update_replicas copie;*)
               copie::(create_copies operation graph (count-1)) 

(** Updates the list of schedulables, considering that operation has
    just been scheduled *)
  let succs_successors opn = 
     List.fold_left (fun succs succ -> (succ.t_opn_successors)@succs) [] opn.t_opn_successors

  let preds_predecessors opn = 
     List.fold_left (fun preds pred -> (pred.t_opn_predecessors)@preds) [] opn.t_opn_predecessors

(** Returns true if operation has already been scheduled *)
let is_scheduled operation =
  (is_constant operation) || (operation.t_opn_operator <> None)

(** Returns true if operation is schedulable *)
let is_it_schedulable operation =
  match (is_scheduled operation) with
  | true -> false
  | false ->
      let preds = match (is_memory operation) with
      | true -> failwith ("not considered")
      | false -> preds_predecessors operation in
      List.for_all
	(fun opn -> (is_scheduled opn) || (is_memory opn) || (is_constant opn))
	preds

(***** remove copies of operation whici is not scheduled *****)
 let update_opns_copies opns index graph =
   (*ps ("remove from "^(string_of_int index)^" to remove "^(string_of_int (List.length opns)));*)
   for i=index to !level_max do 
     remove_operation graph (List.nth opns i);
     (*ps ("remove  "^(name_of_operation (List.nth opns i)));*)
   done 


  let update_schedulables graph operation schedulables =
    schedulables := List.filter (fun opns -> (List.hd opns)<>operation) !schedulables;
    let new_opns = (succs_successors operation) in
     (*ps (List.fold_left (fun s -> fun opn -> s^(name_of_operation opn)^" ") " new opns = " new_opns);*)
    let new_schedulables = List.filter (fun opn -> (is_it_schedulable opn)) new_opns in 
     (*ps(List.fold_left (fun s->fun opn->s^(name_of_operation opn)^" ") "new sched = " new_schedulables);*)
      schedulables := !schedulables@(List.map 
				       (fun opn ->  opn::(create_copies opn graph !level_max)
				       ) new_schedulables)

  (* Schedule all the copies of an optimal operation *)
  let schedule_opts opts graph = 
    List.iter (fun (opt,opr) ->  
	     (* schedule optimal operation *)                         
	     schedule opt opr false 0. 0.;
	     let delta_opt = delta opt.t_opn_class opr in  
	     let esfs_opt = 
	       make_coms opt graph true [] in 
             let eefs_opt = (esfs_opt+.delta_opt) in
	       schedule opt opr true esfs_opt eefs_opt;
               old_len := max !old_len eefs_opt;    
	       (*ps ("\n fin schedule "^(name_of_operation opt)^" into "^(name_of_operator opr)
		   ^"  at  "^(string_of_float opt.t_opn_esfs)^"\n");*)
	       (* Data becoming available on operator *)  
	       List.iter 
		 (function prt -> match (prt.t_prt_dir,prt.t_prt_class) with
		    | Port.Out, Port.Data_Port | Port.Out, Port.Init_Memory_Port -> 
			debug_ps 4 debug_level 
			("Adding "^(name_of_data (opt,prt))^" on "^(name_of_operator opr)^"(with cond "
			 ^(string_of_condlist opt.t_opn_condition)^")");
			update_datas opr (opt,prt) opt prt (eefs opt)
		    | _,_ -> ()
		 ) opt.t_opn_ports; 
	) opts
       

(******************************)
(**** graph transformation ****)
(******************************)
(*  create a routing operation operation *)
  let create_copie_routing operation = 
    let name_copie = ("_1"^"#") in   
    let rtopn = (new_opn operation.t_opn_path operation.t_opn_class operation.t_opn_arguments_values 
                         operation.t_opn_ports [] [] operation.t_opn_condition None 0 0. true 
                         operation.t_opn_origin None operation.t_opn_xsc_name 
                         operation.t_opn_referencing_alglib operation.t_opn_referencing_algname 
                         operation.t_opn_code_phases) in       
      rtopn.t_opn_path <- (List.rev (List.tl (List.rev rtopn.t_opn_path)))@
                          [(List.hd (List.rev rtopn.t_opn_path))^name_copie]; 
      rtopn.t_opn_ports <- [];
      rtopn.t_opn_xsc_name <- operation.t_opn_xsc_name; 
      rtopn

 (* add routing operation  between two dependent opns A and B. 
    Exp. A --> B .... become .... A --> R_{AB} ---> B *)
  let create_routing_opn dpd rtopn =   
    let sprt, sopn, dprt, dopn = dpd.t_dpd_sprt, dpd.t_dpd_sopn, dpd.t_dpd_dprt, dpd.t_dpd_dopn in    
    let rt_opn = (create_copie_routing rtopn) in
      let name_dopn =  (name_of_operation dopn) in
	rt_opn.t_opn_path <- ["S_"^(name_of_operation sopn)^((name_of_operation dopn))];   	
	let prt_in =
	  {t_prt_name=dprt.t_prt_name;t_prt_dir=dprt.t_prt_dir;t_prt_typename=dprt.t_prt_typename;
           t_prt_class=dprt.t_prt_class;t_prt_dim=dprt.t_prt_dim;t_prt_order=dprt.t_prt_order}  in  
	  (*sopn.t_opn_ports <- prt_out :: sopn.t_opn_ports;*)  
	  rt_opn.t_opn_ports <- prt_in  :: rt_opn.t_opn_ports; 
	  ignore(backup_dependence_add sopn sprt rt_opn prt_in dpd.t_dpd_class dpd.t_dpd_condition true);
	    (* add dependences between routing operation and succ *)
	let prt_out = 
          {t_prt_name=sprt.t_prt_name;t_prt_dir=sprt.t_prt_dir;t_prt_typename=sprt.t_prt_typename;  
	   t_prt_class=sprt.t_prt_class;t_prt_dim=sprt.t_prt_dim;t_prt_order=sprt.t_prt_order}  in   
	   rt_opn.t_opn_ports <- prt_out :: rt_opn.t_opn_ports; 
          ignore(backup_dependence_add rt_opn prt_out dopn dprt dpd.t_dpd_class dpd.t_dpd_condition true);
	   (* schedule routing operation *)
           (*ps ("create routing operation");ps_dpds rt_opn;*)
         rt_opn	      

(***  ADD ROUTING OPERATION ***)
 let add_routing_opn opn rt_opn graph =
   List.iter (fun pred_dpd -> let new_opn = create_routing_opn pred_dpd rt_opn in
            dependence_remove pred_dpd;
            Hashtbl.add graph (identifier_of_operation new_opn) new_opn) opn.t_opn_dependences_predecessors

let ps_schedulable schedulables = 
  List.fold_left (function s -> function opns ->  
        s^(identifier_of_operation (List.hd opns))^" ") 
	" schedulables : " !schedulables 

(**** Update reliability ****)
let update_rel opn oprs =
  old_rel := !old_rel *. (compute_rel opn oprs)

(*******************************)
(* FAULT TOLERANCE ADEQUATION  *)
(*******************************)
  let fault_tolerance_adequation graph =
    let nb_opns = ref 0 in
      old_len := 0. ;
      old_rel := 1.;
      (*level_max := 2;*)
	  let s = Hashtbl.fold 
		   (fun _ opn s -> 
		      s^"\n"^(identifier_of_operation opn)^(string_of_argsvalues opn.t_opn_arguments_values))
		   graph "Initial graph :" in
	   debug_ps 3 debug_level s; 
	   fault_architecture_init graph;      
	   Hashtbl.clear lefe_table;  
	   adequation_order := 0;
	   (* Create a new operation for routing operation R without preds and succs *)
	   let routing_opn = new_opn ["S"] (Calcul (Operation,"","S")) [] (precedence_ports ()) [] [] 
			       [(None,None)] None 0 0. true (Ihm ["S"]) None 
			       "" "" "" [InitSeq;LoopSeq;EndSeq] in 
	   let oprs = Application.application.app_operator_definitions in 
	     List.iter 
	       (function operator -> Hashtbl.add operator.oprdef_operation_durations ("","S") 0. )
	       oprs;
	     let all_operators = Application.operators_list !archilib !archiname in 
	     let all_medias    = Application.media_list     !archilib !archiname in
	       (* test if all media are SamPP or SamMP *)
	       let step total = match total<100 with 
		 | true -> 1
		 | false -> total/100 in
	       let total_adequation = (hashtbl_length graph) in                
	       let nb_oprs = List.length all_operators in
                  
	       (*let progress_box = Progress_box.create total_adequation in*)
               (***** initialize data for reliability  *****) 
               initialize_failure_rates_table  !file_name;                
               (**** END initialize data for reliability ****)   
               let opns =  Hashtbl.fold (fun _ opn opns -> opn::opns) graph [] in
               let size = List.length opns in 
                 List.iter (fun opn -> add_routing_opn opn routing_opn graph;
                            (*Hashtbl.add originals_operations (name_of_operation opn) opn;*)) opns;
	       let schedulables_opns = (hashtbl_filter 
					  (fun o -> (is_schedulable o) && (not (is_constant o))
					  ) graph) in 
	       let schedulables = ref (List.map 
					 (fun opn -> opn::(create_copies opn graph !level_max)
					 ) schedulables_opns) in
		 try  
		   while !schedulables <> [] do
   		     debug_ps 2 debug_level (ps_schedulable schedulables); 
		     (* Calcul of the optimal operator for each operation *)
		     let operations_operatorsopts = 
		        List.map 
			 (function opns -> opns,(best_oprs_esfs_sp (List.hd opns) graph)) !schedulables in

		     let opns_opts,(rts_opts,oprs_opts,_) = 
                          find_best_candidate operations_operatorsopts in 
                     (*ps "\n -----------------------------------------------------------------------"; 
                     ps ("schedule "^(name_of_operation (List.hd opns_opts))); 
                     ps "-----------------------------------------------------------------------\n";*) 
		       (* schedule all routing operation and copies of optimal operation *)  
                     let rec get_couple opns oprs =
                        match (opns, oprs) with
                        |  (_, []) -> []
                        |  (opn::sopns, opr::soprs) -> (opn,opr):: (get_couple sopns soprs)
                        |  (_, _) -> invalid_arg "get_couple" in
                     let opts = (get_couple opns_opts oprs_opts) in
                     let rts = List.map (fun (opn,opr,_) -> (opn,opr)) rts_opts in 
		       schedule_opts rts graph;
                       schedule_opts opts graph;
                       nb_opns := !nb_opns + (List.length oprs_opts); 
                       (*ps ("| > NB rep of  "^(name_of_operation (List.hd opns_opts))^" is "^
                          (string_of_int  (List.length oprs_opts))^"         ");*)
                       update_rel (List.hd opns_opts) oprs_opts;
  		       (* Update the list of opns_opts copies *)
		       update_opns_copies opns_opts (List.length oprs_opts) graph;
                       (* Update the list of schedulables operations *)
		       update_schedulables graph (List.hd opns_opts) schedulables;
		       (*Progress_box.tick progress_box;*)
		   done; 
                   rate_replication := float_of_int(!nb_opns) /. float_of_int(size); 
                    

              (*ps "";
	      ps (" ___________________________________________________ ");
	      ps ("|                                                   |");
	      ps ("| > Cycle time is "^(string_of_float  !old_len)^"                                ");
	      ps ("| > Reliability is "^(string_of_float !old_rel)^"                   "); 
	      ps ("| > Rate replication is "^(string_of_float !rate_replication)^"                   "); 
	      ps ("|___________________________________________________|");*)
		   (* We need to have the list of operations sorted by esfs (for code generation) *)
                  Hashtbl.iter
		   (fun _ (_,oprschedule) ->
		     let opns = Hashtbl.fold (fun _ {t_sch_operations=cond_opns} opns -> opns@cond_opns)
                         oprschedule [] in
	             let opns_sorted = 
			List.stable_sort 
		        (function opn1,esfs1,eefs1 -> function opn2,esfs2,eefs2 -> match compare esfs1 esfs2 with
			 | 0 -> (match (esfs1 = eefs1) && (esfs2=eefs2) with
                                (* two successive null duration operations*)
			       | true -> compare opn1.t_opn_adequation_order opn2.t_opn_adequation_order 
			       | false -> compare eefs1 eefs2)
			 | x -> x
			 ) opns in 		
		       ignore (List.fold_left (function i -> function opn,_,_ -> 			       
			        opn.t_opn_rank <- i; i+1) 0 opns_sorted)
		     ) schedules;     
		   graph,schedules  
            with exn -> (*Progress_box.close progress_box;*)raise exn
  			    

(** Returns [p] if p is a calcul operation otherwise the full sender
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

(** Returns [opn] if opn is a calcul operation otherwise the full
   sender paths to receivers of communication opn *)
let rec transitive_successors opn =
  let succs = successors opn in
  match opn.t_opn_class with
  | Communication (Send _) ->
      let succs = union (List.map successors (successors opn)) in
      opn::(union (List.map transitive_successors succs))
  | Communication (Reliable_Send _) ->
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
let pretty_conv_cond_list cl =
  List.map pretty_conv_cond cl

(** Returns opn in a prettier format, that is easier to use for
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

(** Adds operation to schedules. Operation is added in either the
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

