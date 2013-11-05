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
  let schedule_length = ref 0.
  let schedule_reliability = ref 0.
  let schedule_big_lambda = ref 0.

  let deltas_sum = ref 0.

  let old_len = ref 0.
  let new_len = ref 0.
  let obj_len = ref 0.

  let old_rel = ref 1.
  let new_rel = ref 0.
  let obj_rel = ref 0.

  let mean_lambda_opr = ref 0.
  let mean_lambda_mda = ref 0.

  let obj_big_lambda = ref 0.

  let schedule_failure = ref false  

  let rate_replication = ref 0.
  let replication_process = ref []

(*  let replication_process_proc =  Hashtbl.create 6 *)
  
  let procs_comb  = Hashtbl.create 10
  let opns_rel_table    = Hashtbl.create 10

  let theta = ref 45. 
 
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
  not ((!test > (!level_max+1))||(!test=0))


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

(*let initialize_failure_rates_table  file_name  =     
  let (algo_lib,algo_name,_),(archilib,archiname) = 
     Application.algo_main_get (), Application.archi_main_get () in 
     let all_operators = Application.operators_list archilib archiname in 
        let name_fileRel = (String.sub file_name 0 (String.rindex file_name '.'))^".rel" in 
        let fileRel = open_in name_fileRel  in 
        let ch = ref "" in 
        List.iter (fun   opr -> ch := (input_line fileRel);  
           Hashtbl.add failures_rate (name_of_operator opr) (float_of_string !ch)) all_operators; 
        close_in fileRel*) 

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
    (*ps ((name_of_operator o1)^" ---->"^(name_of_operator o2));*)
    let no_crossing r1 r2 = 
      let r1 = remove_copies (List.map fst r1)
      and r2 = remove_copies (List.map fst r2) in
	(intersection r1 r2)=[] in
    (*ps "start --------- route";*)
    let archilib,archiname = Application.archi_main_get () in
    let links o = Architecture.links archilib archiname o in
    let rec r o1 o2 route =
      ps ((name_of_operator o1)^" ---->"^(name_of_operator o2));
      match Hashtbl.mem route_table (o1,o2) with
	| true -> (* this route already calculated *)
            
	    let subroutes = Hashtbl.find route_table (o1,o2) in
	      List.filter (no_crossing route) subroutes
	| false ->
            let links = List.filter (function (og1,og2) -> no_crossing [og2] route) (links o1) in
              (* links : list of links from o1 not in ll *)
	    let subroutes = List.map (function (og1,(opr2,_)) as lnk -> ps (name_of_operator opr2);(lnk,(r opr2 o2 (og1::route)))) links in
	    let subroutes = List.concat (List.map (function (og1,og2),rlist -> List.map (function r -> og1::og2::r) rlist) subroutes) in
              (* subroutes : list of routes from o1 to o2 *)
	      (*let subroutesmin = list_min_elements List.length subroutes 0 in*)
	    let subroutesmin = subroutes in
              (* list of shortest routes from o1 to o2 *)
	      List.map remove_copies subroutesmin in
    (*ps "...............";*)
    let routes = (match Hashtbl.mem route_table (o1,o2) with
		    | true  -> (Hashtbl.find route_table (o1,o2))
		    | false -> 
                        let routes = r o1 o2 [] in 
                        (*ps "end     ............ jjjj";*)
                        ps ((name_of_operator o1)^" ---->"^(name_of_operator o2));
                        List.iter (fun r1 ->   
                                     ps (List.fold_left (fun s (og1,opr2) -> (s^" "^(name_of_operator og1))) " " r1)) routes;
                        ps (".................."^(string_of_int (List.length routes))); 
			Hashtbl.add route_table (o1,o2) routes;
			Hashtbl.add route_table (o2,o1) (List.map List.rev routes);routes) in
    (*ps ";;;;;;;;;;;;;;";*)
    let routes = List.filter (function route ->     
        let opr_route = (List.map fst route) in (intersection opr_route exclusives_routes)=[]) routes in
      (*ps ("find route for "^(string_of_int (List.length (list_min_elements List.length routes 0))));*) 
      (*ps "------------- route";*)
    let route = 
       try List.hd (list_min_elements List.length routes 0) 
      with  _ -> raise (Failure ("No route "^(name_of_operator o1)^" to "^(name_of_operator o2)^" !!")) in
      (*ps "fin route";*)  
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

  (*  Return the number of operation copies which are scheduled 
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
      (Hashtbl.replace replicas_operations original replicas) *)

  (* update the list of operation copies : remove 
  let remove_replicas replica  = 
    let original = (get_original replica) in
    let replicas = (match (Hashtbl.mem replicas_operations original) with
       	              | true  -> List.tl (Hashtbl.find replicas_operations original)  
		      | false -> (Hashtbl.find replicas_operations original)) in
      (Hashtbl.replace replicas_operations original replicas) *)


  (* Place in graph the primary communication corresponding to dependence dpd *) 
  let place_communications graph status tcond dpd (*rt_opns*) =

  let place_com (data,(operator_src,operation_src,port_src,t0))
	  operation_dst port_dst operator_dst dpdclass cond
	  place status =
	
	let rec place_com_aux sopn sprt eefs place status opr_src = 
	   (*ps ("from "^(name_of_operator opr_src)^" to "^
	     (name_of_operator operator_dst)^" placing "^
	     (name_of_operation sopn));*)
	  match opr_src =@ operator_dst with
	  | true -> sopn,sprt,eefs
	  | false -> 
    	      let lnks = shortest_links opr_src operator_dst in
	       (*ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^
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
        (*ps "5";*)
       match (opr_src=@operator_dst) with
	| true  -> sopn,sprt,eefs
	| false -> match route with 
	    | [] -> raise (Failure "place_copie_com_aux no exclusive route ...") 
	    | og1::og2::og3::rest -> let best_lnk = (og1::og2::[og3]) in                 
	      let (opn,prt,eefs),next_opr = 
                reliable_place_link best_lnk place sopn sprt eefs data graph cond status dpdclass in
                place_SamPP_com_aux opn prt eefs place status next_opr rest 
	    |  _ -> raise (Failure "place_copie_com_aux no exclusive route")  in
	
         (*ps "3";*)
	let eefs = max t0 tcond in
	(*       ps ("placing com from "^(name_of_operator operator_src)); *)
         (*ps "6";*)
	let opnpred,prtpred,eefs =
        let route = (routes operator_src operator_dst []) in 
        (*ps "7";*) 
	       place_SamPP_com_aux operation_src port_src eefs place status operator_src route in
        (*ps "4";*)
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
	                          (fun (operator_src,opn,port_src,t0) -> opn =$ sopn)operators_src_list) in
      let f (o,_,_,_) = match o with
	| Media mda -> mda.mdaref_links2.((operator_of_operator_class operator_dst).oprref_id)
	| Operator opr -> opr.oprref_links2.((operator_of_operator_class operator_dst).oprref_id) in
      let operators_src_list = (list_min_elements f operators_src_list 0) in      
      let best_operator_src = ref 
        (try match operators_src_list with
           | [] ->  raise (Failure "Ajri hna -------------")
	       (*let rt,sopr,rt_eefs = List.find (fun (rt,opr,rt_eefs) -> rt=$sopn) rt_opns in 
                 (sopr,rt,sprt,rt_eefs)*)
           | src -> (List.hd src)   
	 with  _ -> raise (Failure "Add disjoints links to architecture!!!"))  in
	 (* Place the communication *)
	(*place_com (data, !best_operator_src) dopn dprt operator_dst graph tcond dpdclass cond true status*) 
      let eefs = place_com (data, !best_operator_src) dopn dprt operator_dst dpdclass cond true status in
      eefs




 
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

(**************************************)
(* Builds operation predecessors coms *)
(**************************************)
  let make_coms operation graph status (*rt_opns*) =
    debug_ps 3 debug_level ("making comm "^(name_of_operation operation));
    let make_one_com tcond dpd =       
	match (pi dpd.t_dpd_sopn =@ pi operation) with 
	  | true  -> eefs dpd.t_dpd_sopn
	  | false ->  
	      let best_eefs = 
                  (*ps "1111111111111";*)  
		place_communications graph status tcond dpd (*rt_opns*) in 
                  (*ps "2222222222222";*)
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
  let fault_architecture_init _obj_len _obj_big_lambda _theta _level_max graph = 
    architecture_init graph;
    obj_len := _obj_len;
    obj_big_lambda := _obj_big_lambda;
    obj_rel := exp(-. (!obj_big_lambda *. !obj_len));
    level_max := _level_max;
    theta := _theta;      
    old_len := 0. ;
    old_rel := 1.;    
    deltas_sum := 0.;
    rate_replication := 0.;
    replication_process := [];
    scheduled_opns := [];
    Hashtbl.clear replication_process_proc;
    Hashtbl.clear lefe_table;
    Hashtbl.clear opns_rel_table;  
    adequation_order := 0;
    Hashtbl.clear route_table;
    Hashtbl.clear failures_rate;
    Hashtbl.clear procs_comb;
    schedule_failure := false;
    let oprlist = List.map (Architecture.operator_reference !archilib !archiname ) (Architecture.operators_list !archilib !archiname) in
    let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
      List.iter (function o -> (Hashtbl.add route_table (Operator o, Operator o) [[]]);) oprlist; 
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

  (* get esfs for operation *)
  let get_esfs operation operator graph (*rt_opns*) = 
    debug_ps 3 debug_level ("Try minimize start time of "^(name_of_operation operation));    
    debug_ps 3 debug_level "schedule operation....................";
    schedule operation operator false 0. 0.;  
    debug_ps 3 debug_level "make coms operation....................";
    let esfs_operation =  
      make_coms operation graph false (*rt_opns*) in
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
		  rt_opn.t_opn_operator <- None;
               ) (List.rev rt_opns)  

  (* schedule routing opns *)
  let schedule_routing_opn rt_opn graph =
    let operators = (operators_constraint rt_opn) in
    let opnlib,opnname = deflibname rt_opn in
    let opns_sp = List.fold_left
       (fun opts operator -> 
  	 debug_ps 2 debug_level("\nRouting operation : Trying "^(name_of_operation rt_opn)^" on "^(name_of_operator operator));
	 match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
	 | true -> let delta = delta rt_opn.t_opn_class operator in
		   let esfs = get_esfs rt_opn operator graph (*[]*) in                     
		   let spfast =  esfs +. delta in  
                     (operator,esfs,spfast)::opts
	 | false -> opts) [] operators in
    let opr_opt,esfs_opt,sp = 
      (List.hd (list_min_elements (fun (opr,esfs,sp) -> sp) opns_sp 0.)) in
    schedule rt_opn opr_opt false 0. 0.; 
    let delta_rt = delta rt_opn.t_opn_class opr_opt in
    let esfs = make_coms rt_opn graph false (*[]*) in  
      rt_opn,opr_opt,(esfs +. delta_rt) 


 (*deschedule opns from oprs *)
  let deschedule_opns oprs opns graph =
    let index = ref ((List.length oprs)-1) in
    List.iter (fun opr ->
		 let opn = List.nth opns !index in
		   delete_coms opn graph; 			 		   
		   deschedule opn opr false;
		   opn.t_opn_operator <- None;
                   index := !index-1; 
               ) (List.rev oprs)  
(************************************************)
(***** schedule opns into oprs ******************)
(************************************************)
  let esfs_schedule_opns oprs opns graph (*rt_opns*) =    
    debug_ps 2 debug_level ("\nOperation : Trying "^(name_of_operation (List.hd opns))^" on "^
			      (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) "" oprs));
    let index = ref 0 in    
    let esfs = List.fold_left
       (fun tmp_esfs opr ->
         let opn = List.nth opns !index in         
           index := !index + 1;
         schedule opn opr false 0. 0.;   	 
         let delta_opn = delta opn.t_opn_class opr in	 
	 let esfs = make_coms opn graph false (*rt_opns*) in 
	 let delta_if_routing_opn =  (match (is_routing_operation opn) with
	                                | false -> 0. 
					| true  -> delta (List.hd opn.t_opn_successors).t_opn_class opr) in 
    	 let esfs = esfs +. delta_if_routing_opn  in 	 
         (esfs,delta_opn)::tmp_esfs) []  oprs in
    (* schedule opns successors when oprs length is > 1 *)
      esfs    


let abss n = 
  match n>0. with
    | true -> n 
    | false -> (-. n)
 


(* Compute the reliability *)
let old_compute_rel all_opns scheduled_opns =      
  match (is_routing_operation (List.hd all_opns)) with
  | false ->
      let non_rel = 
	List.fold_left (fun tmp_non_rel opn ->
			  match (!scheduled_opns>0) with
			  | false -> tmp_non_rel
			  | true  -> scheduled_opns := !scheduled_opns - 1;
			      let opn_rel_comm = 
				(List.fold_left (fun comm_rel send -> match (is_communication send.t_dpd_sopn) with
						 |false -> comm_rel;
						 |true -> let recv =  (List.hd (send.t_dpd_sopn).t_opn_predecessors) in					
						   let lambda_mda = (Hashtbl.find failures_rate (pi recv)) in 
						   let com_exe    = (delta recv.t_opn_class  (pi recv)) in    
						   comm_rel +. (lambda_mda*.com_exe);
						) 0. opn.t_opn_dependences_predecessors) in
			      let opn_exe    = delta opn.t_opn_class  (pi opn) in    
			      let lambda_opr = Hashtbl.find failures_rate (pi opn) in  
			      let opn_rel    = exp(-.(opn_rel_comm+.(lambda_opr*.opn_exe))) in 
			      (tmp_non_rel *. (1. -. opn_rel))) 1.  all_opns in
      (1. -. non_rel) 
    | true ->  
	let opn = List.hd all_opns in
	let non_rt_rel_comm = 
	  (List.fold_left (fun tmp_comm_rel send -> match (is_communication send.t_dpd_sopn) with
			   |false -> tmp_comm_rel;
			   |true -> let recv =  (List.hd (send.t_dpd_sopn).t_opn_predecessors) in					
			     let lambda_mda = (Hashtbl.find failures_rate (pi recv)) in 
			     let com_exe    = (delta recv.t_opn_class  (pi recv)) in   
			     tmp_comm_rel *. (1. -.  exp(-.(lambda_mda*.com_exe)));			     
			  ) 1. opn.t_opn_dependences_predecessors) in
	let opn_exe    = delta opn.t_opn_class  (pi opn) in    
	let lambda_opr = Hashtbl.find failures_rate (pi opn) in  
	let opn_rel    = exp(-.(lambda_opr*.opn_exe)) in 
	((1. -. non_rt_rel_comm) *. opn_rel)  
   

(** ps predecessors *)
let ps_pred opns = 
  List.iter (fun opn -> ps (List.fold_left (fun s pred -> 
					      s^" , "^(name_of_operation pred.t_dpd_sopn)
					   ) (name_of_operation opn) opn.t_opn_dependences_predecessors);
	    ) opns
  


(* Compromise value *) 
let compromise _ILen _IRel _ILambda =
  (*let gain = ((new_len -. !old_len) /. (!obj_len -. !old_len)) in   
    let loss = ((new_big_lambda -. !old_big_lambda) /. (!obj_big_lambda -. !old_big_lambda)) in 
    let loss = abs_float ((new_rel  -. !old_rel) /. (!obj_rel -. !old_rel)) in  *)
  _ILambda
  (*(((cos ((!theta *. 3.14) /. 180.)) *. gain) +. ((sin ((!theta *. 3.14 ) /. 180.)) *. loss)),gain,loss*)
       

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
      | false -> predecessors operation in
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
   schedulables := List.filter (fun opns -> (List.hd opns)<>$operation) !schedulables;
   let new_opns = (*succs_*)(successors operation) in
   debug_ps 10 debug_level (List.fold_left (fun s -> fun opn -> s^(name_of_operation opn)^" ") " new opns = " new_opns);
   let new_schedulables = List.filter (fun opn -> (is_it_schedulable opn)) new_opns in 
   debug_ps 10 debug_level (List.fold_left (fun s->fun opn->s^(name_of_operation opn)^" ") "new sched = " new_schedulables);
   schedulables := !schedulables@(List.map (fun opn ->  
					      if (is_routing_operation opn) 
					      then [opn]
					      else opn::(create_copies opn graph !level_max)) new_schedulables)

  (* Schedule all the copies of an optimal operation *)
  let schedule_opts opts graph = 
    List.iter (fun (opt,opr) ->  
	     (* schedule optimal operation *)                         
	     schedule opt opr false 0. 0.;
	     let delta_opt = delta opt.t_opn_class opr in  
	     let esfs_opt = 
	       make_coms opt graph true (*[]*) in 
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
    rt_opn.t_opn_path <- ["S_"^(name_of_operation sopn)^((name_of_operation dopn))^"%"];   	
    let prt_in =
	  {t_prt_name=dprt.t_prt_name;t_prt_dir=dprt.t_prt_dir;t_prt_typename=dprt.t_prt_typename;
           t_prt_class=dprt.t_prt_class;t_prt_dim=dprt.t_prt_dim;t_prt_order=dprt.t_prt_order}  in  
	rt_opn.t_opn_ports <- prt_in  :: rt_opn.t_opn_ports; 
	ignore(backup_dependence_add sopn sprt rt_opn prt_in dpd.t_dpd_class dpd.t_dpd_condition true);
	  (* add dependences between routing operation and succ *)
	  let prt_out = 
          {t_prt_name=sprt.t_prt_name;t_prt_dir=sprt.t_prt_dir;t_prt_typename=sprt.t_prt_typename;  
	   t_prt_class=sprt.t_prt_class;t_prt_dim=sprt.t_prt_dim;t_prt_order=sprt.t_prt_order}  in   
	rt_opn.t_opn_ports <- prt_out :: rt_opn.t_opn_ports; 
        ignore(backup_dependence_add rt_opn prt_out dopn dprt dpd.t_dpd_class dpd.t_dpd_condition true);
	  (* schedule routing operation *)	
          rt_opn	      

(***  ADD ROUTING OPERATION S_{AB}  for  A --> B *)
 let add_routing_opn opn rt_opn graph =
   List.iter (fun pred_dpd -> let new_opn = create_routing_opn pred_dpd rt_opn in
            (*dependence_remove pred_dpd;*)
            Hashtbl.add graph (identifier_of_operation new_opn) new_opn) opn.t_opn_dependences_predecessors

let ps_schedulable schedulables = 
  List.fold_left (function s -> function opns ->  
        s^(identifier_of_operation (List.hd opns))^" ") 
	" schedulables : " !schedulables 

(********************************************************************)
(*****  COMPUTE RELIABILITY WITH PREDS AND SUCCS COMMUNICATIONS  ****)
(********************************************************************)
let re_compute_rel_with_com all_opns scheduled_opns =   
  let non_rel = ref 1. in
  for i=1 to !scheduled_opns do			  
    let next_opn = List.nth all_opns (i-1) in
    (*** compute reliability of the all send communication for next_opn ***)
    let succ_comm_lambda_exe = 
      (List.fold_left (fun comm_rel send -> match (is_communication send) with
		       |false -> comm_rel;
		       |true ->
			  let lambda_mda = (Hashtbl.find failures_rate (pi send)) in 
			  let com_exe   = (delta send.t_opn_class (pi send)) in    
	  debug_ps 10 debug_level ((name_of_operation next_opn)^"   comms  "^(name_of_operation send)^"  "^(string_of_float (lambda_mda *. com_exe)));
			  comm_rel +. (lambda_mda *. com_exe)) 0. next_opn.t_opn_successors) in
    (*** compute reliability of the all receive communication for next_opn when the sender is a routing operation ***)
    let pred_comm_lambda_exe = 
      (List.fold_left (fun comm_rel recv -> match (is_communication recv) with
		       | false -> comm_rel;
		       | true ->
			   (let comm_send = (List.hd recv.t_opn_predecessors) in
			    match (is_routing_operation (List.hd recv.t_opn_predecessors)) with
			    | false -> comm_rel
			    | true  -> 
				let lambda_mda = (Hashtbl.find failures_rate (pi comm_send)) in 
				let com_exe   = (delta comm_send.t_opn_class (pi comm_send)) in    
				debug_ps 10 debug_level ((name_of_operation next_opn)^"  comms  "^(name_of_operation comm_send)
					 ^"  "^(string_of_float (lambda_mda *. com_exe)));
			  comm_rel +. (lambda_mda *. com_exe))
		      ) 0. next_opn.t_opn_predecessors) in

    let opn_exe    = delta next_opn.t_opn_class  (pi next_opn) in    
    let lambda_opr = Hashtbl.find failures_rate (pi next_opn) in
    non_rel := !non_rel  *. (1. -. exp(-. ((pred_comm_lambda_exe) +. (succ_comm_lambda_exe) +. (lambda_opr *. opn_exe))));
    debug_ps 10 debug_level ((name_of_operation next_opn)^" =  "
			     ^(string_of_float  (1. -. exp(-. ((pred_comm_lambda_exe) +. (succ_comm_lambda_exe) +. (lambda_opr *. opn_exe))))));
  done;  
  debug_ps 10 debug_level ((name_of_operation (List.hd all_opns))^","^(name_of_operator (pi (List.hd all_opns)))^" = "^(string_of_float (1. -. !non_rel)));
  (1. -. !non_rel)

(******************************************************************)
(*****  UPDATE OPN RELIABILITY WHEN SCHEDULING ITs SUCCESSORS  ****)
(******************************************************************)
let update_old_reliability scheduled_opn = 
  debug_ps 10 debug_level (" recompute rel for predecessors of "^(name_of_operation scheduled_opn));
  
  let opns_to_recompute_rel = List.map (fun pred_opn ->  match (is_communication pred_opn) with
					  |false -> pred_opn
					  |true ->	      
					     let send = (List.hd pred_opn.t_opn_predecessors) in  
					     (List.hd send.t_opn_predecessors)
				       ) scheduled_opn.t_opn_predecessors in

  debug_ps 10 debug_level (List.fold_left (fun s opn ->  s^" "^(name_of_operation opn)) "befor =  "  opns_to_recompute_rel);
  let opns_to_recompute_rel = List.filter (fun opn -> (is_original opn)) opns_to_recompute_rel in
  debug_ps 10 debug_level (List.fold_left (fun s opn -> s^" "^(name_of_operation opn)) "after =  "  opns_to_recompute_rel);
  
  (match (is_routing_operation scheduled_opn) with
  | true  ->
      List.iter (fun tmp_opn -> 
		   let opns,tmp_rel = Hashtbl.find opns_rel_table (name_of_operation tmp_opn) in
		   let new_rel = re_compute_rel_with_com opns (ref (List.length opns)) in
		   Hashtbl.replace opns_rel_table (get_original tmp_opn) (opns,new_rel);
		   old_rel :=  (!old_rel /. tmp_rel) *. new_rel; 
		   debug_ps 10 debug_level ((name_of_operation tmp_opn)^" on "^(name_of_operator (pi tmp_opn))
		       ^" =  "^(string_of_float tmp_rel)^" ---> "^(string_of_float new_rel));		   
		) opns_to_recompute_rel;
  | false ->
      List.iter (fun tmp_opn -> 
		   match (is_routing_operation tmp_opn) with
		   | true  -> ()
		   | false ->
		       let opns,tmp_rel = Hashtbl.find opns_rel_table (name_of_operation tmp_opn) in
		       let new_rel = re_compute_rel_with_com opns (ref (List.length opns)) in
		       Hashtbl.replace opns_rel_table (get_original tmp_opn) (opns,new_rel);
		       old_rel :=  (!old_rel /. tmp_rel) *. new_rel; 
		   debug_ps 10 debug_level ((name_of_operation tmp_opn)^" on "^(name_of_operator (pi tmp_opn))
		       ^" =  "^(string_of_float tmp_rel)^" ---> "^(string_of_float new_rel));		   
		 ) opns_to_recompute_rel)
  
(***********************************************************************)
(*****  COMPUTE RELIABILITY WITH ONLY ROUTING PREDS COMMUNICATIONS  ****)
(***********************************************************************)
let compute_rel_no_com all_opns scheduled_opns =   
  let non_rel = ref 1. in
  for i=1 to !scheduled_opns do			  
    let next_opn = List.nth all_opns (i-1) in
    (*** compute reliability of the all receive communication for next_opn when the sender is a routing operation ***)
    let pred_comm_lambda_exe = 0.
      (*List.fold_left (fun comm_rel recv -> match (is_communication recv) with
		       | false -> comm_rel;
		       | true ->
			   (let comm_send = (List.hd recv.t_opn_predecessors) in
			    match (is_routing_operation (List.hd recv.t_opn_predecessors)) with
			    | false -> comm_rel
			    | true  -> 
				let lambda_mda = (Hashtbl.find failures_rate (pi comm_send)) in 
				let com_exe   = (delta comm_send.t_opn_class (pi comm_send)) in    
				debug_ps 10 debug_level ((name_of_operation next_opn)^"  comms  "^(name_of_operation comm_send)
							 ^"  "^(string_of_float (lambda_mda *. com_exe)));
				comm_rel +. (lambda_mda *. com_exe))
		      ) 0. next_opn.t_opn_predecessors*) in    
    let opn_exe    = delta next_opn.t_opn_class  (pi next_opn) in     
    let lambda_opr = Hashtbl.find failures_rate (pi next_opn) in
    non_rel := !non_rel  *. (1. -. exp(-. ( pred_comm_lambda_exe +. (lambda_opr *. opn_exe))));
    debug_ps 10 debug_level (string_of_float !non_rel);
  done;  
  debug_ps 10 debug_level ((name_of_operation (List.hd all_opns))^","^(name_of_operator (pi (List.hd all_opns)))^" = "^(string_of_float (1. -. !non_rel)));
  (1. -. !non_rel)

(********************************************************************)
(*****  COMPUTE RELIABILITY WITH PREDS and SUCCS COMMUNICATIONS  ****)
(********************************************************************)
let compute_rel_with_com all_opns scheduled_opns random_medias =   
  let non_rel = ref 1. in
  for i=1 to !scheduled_opns do			  
    let next_opn = List.nth all_opns (i-1) in
    let random_media = List.nth random_medias (i-1) in
    (*** compute reliability of the all receive communication for next_opn when the sender is a routing operation ***)
    let pred_comm_lambda_exe = 
      (List.fold_left (fun comm_rel recv -> match (is_communication recv) with
		       | false -> comm_rel;
		       | true ->
			   (let comm_send = (List.hd recv.t_opn_predecessors) in
			    match (is_routing_operation (List.hd recv.t_opn_predecessors)) with
			    | false -> comm_rel
			    | true  -> 
				let lambda_mda = (Hashtbl.find failures_rate (pi comm_send)) in 
				let com_exe   = (delta comm_send.t_opn_class (pi comm_send)) in    
				debug_ps 10 debug_level ((name_of_operation next_opn)^"  comms  "^(name_of_operation comm_send)
							 ^"  "^(string_of_float (lambda_mda *. com_exe))); 
				comm_rel +. (lambda_mda *. com_exe)) 
		      ) 0. next_opn.t_opn_predecessors) in 
    (*** compute estimation reliability of the all send communication for next_opn ***)
    let succ_comm_lambda_exe = match  !scheduled_opns=(-1) with
      | true  -> 0.
      | false ->
      (List.fold_left (fun comm_rel dpd -> match (is_routing_operation dpd.t_dpd_dopn) with
		       | false -> comm_rel;
		       | true ->
			   let comm_send = Communication (
			     Send ((dpd.t_dpd_sopn,dpd.t_dpd_sprt),operator_of_operator_class (pi next_opn),[operator_of_operator_class (pi next_opn)])) in
			   let lambda_mda = (Hashtbl.find failures_rate random_media) in 
			   let com_exe   = (delta comm_send random_media) in    
			   debug_ps 10 debug_level ((name_of_operation next_opn)^"  comms "^"  "^(string_of_float (lambda_mda *. com_exe))); 
			   comm_rel +. (lambda_mda *. com_exe)) 0. next_opn.t_opn_dependences_successors) in     
    let opn_exe    = delta next_opn.t_opn_class  (pi next_opn) in      
    let lambda_opr = Hashtbl.find failures_rate (pi next_opn) in
    non_rel := !non_rel  *. (1. -. exp(-. ( pred_comm_lambda_exe +. (succ_comm_lambda_exe)  +. (lambda_opr *. opn_exe))));
    debug_ps 10 debug_level (string_of_float !non_rel);
  done;  
  debug_ps 10 debug_level ((name_of_operation (List.hd all_opns))^","^(name_of_operator (pi (List.hd all_opns)))^" = "^(string_of_float (1. -. !non_rel)));
  (1. -. !non_rel)


let reps = ref " "
let xxxx = ref 10000
let yyyy = ref 10000


(**************************************************************)
(*****  RETURNS THE BEST SCHEDULE FOR A SET OF OPERATIONS  ****)
(**************************************************************)
let get_best_oprs_comb opns graph random_media only_length =
(*if (is_routing_operation (List.hd opns)) then xxxx := 1 else xxxx := 0;*)
  let best_comb, best_sp,best_esfs,best_delta,best_rel, best_ILambda = 
    Hashtbl.fold  (fun _ try_oprs_comb (tmp_comb,tmp_sp,tmp_esfs,tmp_delta,tmp_rel,tmp_ILambda) ->
		     match ((List.length opns)>=(List.length try_oprs_comb)) with
		     | true -> 			 
			 let new_comb =  (match (is_routing_operation (List.hd opns)) with
					  | false -> try_oprs_comb 
					  | true  -> let oprs_for_rt_opns = List.map (fun opn -> (pi opn)) (List.hd opns).t_opn_predecessors in
					             List.filter (fun opr -> opr=@(List.hd try_oprs_comb)) oprs_for_rt_opns) in
			 (match (new_comb = []) with
			 | true -> tmp_comb,tmp_sp,tmp_esfs,tmp_delta,tmp_rel,tmp_ILambda
			 | false ->
			     debug_ps !yyyy debug_level ("\n start combainison "^(name_of_operation (List.hd opns)));
			     debug_ps !yyyy debug_level (List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) ("\n   try on operators  = ") new_comb); 
			     let opns_esfs    = (esfs_schedule_opns new_comb opns graph) in                
			     let opns_rel     = (compute_rel_with_com opns (ref (List.length new_comb)) random_media) in 
			     let _esfs,_delta = List.fold_left (fun (_esfs,_delta) (esfs,delta) -> if ((esfs+.delta)>(_esfs+._delta)) 
								then (esfs,delta) else (_esfs,_delta)) (min_float,min_float) opns_esfs in
			     let delta_exe_sum = List.fold_left (fun delta_tmp (esfs,delta) -> delta_tmp +. delta) 0. opns_esfs in			 
     			     let index = ref 0 in
			     let delta_com_sum = 
			       List.fold_left (fun delta_tmp opr ->
						 let opn = List.nth opns !index in
						 index := !index + 1;
						 delta_tmp +. (List.fold_left (fun tmp_sum recv -> match (is_communication recv) with 
									       | false -> tmp_sum 
									       | true  -> let send = (List.hd recv.t_opn_predecessors) in
										 tmp_sum +. (delta send.t_opn_class (pi send))) 0. opn.t_opn_predecessors)
					      ) 0. new_comb  in		    			     	     
			     update_old_reliability (List.hd opns);			     
			     let new_rel  =  !old_rel *.   opns_rel  in  			     
			     let new_len  =  max !old_len (_esfs +. _delta) in
			     let sum_Ci   = !deltas_sum +. delta_exe_sum  +. delta_com_sum in			     
			     let exact_ILambda = get_lambda new_rel sum_Ci in	    		   
			     let new_sp = (_esfs +. _delta) +. (lefe (List.hd opns))  in			     
			     deschedule_opns new_comb opns graph;
			     debug_ps !xxxx debug_level ((List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) "\n  schedule on  = " new_comb)^
							   " \n  schedule pressure =  "^(string_of_float new_sp)^"   L =  "^(string_of_float new_len)
							 ^"   R = "^(string_of_float new_rel)^"   /\\ = "^(string_of_float exact_ILambda)); 
			     (match ((exact_ILambda <= !obj_big_lambda)||(only_length)) with  
			      |true -> (match (((new_sp < tmp_sp)&&(not only_length))||((exact_ILambda <= tmp_ILambda)&&(only_length)))  with  
					| true  -> new_comb,new_sp,   _esfs,   _delta,opns_rel,exact_ILambda		         
					| false -> (match (((new_sp = tmp_sp)&&((List.length new_comb)<(List.length tmp_comb))&&(not only_length))||
     						           ((exact_ILambda <= tmp_ILambda)&&(only_length)))  with
						    | true  -> new_comb,new_sp,   _esfs,   _delta,opns_rel,exact_ILambda
						    | false -> tmp_comb,tmp_sp,tmp_esfs,tmp_delta,tmp_rel, tmp_ILambda))
 			      |false ->  tmp_comb,tmp_sp,tmp_esfs,tmp_delta,tmp_rel,tmp_ILambda))
			 | false -> tmp_comb,tmp_sp,tmp_esfs,tmp_delta,tmp_rel,tmp_ILambda) procs_comb ([],max_float,max_float,max_float,0.,max_float) in 
  (best_comb,best_sp,best_esfs,best_delta,best_rel,best_ILambda) 



(*************************************************************************************) 
(*** if failure rate not satisfied then re-execute without failure rate constarint ***)
(*************************************************************************************) 
let best_oprs_comb opns graph random_media = 
  debug_ps !yyyy debug_level ("\n=================="^(name_of_operation (List.hd opns))^"==================================");
  let best_comb, best_comp,best_esfs,best_delta,best_rel,best_ILambda = get_best_oprs_comb opns graph random_media false in
  let best_comb, best_comp,best_esfs,best_delta,best_rel,best_ILambda = (match best_comb with
						      | []  -> debug_ps !xxxx debug_level "\n----------------RE-EXECUTE------NO Lambda-------------->>>>\n";
							       get_best_oprs_comb opns graph random_media true   
						      |  _  -> best_comb, best_comp,best_esfs,best_delta,best_rel,best_ILambda) in  
  debug_ps !xxxx debug_level ((List.fold_left (fun s opr -> s^" "^(name_of_operator opr)) ((name_of_operation (List.hd opns))^"\n oprs bests = ") best_comb)
			  ^" \n  schedule pressure =  "^(string_of_float best_comp)^"   R = "^(string_of_float (best_rel))
			     ^"  /\\ =   "^(string_of_float  best_ILambda));
  (best_comb,best_comp,best_esfs,best_delta,best_rel,best_ILambda)
 

let find_best_candidate operations_operatoropt =
  let eefs_min = List.fold_left (fun tmp_eefs (opns,(oprs,sp,esfs,delta,rel,lambda)) -> min tmp_eefs (esfs +. delta)) max_float operations_operatoropt in
  let x = List.fold_left (fun (opns_opt,(oprs_opt,sp_opt,esfs_opt,delta_opt,rel_opt,lambda_opt)) (opns,(oprs,sp,esfs,delta,rel,lambda)) ->   
			    match ((((esfs < eefs_min) && (sp > sp_opt))&&(oprs<>[]))||(sp_opt==max_float)) with
			    (*match (((sp > sp_opt)&&(oprs<>[]))||(sp_opt==max_float)) with*)
			      | true   -> (opns,(oprs,sp,esfs,delta,rel,lambda))
			      | false  -> (opns_opt,(oprs_opt,sp_opt,esfs_opt,delta_opt,rel_opt,lambda_opt))
			 ) (List.hd operations_operatoropt) operations_operatoropt in
 x

 
(*******************************)
(*  RELIABILITY ADEQUATION     *)
(*******************************)
  let adequation  _obj_len _obj_big_lambda _theta _level_max fix_level _reliability_file_name graph =       
    let s = Hashtbl.fold  
      (fun _ opn s -> 
	 s^"\n"^(identifier_of_operation opn)^(string_of_argsvalues opn.t_opn_arguments_values))
      graph "Initial graph :" in
    debug_ps 3 debug_level s; 
    reps  := " ";    
    fault_architecture_init _obj_len _obj_big_lambda _theta _level_max graph;          
    let oprs = Application.application.app_operator_definitions in 
    let opns =  Hashtbl.fold (fun _ opn opns -> opn::opns) graph [] in             
    let initial_nb_opns = Hashtbl.length graph in
    (*** create routing operation *)
    let routing_opn = new_opn ["S"] (Calcul (Operation,"","S")) [] (precedence_ports ()) [] []   
      [(None,None)] None 0 0. true (Ihm ["S"]) None "" "" "" [InitSeq;LoopSeq;EndSeq] in   
    List.iter (function operator ->  (Hashtbl.add operator.oprdef_operation_durations ("","S") 0.0);	      ) oprs;  
    List.iter (fun opn -> add_routing_opn opn routing_opn graph;) opns;     
    let all_operators = Application.operators_list !archilib !archiname in 
    let all_media     = Application.media_list !archilib !archiname in 
    (*let random_media  = List.hd all_media in*)
    let nb_oprs = List.length all_operators in
    let nb_opns = ref 0 in
    (*let progress_box = Progress_box.create total_adequation in*)
    (***** initialize data for reliability  *****)
    initialize_failure_rates_table _reliability_file_name;
    mean_lambda_opr := (List.fold_left (fun lambda_tmp opr -> lambda_tmp +. (Hashtbl.find failures_rate opr)) 
			 0. all_operators) /. (float_of_int (List.length all_operators));
    mean_lambda_mda := (List.fold_left (fun lambda_tmp opr -> lambda_tmp +. (Hashtbl.find failures_rate opr)) 
			 0. all_media) /. (float_of_int (List.length all_media));    

    let random_media = List.sort (fun mda1 mda2 -> compare (Hashtbl.find failures_rate mda2) (Hashtbl.find failures_rate mda1)) all_media in

    let combinaisons = (int_of_float ((2. ** (float_of_int nb_oprs))) - 1) in
    let comb_i = Array.create nb_oprs 0 in

    for i = 1 to combinaisons do
      if (decimal2binary (ref i) comb_i) then Hashtbl.add procs_comb  i (get_combinaison_procs comb_i all_operators nb_oprs);      
    done;
    (**** END initialize data for reliability ****)       
    let schedulables_opns = (hashtbl_filter (fun o -> (is_schedulable o) && (not (is_constant o)) ) graph) in 
    let schedulables = ref (List.map (fun opn -> opn::(create_copies opn graph !level_max)) schedulables_opns) in
    try  
      let step = ref 1 in
      while !schedulables <> [] do
        debug_ps !xxxx debug_level  "\n ========================================================";
   	debug_ps !xxxx debug_level (ps_schedulable schedulables); 
	let exist = try Some (List.find (fun opns -> is_routing_operation (List.hd opns)) !schedulables)
	            with _  -> None  in
	(* Calcul of the optimal operator for each operation *)
	let operations_operatorsopts =  List.map (function opns -> 
						    opns,(match (is_routing_operation (List.hd opns)) with
							  | false -> 
							      if (exist=None) 
						              then (best_oprs_comb opns graph random_media)
                                                              else ([],max_float,max_float,max_float,0.,max_float);
							  | true  -> (best_oprs_comb opns graph random_media))) !schedulables in
	let opns_opts,(oprs_opts,comp_opt,esfs_opt,delta_opt,rel_opt,lambda_opt) = find_best_candidate operations_operatorsopts in 

	if (oprs_opts=[]) then begin  schedule_failure := true; raise (Failure "Constraint not respected !!!!!!");end; 
	(* schedule all routing operation and copies of optimal operation *)  
        let rec get_couple opns oprs =
          match (opns, oprs) with
          |  (_, []) -> [] 
          |  (opn::sopns, opr::soprs) -> ((opn,opr),name_of_operator opr):: (get_couple sopns soprs) 
          |  (_, _) -> invalid_arg "get_couple" in  
        let opts2 = (get_couple opns_opts oprs_opts) in
        let opts,oprs_id = List.split opts2 in 

	debug_ps 10 debug_level (List.fold_left (fun s opn -> s^"   "^(name_of_operation opn)) ("\n \n schedule     =   ")  opns_opts);  
        debug_ps 10 debug_level (List.fold_left (fun s opr -> s^"   "^(name_of_operator opr))  (" on oprs = ")  oprs_opts);
	(*ps"------------------------------old dpds";ps_dpds (List.hd opns_opts); *)
	(match (is_routing_operation (List.hd opns_opts)) with
	|  false ->
	     if ((List.length oprs_opts)=1)
	     then  List.iter (fun dpd -> 			    
				if (is_routing_operation dpd.t_dpd_dopn) 
				then  remove_operation graph dpd.t_dpd_dopn;
			     ) (List.hd opns_opts).t_opn_dependences_successors
	     else  List.iter (fun opn_opt -> 
				List.iter (fun dpd -> 			    
					     if (not (is_routing_operation dpd.t_dpd_dopn)) 
					     then  dependence_remove dpd;
					  ) opn_opt.t_opn_dependences_successors;
			     ) opns_opts;
             let rate = (List.length oprs_opts) in

             debug_ps 0 debug_level ("opn "^(string_of_int !step)^" : rep = "^(string_of_int rate));
             step := !step + 1;  

	     nb_opns := !nb_opns + rate;          
	     replication_process := (List.length oprs_opts)::!replication_process;
	     
	     List.iter (fun opr ->
			  let index,rate_p,nb_opn = (Hashtbl.find replication_process_proc (name_of_operator opr)) in
			  (Hashtbl.replace replication_process_proc (name_of_operator opr) (index,(rate_p+rate),nb_opn+1) )) oprs_opts;
	     
	     (* Update the list of opns_opts copies *) 
	     update_opns_copies opns_opts (List.length oprs_opts) graph; 
	     (*print_string (" ; "^(string_of_float lambda_opt));*)
	| true -> (););

        schedule_opts opts graph;

	let opns_opts = (List.map (fun (opn,opr) -> 
				     let delta_comm_sum = 
				       List.fold_left (fun tmp_sum recv -> match (is_communication recv) with 
						       | false -> tmp_sum 
						       | true  -> let send = (List.hd recv.t_opn_predecessors) in
							 tmp_sum +. (delta send.t_opn_class (pi send))) 0. opn.t_opn_predecessors in
				     deltas_sum := !deltas_sum +. delta_comm_sum  +. (delta opn.t_opn_class opr);
				     opn)  opts) in

	debug_ps 0 debug_level ((List.fold_left (fun s opn -> s^" "^(name_of_operation opn)) ("\n end schedule  = ")  [(List.hd opns_opts)])
				   ^(List.fold_left (fun s opr -> s^" "^(name_of_operator opr))  (" on ")  oprs_opts)
				   ^"            R = "^(string_of_float (!old_rel *. rel_opt))
				   ^"    /\\ = "^(string_of_float lambda_opt (*-. log(!old_rel *. rel_opt)/. !deltas_sum*)));
	debug_ps 10 debug_level ((name_of_operation (List.hd opns_opts))^" = "^(string_of_float lambda_opt)); 


	if not (is_routing_operation (List.hd opns_opts)) 
	then begin 
	  reps := !reps^" ;  "^(string_of_int (List.length (opns_opts)));
	  scheduled_opns := ((identifier_of_operation (List.hd opns_opts)),oprs_id):: !scheduled_opns;
	end;

	update_old_reliability (List.hd opns_opts); 

	Hashtbl.add opns_rel_table (get_original (List.hd opns_opts)) (opns_opts, rel_opt);	

	old_rel := !old_rel *. rel_opt;	

	(* Update the list of schedulables operations *) 
	update_schedulables graph (List.hd opns_opts) schedulables; 
        (*ps"------------------------------new dpds";ps_dpds (List.hd opns_opts);*)

	(*Progress_box.tick progress_box;*)                       
      done; 

      rate_replication := (float_of_int !nb_opns) /. (float_of_int initial_nb_opns);
      schedule_length := Hashtbl.fold (fun  _  opn eefs_tmp -> (max eefs_tmp (eefs opn))) graph min_float;
      debug_ps 10 debug_level ("\n ------------------------------------------ \n");
      schedule_reliability := Hashtbl.fold (fun  _  (opns,opns_rel) rel_tmp ->  					     
					      let new_rel =  match (is_routing_operation (List.hd opns)) with
						| true  -> 1.
						| false -> (re_compute_rel_with_com opns (ref (List.length opns))) in
					      debug_ps 10 debug_level ((name_of_operation (List.hd opns))^"    =     "
								      ^(string_of_float opns_rel)^"  <-------------->  "^(string_of_float new_rel)); 
					      rel_tmp *. new_rel
					   ) opns_rel_table 1.;


      scheduled_opns := List.rev !scheduled_opns;

     schedule_big_lambda := -. log(!schedule_reliability) /. (*!schedule_length*) !deltas_sum;

     (*if (!schedule_big_lambda > !obj_big_lambda) then begin  schedule_failure := true; raise (Failure "Constraint not respected !!!!!!");end;*)

     debug_ps 10 debug_level ("\n             new Rel =   "^(string_of_float !schedule_reliability));   
     debug_ps 10 debug_level ("\n              sum =   "^(string_of_float !deltas_sum)); 
     debug_ps 10 debug_level ("\n              new lam =   "^(string_of_float !schedule_big_lambda)); 
     debug_ps 10 debug_level ("\n              old lam =   "^(string_of_float (-. log(!old_rel)/. !deltas_sum))); 

     debug_ps 10 debug_level ("\n__________________________________________\n         "^ !reps);



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
    with exn ->  
      debug_ps 10 debug_level ("\n__________________________________________\n            "^ !reps);      
      rate_replication := -1.0;
      schedule_length := -1.0;
      schedule_reliability := -1.0;
      schedule_big_lambda := -1.0; graph,schedules
      
      (*Progress_box.close progress_box;*) (*raise exn*)
      
      
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
  (*let succs = successors opn in*)
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







 
