(************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            HamoudiKalla                               *)
(*                                                                       *)
(*                     Projet POP ART, INRIA Rohones-alpes               *)
(*************************************************************************)


(* WARNING : be cautious when changing this module. *)

(* There are very tough performance requirements, especially when
   dealing with communications. *)

(* This module contains the functions of the adequation heuristic. The
   functor enables us to choose the progress box, which is used to display
   the progress of the adequation. That way we can for instance have a
   silent progress box for command line mode.*)

module Make (Progress_box : Progress_box.PROGRESS_BOX_SIG) = 
  struct 
    open Types 
    open Adequationtypes 
    open Adequation_core

    type t_opr_src = {
      mutable s_tilde : float; 
      mutable f_tilde : float;   
      mutable s_tilde_old : float; 
      mutable f_tilde_old : float;   
      mutable lambda : float;
      mutable intervals : float list;
    }

   	
    let opr_src = {s_tilde=max_float; f_tilde=min_float; s_tilde_old=0.0; f_tilde_old=0.0; lambda =0.0; intervals=[]} 
    let incremental_overlapping_C = ref 0.   
 
    let schedule_length = ref 0.
    let schedule_reliability = ref 0.

    let normalized_value_for_reliability_weight = ref 1.
      
    let operations_median = Hashtbl.create 100
    (*let articulation_points = Hashtbl.create 10
    let path_table = Hashtbl.create 30*)          
    let debug_level = 0

  (*  print paths	
    let psr r =  
      let name_of_og (o,g) = 
	(name_of_operator o)^(match g with | Some g -> "."^g.gte_name | _ -> "") in
      match r with
      | [] -> ()
      | hd::tl -> 
	  ps (List.fold_left
		(function s -> function og -> s^"->"^(name_of_og og))
		(name_of_og hd) tl)
 
   Calculate the list of shortest paths between o1 and o2 
    let paths o1 o2 =  
      ps ("Search path from "^(name_of_operator o1)^" to "^(name_of_operator o2));  
      let no_crossing r1 r2 = 
	let r1 = remove_copies (List.map fst r1)
	and r2 = remove_copies (List.map fst r2) in
	(intersection r1 r2)=[] in
      let archilib,archiname = Application.archi_main_get () in
      let links o = Architecture.links archilib archiname o in
      let rec r o1 o2 path =
	match Hashtbl.mem path_table (o1,o2) with
	| true -> (* this path already calculated *)
	    let subpaths = Hashtbl.find path_table (o1,o2) in
	    List.filter (no_crossing path) subpaths
	| false ->
	    let links = List.filter (function (og1,og2) -> no_crossing [og2] path) (links o1) in 
            (* links : list of links from o1 not in ll *)
	    let subpaths = List.map (function (og1,((opr2,_) as _og2)) as lnk -> lnk,(r opr2 o2 (og1::path))) links in
	    let subpaths = List.concat (List.map (function (og1,og2),rlist -> List.map (function r -> og1::og2::r) rlist) subpaths) in
            (* subpaths : list of paths from o1 to o2 *)
	    (*let subpathsmin = list_min_elements List.length subpaths 0 in*)
	    let subpathsmin = subpaths in
            (* list of shortest paths from o1 to o2 *)
	    List.map remove_copies subpathsmin in
      let paths = (match Hashtbl.mem path_table (o1,o2) with
		    | true  -> (Hashtbl.find path_table (o1,o2))
		    | false -> let paths = r o1 o2 [] in
		      Hashtbl.add path_table (o1,o2) paths;
		      Hashtbl.add path_table (o2,o1) (List.map List.rev paths);
		      paths) in
      let path = try List.hd 
	(list_min_elements List.length paths 0) with  _ -> raise (Failure ("No paths from "^(name_of_operator o1)^" to "^(name_of_operator o2)^" !!")) in  
      ps ("Find path for "^(name_of_operator o1)^" -> "^(name_of_operator o2));      
      print_string "Found path is : ";List.iter psr [path]; 
      path  

 parcourir graphe 
    let rec compute_articulation_points opr oprs =
      match (Hashtbl.mem articulation_points opr) with 
      | true  ->  ()(*ps((name_of_operator opr)^" exist");*) 
      | false ->  
	  let links =  Architecture.links  !archilib  !archiname  opr in 
          let next_oprs = List.map (fun ((_,_),(opr2,_)) -> opr2) links in                      
	  ps (List.fold_left (fun s opr2 -> (s^(name_of_operator opr2)^", ")) ((name_of_operator opr)^"..........: ") next_oprs);  
	  Hashtbl.replace articulation_points opr (true,false,-1,-1); 
	  List.iter (fun opr2 -> 		  
		       compute_articulation_points opr2 (opr::oprs); 
		    ) next_oprs  
   
 let t = ref 0 
 let nbponts = ref 0

 compute articulation points 
    let rec compute_articulation_points opr_w =
      let (mark_w,arti_w,debut_w,bas_w,pere_w) = Hashtbl.find articulation_points opr_w in
      Hashtbl.replace articulation_points opr_w  (mark_w,arti_w,!t,!t,pere_w);
      t := !t + 1;
      let links =  Architecture.links  !archilib  !archiname  opr_w in
      let next_mdas = List.map (fun ((_,_),(mda_u,_)) -> mda_u) links in                     
      ps (List.fold_left (fun s mda_u -> (s^(name_of_operator mda_u)^", ")) ((name_of_operator opr_w)^":------->   ") next_mdas);
      let next_oprs = List.map (fun mda -> 
				  let links =  Architecture.links  !archilib  !archiname  mda in
				  let next_oprs = List.filter (fun opr -> opr<>@opr_w) (List.map (fun ((_,_),(opr_u,_)) -> opr_u) links) in  
				    (List.hd next_oprs)
			       ) next_mdas in
      (*ps (List.fold_left (fun s opr_u -> (s^(name_of_operator opr_u)^", ")) ((name_of_operator opr_w)^":------->   ") next_oprs);*)
      List.iter (fun opr_u -> 		 
		   let (mark_u,arti_u,debut_u,bas_u,pere_u) =  Hashtbl.find articulation_points opr_u in
		   match debut_u with
		   |  -1  ->  Hashtbl.replace articulation_points opr_u (mark_u,arti_u,debut_u,bas_u,pere_w);
			      compute_articulation_points opr_u;
			      let (mark_u,arti_u,debut_u,bas_u,pere_u) =  Hashtbl.find articulation_points opr_u in
			      let (mark_w,arti_w,debut_w,bas_w,pere_w) =  Hashtbl.find articulation_points opr_w in			      
			      (match bas_w > bas_u with
			      |  true  ->  Hashtbl.replace articulation_points opr_w (mark_w,arti_w,debut_w,bas_u,pere_w);
			      |  false -> ());
			      (match bas_u=debut_u with
			      |  true  ->  ps("points  =  "^(name_of_operator opr_w)^" -- "^(name_of_operator opr_u)); nbponts := !nbponts + 1;
			      |  false -> ());				   
		   |  _   -> let (mark_w,arti_w,debut_w,bas_w,pere_w) =  Hashtbl.find articulation_points opr_w in
		             (match ((pere_w<>@opr_u)&&(debut_u<bas_w)) with
			      |  true  ->  Hashtbl.replace articulation_points opr_w (mark_w,arti_w,debut_w,bas_u,pere_w);
			      |  false -> ());
		) next_oprs     

(* compute reliability of all paths between opr and operator_source *)

    let compute_paths_reliability opr opr_src =
      let paths = (paths opr opr_src) in      
      ps ("............Find path for "^(name_of_operator opr)^" -> "^(name_of_operator opr_src));      
      print_string "Found path is : ";List.iter psr [paths];     
      10.
      *)

 
       (* Place in graph the communication corresponding to dependence dpd
	 after instant tcond with status status *)
    let place_communication graph status tcond dpd =
      (* Place the communication of operation_src.port_src between
	 operator_src and operator_dst after instant t0 *)
      let place_com (data,(operator_src,operation_src,port_src,t0))
	  operation_dst port_dst operator_dst dpdclass cond
	  place status =

	let rec place_com_aux sopn sprt eefs place status opr_src = 
	  debug_ps 3 debug_level ("from "^(name_of_operator opr_src)^" to "^
	     (name_of_operator operator_dst)^" placing "^
	     (name_of_operation sopn)); 
	  match (name_of_operator opr_src) = (name_of_operator operator_dst) with
	  | true -> sopn,sprt,eefs
	  | false -> 
    	      let lnks = shortest_links opr_src operator_dst in
	       (*ps (List.fold_left (fun s ((_,_),(opr,_)) -> s^
		 " "^(name_of_operator opr)) "Possible next oprs are " lnks);*) 
	      let  best_lnk =
		find_best_link lnks [] max_float sopn sprt eefs data graph cond
		  status dpdclass operator_dst false in
	      (* This time, place it if place is true *)
	      let (opn,prt,eefs),next_opr =
		debug_ps 3 debug_level ("...eefs "^(string_of_float eefs));
		place_link best_lnk place sopn sprt eefs data graph cond
		  status dpdclass false in
	      (*  	    ps ("next opr "^name_of_operator next_opr); *)
	      place_com_aux opn prt eefs place status next_opr in 
	
	let eefs = max t0 tcond in
	debug_ps 3 debug_level ("                            "^(string_of_float (eefs)));
	(*       ps ("placing com from "^(name_of_operator operator_src)); *)
	let opnpred,prtpred,eefs =
	  place_com_aux operation_src port_src eefs place status operator_src in
	debug_ps 3 debug_level ("                            "^(string_of_float (eefs)));
	(match place with
	  (* Add dependence between last opn created and dopn (operation
	  which required the communication) *)
	| true ->
	    Adequationtypes.dependence_add opnpred prtpred operation_dst
	      port_dst dpdclass cond status
	| false -> ());
	eefs in
      
      let sopn,sprt,dopn,dprt,dpdclass,cond =
	dpd.t_dpd_sopn,dpd.t_dpd_sprt,
	dpd.t_dpd_dopn,dpd.t_dpd_dprt,
	dpd.t_dpd_class,dpd.t_dpd_condition in
      
      debug_ps 3 debug_level ("place_communication........."^
			      (name_of_data (sopn,sprt))^" -> "^
			      (name_of_data (dopn,dprt)));

      let data,operator_dst = (sopn,sprt),(pi dopn) in
      
      (* Calculate list of operators containing data *)

      let operators_src_list = Hashtbl.find datas data in

      let operators_src_list = List.filter (function (_,opn,_,_) ->
	match conditioned_operation opn with
	| false -> true
	| true ->
	    List.for_all
	      (fun c -> List.mem c dpd.t_dpd_condition)
	      opn.t_opn_condition) operators_src_list in
      let f (o,_,_,_) = match o with
      | Media mda ->
	  mda.mdaref_links2.((operator_of_operator_class operator_dst).oprref_id)
      | Operator opr ->
	  opr.oprref_links2.((operator_of_operator_class operator_dst).oprref_id) in
      let operators_src_list = list_min_elements f operators_src_list 0 in
      (* operators_src_list : list of operators containing data and
      nearest of operator_dst *)      
      let s =
	List.fold_left
	  (fun s (operator_src,operation_src,port_src,t0) ->
	    s^(name_of_operator operator_src)^" ")
	  (" (looking for cond"^(string_of_condlist dpd.t_dpd_condition)^") ")
	  operators_src_list in
      debug_ps 4 debug_level ((name_of_data data)^" on "^s);

      (* Not done anymore : Place the communication between each
      operator and the destination to determine the best path *)
      let best_operator_src =
	List.hd operators_src_list in 

      (* Place the communication *)
      ignore(place_com (data,best_operator_src) dopn dprt operator_dst
	       dpdclass cond true status)

(* Builds operation predecessors coms *)
    let make_coms operation graph status =
      let make_one_com tcond dpd =
	debug_ps 3 debug_level ("making com "^(string_of_dpd dpd)^"   : ");
	match (is_memory dpd.t_dpd_sopn) || (is_constant dpd.t_dpd_sopn) ||
	(pi dpd.t_dpd_sopn =@ pi operation) with
	| true -> ()
	| false -> place_communication graph status tcond dpd;
	    match status with
	    | true -> Adequationtypes.dependence_remove dpd
	    | false -> () in
      
      let dpds_data,dpds_condition =
	List.partition
	  (fun {t_dpd_class=(edgetype,_)} -> edgetype=Data)
	  operation.t_opn_dependences_predecessors in
      debug_ps 3 debug_level
	((name_of_operation operation)^" coms are : "^
	 (List.fold_left
	    (fun s dpd -> string_of_dpd dpd)
	    "" dpds_data));
      let tc = make_cond_dpds (make_one_com) dpds_condition in
      List.iter (make_one_com tc) dpds_data

(* Build memory successor coms *)
    let make_memory_succ_coms graph opn_eefs operation =
      let dpds_data,dpds_condition =
	List.partition
	  (fun {t_dpd_class=(edgetype,_)} -> edgetype=Data)
	  operation.t_opn_dependences_successors in
      ignore (make_cond_dpds (fun tc dpd ->
	match (pi operation =@ pi dpd.t_dpd_dopn) with
	| true -> ()
	| false -> place_communication graph true tc dpd) dpds_condition);
      List.iter
	(fun dpd ->
	  match (pi operation =@ pi dpd.t_dpd_dopn) with
	  | true -> ()
	  | false -> Adequationtypes.dependence_remove dpd)
	dpds_condition;
      List.iter
	(fun dpd ->
	  match (pi operation =@ pi dpd.t_dpd_dopn)  with
	  | true -> ()
	  | false -> place_communication graph true opn_eefs dpd;
	      Adequationtypes.dependence_remove dpd)
	dpds_data

(* Returns operation esfs on operator on which it has been scheduled
(taking communication time into account).*)
    let esfs operation =
      let operator = pi operation in
      (*let delta = delta operation.t_opn_class operator in*)
      let tdpds =
	list_max_value dpd_EEFS operation.t_opn_dependences_predecessors 0. in
      debug_ps 1 debug_level ("Dpds esfs: "^(string_of_float tdpds));
      let tcond =
	esfs_cond operation operator tdpds operation.t_opn_condition in
      tcond

(* Selects the best candidate (considering schedule pressure) among
operations_operatoropt *)
    let find_best_candidate operations_operatoropt =
      List.fold_left
	(fun (opn_max,(opr_max,(esfs_max,_Si_max,_Fi_max,_Ci_max,_DL_max))) (opn,(opr,(esfs,_Si,_Fi,_Ci,_DL))) -> 
	   match (_DL>_DL_max) with
	   | true  -> (opn,(opr,(esfs,_Si,_Fi,_Ci,_DL)))
	   | false -> (opn_max,(opr_max,(esfs_max,_Si_max,_Fi_max,_Ci_max,_DL_max)))) 
	(List.hd operations_operatoropt) operations_operatoropt 


 (*  create new opn source to compute fi for dogan cost function *)
  let create_source_operation operation = 
    let src_opn = new_opn ["Source"] (Calcul (Operation,"","Source")) [] (precedence_ports ()) [] [] 
			       [(None,None)] None 0 0. true (Ihm ["Source"]) None 
			       "" "" "" [InitSeq;LoopSeq;EndSeq] in 
    let in_ports = List.filter (fun p -> (p.t_prt_dir=Port.Out)) operation.t_opn_ports in
    src_opn.t_opn_ports <- 
      List.fold_left (fun ports p -> match (p.t_prt_name<>"precedence_out") with
		| false  -> ports		    
		| true -> {t_prt_name=p.t_prt_name;t_prt_dir=Port.In;t_prt_typename=p.t_prt_typename; 
			   t_prt_class=p.t_prt_class;t_prt_dim=p.t_prt_dim;t_prt_order=p.t_prt_order}::ports) [] in_ports;
    List.iter (fun dprt ->
		 let sprt = (List.find (function prt -> prt.t_prt_name=dprt.t_prt_name) operation.t_opn_ports) in
		   dependence_add operation sprt src_opn dprt (Data,Strong_Precedence_Data) [] true
	      ) src_opn.t_opn_ports;
      src_opn


  let lambda_path opr1 opr2 = 
    ((Hashtbl.find  failures_rate opr1)+.(Hashtbl.find Adequationtypes.failures_rate opr2))


(* Compute Reliability term 1 for the cost function *)
  let reliability_C1 operation operator src_opr graph = 
    let src_opn = create_source_operation operation in
    operation_add graph src_opn;
    schedule operation operator false 0. 0.;
    create_gaps_backups ();
    make_coms operation graph false; 
    debug_ps 10 debug_level 
      ("try ("^(name_of_operator operator)^","^(name_of_operation operation)^") ---> ("^(name_of_operator src_opr)^","^(name_of_operation src_opn)^")"); 
    let _Si = esfs operation in      
    let _Fi_opn = _Si +. (delta operation.t_opn_class operator) in
    	debug_ps 10 debug_level ((name_of_operator  operator)^":  S("^(name_of_operation operation)^") = "^(string_of_float _Si)
      ^":  F("^(name_of_operation operation)^") = "^(string_of_float _Fi_opn));
    (* Data becoming available on operator *)
    List.iter
      (fun prt ->
	 match (prt.t_prt_dir,prt.t_prt_class) with
	 | Port.Out,_ -> debug_ps 10 debug_level("Adding "^(name_of_data (operation,prt))^" on "^ (name_of_operator operator)^")");
	     update_datas operator (operation,prt) operation prt _Fi_opn
	 | _,_ -> ())	operation.t_opn_ports;       
    schedule src_opn src_opr false 0. 0.;
    (*create_gaps_backups ();*)
    make_coms src_opn graph false;
    let _Fi = max (esfs src_opn) _Fi_opn in   
    (*ps ((name_of_operator  src_opr)^":  S(source) = "^(string_of_float _S));*)
    (* remove source available data on operator *)
    List.iter
      (fun prt ->
	 match (prt.t_prt_dir,prt.t_prt_class) with
	   | Port.Out,_ -> debug_ps 10 debug_level ("removing "^(name_of_data (operation,prt))^" on "^ (name_of_operator operator)^")"); 
               Hashtbl.remove datas (operation,prt) 
	   | _,_ -> ())	operation.t_opn_ports; 
    (*ps (List.fold_left (fun s dpd -> s^", "^(name_of_operation dpd.t_dpd_sopn)) "preds : " src_opn.t_opn_dependences_predecessors); *)      
    delete_coms src_opn graph;
    operation_remove graph src_opn;
    (*restore_gaps ();*)
    deschedule src_opn src_opr false; 
    delete_coms operation graph;         
    restore_gaps ();
    deschedule operation operator false;    
    let failure_of_network =  (lambda_path operator src_opr) in 
    debug_ps 10 debug_level ((name_of_operation operation)^"-"^(name_of_operator operator)^" :: "^(string_of_float  failure_of_network)
			    ^" * (("^(string_of_float _Fi)^" - "^(string_of_float _Si)^") * "^(string_of_float _Fi)^" =  "
			    ^(string_of_float (failure_of_network *. ((_Fi -. _Si) *. _Fi))));
    _Fi_opn, _Fi, failure_of_network *. ((_Fi -. _Si) *. _Fi)

(* Compute Reliability term 2 for the cost function *)
  let reliability_C2 operation operator src_opr graph =  
    let src_opn = create_source_operation operation in
    operation_add graph src_opn;
    schedule operation operator false 0. 0.;
    create_gaps_backups ();
    make_coms operation graph false; 
    debug_ps 10 debug_level 
      ("try ("^(name_of_operator operator)^","^(name_of_operation operation)^") ---> ("^(name_of_operator src_opr)^","^(name_of_operation src_opn)^")"); 
    let _Si = esfs operation in      
    let _Fi_opn = _Si +. (delta operation.t_opn_class operator) in
    	debug_ps 10 debug_level ((name_of_operator  operator)^":  S("^(name_of_operation operation)^") = "^(string_of_float _Si)
      ^":  F("^(name_of_operation operation)^") = "^(string_of_float _Fi_opn));
    (* Data becoming available on operator *)
    List.iter
      (fun prt ->
	 match (prt.t_prt_dir,prt.t_prt_class) with
	 | Port.Out,_ -> debug_ps 10 debug_level("Adding "^(name_of_data (operation,prt))^" on "^ (name_of_operator operator)^")");
	     update_datas operator (operation,prt) operation prt _Fi_opn
	 | _,_ -> ())	operation.t_opn_ports;       
    schedule src_opn src_opr false 0. 0.;
    (*create_gaps_backups ();*)
    make_coms src_opn graph false;
    let _Fi = max (esfs src_opn) _Fi_opn in   
    (*ps ((name_of_operator  src_opr)^":  S(source) = "^(string_of_float _S));*)
    (* remove source available data on operator *)
    List.iter
      (fun prt ->
	 match (prt.t_prt_dir,prt.t_prt_class) with
	   | Port.Out,_ -> debug_ps 10 debug_level ("removing "^(name_of_data (operation,prt))^" on "^ (name_of_operator operator)^")"); 
               Hashtbl.remove datas (operation,prt) 
	   | _,_ -> ())	operation.t_opn_ports; 
    (*ps (List.fold_left (fun s dpd -> s^", "^(name_of_operation dpd.t_dpd_sopn)) "preds : " src_opn.t_opn_dependences_predecessors); *)      
    delete_coms src_opn graph;
    operation_remove graph src_opn;
    (*restore_gaps ();*)
    deschedule src_opn src_opr false; 
    delete_coms operation graph;         
    restore_gaps ();
    deschedule operation operator false;    
    let failure_of_network =  (lambda_path operator src_opr) in 
    (*ps (string_of_float  failure_of_network);*)
    let normalized_sums = ((_Fi -.  _Fi_opn) +. (delta_median operations_median operation))/. !normalized_value_for_reliability_weight in
    normalized_value_for_reliability_weight := (max normalized_sums !normalized_value_for_reliability_weight); 
    let reliability_weight  =  (normalized_sums +. 1.)  in
    reliability_weight *. (1.-.(failure_of_network*.(_Fi -. _Si))) *. (_Fi -. _Si) 

(* Compute Reliability term 3 for the cost function *)
  let reliability_C3 operation operator src_opr graph =  max_float
      

(* Returns the optimal esfs, schedule pressure and operator for
operation. Fails if no operator is able to execute this operation *)
    let dynamic_level operation operators operator_source  with_reliability graph =
      let opnlib,opnname = deflibname operation in
      let (opr_opt,esfs_opt,_Si_opt,_Fi_opt,_Ci_opt,_DL_opt) = 
	List.fold_left
	  (fun ((opr_m,esfs_m,_Si_m,_Fi_m,_Ci_m,_DL_m) as uple) operator -> 
  	    debug_ps 2 debug_level ("Trying "^(identifier_of_operation operation)^
				    " on "^(name_of_operator operator));
	    match Architecture.able_to_execute opnlib opnname (operator_of_operator_class operator) with
	    | true -> 
		(*ps "\n";*)
		schedule operation operator false 0. 0.;
		create_gaps_backups ();
 		make_coms operation graph false;
                let _SL = (lefe_median operations_median operation) in
		let max_term = esfs operation in
		let delta_median = delta_median operations_median operation in
		let delta = delta operation.t_opn_class operator in 
		let _DL_no_Reliability = _SL -. max_term +. delta_median -. delta in
		debug_ps 10 debug_level ("(SL - S)+(delta_median-delta))"^" = "
					^(string_of_float _SL)^" - "^(string_of_float max_term)^"  +  "
					^(string_of_float delta_median)^" - "^((string_of_float delta))^" = "^((string_of_float _DL_no_Reliability)));
		delete_coms operation graph;         
		restore_gaps ();
		deschedule operation operator false;                
		debug_ps 10 debug_level ("---------- S("^(name_of_operation operation)^","^(name_of_operator operator)^") = "^(string_of_float max_term));
                let _Si_new,_Fi_new,_DL_new,_Ci_new  = match  with_reliability with 
		  | 1  -> let _Si,_Fi,reliability_tmp = (reliability_C1 operation operator operator_source graph) in
		              _Si,_Fi,(_DL_no_Reliability -. reliability_tmp),reliability_tmp
		  | 2  -> let reliability_tmp = (reliability_C2 operation operator operator_source graph) in
		              0.0,0.0,(_DL_no_Reliability -. reliability_tmp),reliability_tmp
		  | 3  -> let reliability_tmp = (reliability_C3 operation operator operator_source graph) in
		              0.0,0.0,(_DL_no_Reliability -. reliability_tmp),reliability_tmp
		  | _  ->     0.0,0.0,_DL_no_Reliability,-77777777.0   in		 
		debug_ps 10 debug_level ("DL                   : "^(string_of_float _DL_no_Reliability));
                debug_ps 10 debug_level ("DL with reliability  : "^(string_of_float _DL_new));
		debug_ps 10 debug_level ("DL("^(name_of_operation operation)^","^(name_of_operator operator)^") = "^(string_of_float _DL_new));
		(match _DL_new > _DL_m with
		 | true  -> (operator,max_term,_Si_new,_Fi_new,_Ci_new,_DL_new)
		 | false -> uple)
	    | false -> uple)
	  (List.hd operators,max_float,max_float,max_float,max_float,-.max_float) operators in
      match esfs_opt = max_float with
      | true ->
	  failwith ("No operator able to execute :\nOperation : "^
		    (name_of_operation operation)^"\nDefinition : "^
		    opnlib^"/"^opnname^
		    "\n\nMaybe you forgot to define a duration for this definition....\n")
      | false ->
	  (*let _DL = (lefe operation) +. esfs_opt +. (delta operation.t_opn_class opr_opt) in*)
	  (*ps ("For operation "^(name_of_operation operation)^" in operator "^(name_of_operator opr_opt)^
		" esfs "^(string_of_float esfs_opt)^" DL "^(string_of_float _DL)); *)
	  (opr_opt,(esfs_opt,_Si_opt,_Fi_opt,_Ci_opt,_DL_opt))



(* Schedules constants on all operators on which they will be needed
(possible duplications) *)
    let place_constant graph progress_box =
      let rec place opn dpds = match dpds with
      | [] -> Adequationtypes.operation_remove graph opn
      | hd::tl ->
	  let opr = pi hd.t_dpd_dopn in
	  let dpds_opr_list,dpds_other_opr_list =
	    List.partition (function {t_dpd_dopn=dopn} -> pi dopn =@ opr) dpds in
	  let newpath =
	    (parentpath_of_operation opn)@
	    [(name_of_operation opn)^"_"^(name_of_operator opr)] in
	  let newopn =
	    {opn with
	     t_opn_path = newpath;
	     t_opn_dependences_predecessors = [];
	     t_opn_dependences_successors = [];
	     t_opn_operator = Some opr;
	     t_opn_esfs = 0.;
	     t_opn_status = true} in
	  Hashtbl.add graph (identifier_of_operation newopn) newopn;
	  List.iter
	    (fun prt ->
	      match (prt.t_prt_dir,prt.t_prt_class)  with
	      |	Port.Out, Port.Data_Port | Port.Out, Port.Init_Memory_Port ->
		  update_datas opr (newopn,prt) newopn prt 0.
	  |	_,_ -> ())
	    newopn.t_opn_ports;
	  schedule newopn opr true 0. 0.;
	  List.iter (fun dpd ->
	    let prt =
	      List.find
		(fun {t_prt_name=prtname;t_prt_dir=dir} ->
		  prtname=dpd.t_dpd_sprt.t_prt_name && dir=Port.Out)
		newopn.t_opn_ports in
	    Adequationtypes.dependence_add newopn prt dpd.t_dpd_dopn
	      dpd.t_dpd_dprt dpd.t_dpd_class dpd.t_dpd_condition
	      dpd.t_dpd_status)
	    dpds_opr_list;
	  let opns =
	    hashtbl_filter
	      (fun o ->
		(not (is_constant o)) &&
		((execution_operator_of_operation o) =@@ 
		 (operator_of_operator_class opr)))
	      graph in
	  List.iter
	    (fun o ->
	      o.t_opn_condition <- List.map
		  (function (v,vl) ->
		    (match v with
		    | Some (opncond,prtcond) ->
			(match opncond=$opn with
			| true -> (Some (newopn,prtcond))
			| false -> v)
		    | None -> v),vl)
		  o.t_opn_condition)
	    opns;
	  place opn dpds_other_opr_list in
      let constants = List.filter is_constant (list_of_hashtbl graph) in
      List.iter
	(fun opn ->
	  place opn opn.t_opn_dependences_successors;
	  (*Progress_box.tick progress_box*)())
	constants

(* initializes adequation  *)
let  initialize_adequation file_reliability  graph =
  adequation_order := 0;
  schedule_reliability := 0.;
  architecture_init graph;
  Hashtbl.clear lefe_table;
  Hashtbl.clear lefe_table_median;
  Hashtbl.clear operations_median; 
  (*Hashtbl.clear articulation_points;
  initializes paths between operators 
  Hashtbl.clear path_table; 
  let oprlist = List.map (Architecture.operator_reference !archilib !archiname )  (Architecture.operators_list !archilib !archiname) in
  let mdalist = List.map (Architecture.media_reference !archilib !archiname) (Architecture.media_list !archilib !archiname) in
  List.iter (function o -> Hashtbl.add path_table (Operator o, Operator o) [[]]) oprlist; 
  List.iter (function m -> Hashtbl.add path_table (Media m, Media m) [[]]) mdalist ;*)  
  initialize_failure_rates_table file_reliability (*Application.operators_list !archilib !archiname) (Application.media_list !archilib !archiname*); 
  let operator_source = List.nth (Application.operators_list !archilib !archiname) 0 in  
  (*let oprs,mdas = (Application.operators_list !archilib !archiname),(Application.media_list !archilib !archiname) in 
  List.iter (fun opr -> Hashtbl.add articulation_points (opr) (false,false,-1,-1,opr)) (oprs@mdas);
  let x = paths  (List.nth (Application.operators_list !archilib !archiname) 3) (List.nth (Application.operators_list !archilib !archiname) 4) in
  Hashtbl.add articulation_points operator_source (false,false);
  compute_articulation_points operator_source;*) 
  operator_source  


(* compute incremental relaibility cost function *) 	    
let compute_incremental_cost _Si _Fi =
  let len = (List.length opr_src.intervals) - 2 in
  let sum_C = ref 0.in
  for i=0 to len do
    let ti = List.nth opr_src.intervals i in 
    let tj = List.nth opr_src.intervals (i+1) in 
    match ti=tj with
    | true  -> ()
    | false ->	
	(match (((opr_src.s_tilde_old<ti)&&(ti<opr_src.f_tilde_old))||((opr_src.s_tilde_old<tj)&&(tj<opr_src.f_tilde_old)))  with
	 | false -> ()
	 | true  ->
	     let _c_tilde = opr_src.lambda *. (tj-.ti) *. _Fi in 
	     let _c       = opr_src.lambda *. (tj-.ti) *. _Fi in
	     debug_ps 10 debug_level ("["^(string_of_float ti)^","^(string_of_float tj)^"]  on  ["^(string_of_float opr_src.s_tilde_old)^","^
		   (string_of_float opr_src.f_tilde_old)^"] ---->  "^" remove "^(string_of_float (min _c _c_tilde)));
	     sum_C := !sum_C +. (min _c _c_tilde);)
  done;
  !sum_C  

(* update operator source intervals *)
let update_src_intervals _Si_new _Fi_new =
  opr_src.s_tilde_old <- opr_src.s_tilde;
  opr_src.f_tilde_old <- opr_src.f_tilde;
  opr_src.s_tilde <- min opr_src.s_tilde _Si_new;
  opr_src.f_tilde <- max opr_src.f_tilde _Fi_new;
  opr_src.intervals <- List.merge compare opr_src.intervals [_Si_new;_Fi_new]


(* Computes the adequation of graph. Fails with various exception if
something went wrong *)
let adequation file_reliability with_reliability graph =
  let s = Hashtbl.fold (fun _ opn s -> s^"\n"^(identifier_of_operation opn)^ (string_of_argsvalues opn.t_opn_arguments_values))
    graph "Initial graph :" in      
  debug_ps 1 debug_level s; 
  let operator_source = initialize_adequation file_reliability  graph in    
  opr_src.lambda <- (Hashtbl.find  failures_rate operator_source);
  debug_ps 10 debug_level ("operator source  =  "^(name_of_operator operator_source));
  let schedulables =
    ref (hashtbl_filter
	   (fun o -> (is_schedulable o) && (not (is_constant o)))
	   graph) in
  (*let total_adequation = hashtbl_length graph in   *)
  let progress_box = ()(*Progress_box.create total_adequation*) in
  try
    while !schedulables <> [] do
      debug_ps 10 debug_level ("\n *********************************************************************");
      debug_ps 10 debug_level (
	List.fold_left (fun s opn -> s^(identifier_of_operation opn)^" ") "schedulables : " !schedulables);
      (* Calcul of the optimal operator for each operation *)
      let operations_operatoropt =
	List.map
	  (fun opn ->
	     opn,(dynamic_level opn (operators_constraint opn) operator_source with_reliability graph))
	  !schedulables in
      let operation_optimal,(operator_optimal,(esfs_optimal,_Si_opr_src_opt,_Fi_opr_src_opt,_Ci_optimal,_DL_optimal)) =
	find_best_candidate operations_operatoropt in
      schedule operation_optimal operator_optimal false 0. 0.;
      let delta_opt =
	delta operation_optimal.t_opn_class operator_optimal in
      make_coms operation_optimal graph true;       
      schedule operation_optimal operator_optimal true esfs_optimal (esfs_optimal+.delta_opt);
      schedule_reliability := !schedule_reliability +. _Ci_optimal; 
      debug_ps 10 debug_level ("schedule "^(name_of_operation operation_optimal)^" on "^(name_of_operator operator_optimal)
			      ^" :: Si = "^(string_of_float  _Si_opr_src_opt)^" Fi = "^(string_of_float   _Fi_opr_src_opt)
			      ^" with C_i = "^(string_of_float  _Ci_optimal)^" Ri = "^(string_of_float  !schedule_reliability));
      (*compute incrementale cost reliability *
      update_src_intervals _Si_opr_src_opt _Fi_opr_src_opt;
      incremental_overlapping_C := !incremental_overlapping_C +. (compute_incremental_cost _Si_opr_src_opt _Fi_opr_src_opt);*) 
      debug_ps 10 debug_level (List.fold_left (fun l t -> l^" "^(string_of_float t)) " new interval --->   " opr_src.intervals);      
      debug_ps 10 debug_level ("-----------------------------------------");
      (* Data becoming available on operator *)
      List.iter
	(fun prt ->
	   match (prt.t_prt_dir,prt.t_prt_class) with
	   | Port.Out,_ -> debug_ps 4 debug_level
	       ("Adding "^(name_of_data (operation_optimal,prt))^" on "^
		  (name_of_operator operator_optimal)^"(with cond "^
		  (string_of_condlist operation_optimal.t_opn_condition)^")");
	       update_datas operator_optimal (operation_optimal,prt)
		 operation_optimal prt (eefs operation_optimal)
	   | _,_ -> ())
	operation_optimal.t_opn_ports;
      (* We need to place memory successor coms now (this is not
	 needed to determine operation esfs). *)
      (* Yet successor dependences will increase the total
	 latency. So this should be taken into account (improve this)
	 to choose best operator. *)
      (match is_memory operation_optimal with
       | true ->
	   make_memory_succ_coms graph (eefs operation_optimal) operation_optimal
       | false -> ());
      debug_ps 1 debug_level
	("adequation place_calcul : "^
	   (identifier_of_operation operation_optimal)^
	   " on "^(name_of_operator operator_optimal)^" at "^
	   (string_of_float esfs_optimal)^"..............ok");
      (* Update the list of schedulables operations *)
      update_schedulables operation_optimal schedulables;
      (*Progress_box.tick progress_box;*)
    done; 
    debug_ps 10 debug_level (string_of_float  !schedule_reliability); 
    debug_ps 10 debug_level (string_of_float  !incremental_overlapping_C); 
    place_constant graph progress_box;
    schedule_reliability :=  !schedule_reliability -. !incremental_overlapping_C; 
    schedule_length := Hashtbl.fold (fun  _  opn eefs_tmp -> (max eefs_tmp (eefs opn))) graph min_float;
    (* We need to have the list of operations sorted by esfs (for
       code generation) *)
    Hashtbl.iter
      (fun _ (_,oprschedule) ->
	 let opns =
	   Hashtbl.fold
	     (fun _ {t_sch_operations=cond_opns} opns -> opns@cond_opns)
	     oprschedule [] in
	 let opns_sorted =
	   List.stable_sort
	     (fun (opn1,esfs1,eefs1) (opn2,esfs2,eefs2) ->
		match compare esfs1 esfs2 with
		| 0 ->
		    (* two successive null duration operations *)
		    (match (esfs1 = eefs1) && (esfs2=eefs2) with
		     | true -> compare opn1.t_opn_adequation_order
			 opn2.t_opn_adequation_order 
		     | false -> compare eefs1 eefs2)
		| x -> x) opns in
	 ignore (List.fold_left
		   (function i -> function opn,_,_ -> opn.t_opn_rank <- i; i+1)
		   0 opns_sorted))     schedules;	
    (match debug_level > 0 with
     | true -> print_adequation_result ()
     | false -> ());
    graph,schedules
  with exn -> (*Progress_box.close progress_box;*)raise exn
    
  end
    
