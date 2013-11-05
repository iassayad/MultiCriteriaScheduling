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

   let debug_level = 0
   let schedule_reliability = ref 0. 
   let schedule_big_lambda = ref 0.
   let schedule_length = ref 0.
     
       (* Place in graph the communication corresponding to dependence dpd
	 after instant tcond with status status *)
    let place_communication graph status tcond dpd =
      (* Place the communication of operation_src.port_src between
	 operator_src and operator_dst after instant t0 *)
      let place_com (data,(operator_src,operation_src,port_src,t0))
	  operation_dst port_dst operator_dst dpdclass cond
	  place status =
	
	let rec place_com_aux sopn sprt eefs place status opr_src = 
	   (*ps ("from "^(name_of_operator opr_src)^" to "^
	     (name_of_operator operator_dst)^" placing "^
	     (name_of_operation sopn));*) 
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
		place_link best_lnk place sopn sprt eefs data graph cond
		  status dpdclass false in
	      (*  	    ps ("next opr "^name_of_operator next_opr); *)
	      place_com_aux opn prt eefs place status next_opr in 
	
	let eefs = max t0 tcond in
	(*       ps ("placing com from "^(name_of_operator operator_src)); *)
	let opnpred,prtpred,eefs =
	  place_com_aux operation_src port_src eefs place status operator_src in
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
      operator and the destination to determine the best route *)
      let best_operator_src =
	List.hd operators_src_list in 

      (* Place the communication *)
      ignore(place_com (data,best_operator_src) dopn dprt operator_dst
	       dpdclass cond true status)

(* Builds operation predecessors coms *)
    let make_coms operation graph status =
      let make_one_com tcond dpd =
	debug_ps 3 debug_level ("making com "^(string_of_dpd dpd));
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
      let (opn_min,(opr_min,(esfs_min,_))) =
	(* Find the candidate with minimum esfs and, if equalities,
	minimum eefs *)
	List.hd (list_min_elements
		   (fun (opn,(opr,(esfs,_))) ->
		     let eefs = esfs +. (delta opn.t_opn_class opr) in
		     (esfs,eefs))
		   operations_operatoropt (0.,0.)) in
      let eefs_min = esfs_min +. (delta opn_min.t_opn_class opr_min) in
      (* Select among candidates with esfs < eefs_min the candidate with
      maximum SP *)
      (*let opns_sp = ref " " in*)
      let (best_cand,_) =
	List.fold_left
	  (fun (cand_max,sp_max) ((opn,(opr,(esfs,sp))) as cand) ->
	    (*opns_sp :=  (!opns_sp^"   ("^(name_of_operation opn)^","^(name_of_operator opr)^","^(string_of_float sp) ^")  ");  *)
	    match (((esfs < eefs_min) || ((esfs = eefs_min) && ((delta opn.t_opn_class opr) = 0.))) && sp > sp_max) with
	    | true -> (cand,sp)
	    | false -> (cand_max,sp_max))
	  (List.hd operations_operatoropt,-1.) operations_operatoropt in
      (*ps !opns_sp;*)
      best_cand

(* Returns the optimal esfs, schedule pressure and operator for
operation. Fails if no operator is able to execute this operation *)
    let best_opr_esfs_sp operation operators graph =
      let opnlib,opnname = deflibname operation in
      let (opr_opt,esfs_opt,_) = 
	List.fold_left
	  (fun (opr_m,esfs_m,spfastmin) operator -> 
  	    debug_ps 2 debug_level ("Trying "^(identifier_of_operation operation)^
				    " on "^(name_of_operator operator));
	    match Architecture.able_to_execute opnlib opnname
		(operator_of_operator_class operator) with
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
	    | false -> (opr_m,esfs_m,spfastmin))
	  (List.hd operators,max_float,max_float) operators in
      match esfs_opt = max_float with
      | true ->
	  failwith ("No operator able to execute :\nOperation : "^
		    (name_of_operation operation)^"\nDefinition : "^
		    opnlib^"/"^opnname^
		    "\n\nMaybe you forgot to define a duration for this definition.\n")
      | false ->
	  let sp = (lefe operation) +. esfs_opt +. (delta operation.t_opn_class opr_opt) in
	 (*ps ("For operation "^(name_of_operation operation)^
	   " esfs "^(string_of_float esfs_opt)^" sp "^(string_of_float sp)); *)
	(opr_opt,(esfs_opt,sp))



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


(* Computes the adequation of graph. Fails with various exception if
something went wrong *)
    let adequation file_name graph =
      let s = Hashtbl.fold
	  (fun _ opn s -> s^"\n"^(identifier_of_operation opn)^
	    (string_of_argsvalues opn.t_opn_arguments_values))
	  graph "Initial graph :" in
      debug_ps 1 debug_level s;
      architecture_init graph;
      Hashtbl.clear lefe_table;
      adequation_order := 0;
      (*let all_operators =
	Application.operators_list !archilib !archiname in*)
      let schedulables =
	ref (hashtbl_filter
	       (fun o -> (is_schedulable o) && (not (is_constant o)))
	       graph) in
      (*let total_adequation = hashtbl_length graph in*)

      (*let operators_number = List.length all_operators in*)

      let progress_box = (*Progress_box.create total_adequation*) () in
      try
	while !schedulables <> [] do 
	  debug_ps 10 debug_level  "\n ========================================================";
   	  debug_ps 10 debug_level (
	  List.fold_left
	    (fun s opn -> s^(identifier_of_operation opn)^" ")
	    "schedulables : " !schedulables);
          (* Calcul of the optimal operator for each operation *)
	  let operations_operatoropt =
	    List.map
	      (fun opn ->
		opn,(best_opr_esfs_sp opn (operators_constraint opn) graph))
	      !schedulables in
	  let operation_optimal,(operator_optimal,(esfs_optimal,_)) =
	    find_best_candidate operations_operatoropt in
	  schedule operation_optimal operator_optimal false 0. 0.;
	  let delta_opt =
	    delta operation_optimal.t_opn_class operator_optimal in
 	  make_coms operation_optimal graph true;
	  schedule operation_optimal operator_optimal true esfs_optimal
	    (esfs_optimal+.delta_opt);
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
  	  debug_ps 10 debug_level
	    ("adequation place_calcul : "^
	     (identifier_of_operation operation_optimal)^
	     " on "^(name_of_operator operator_optimal)^" at "^
	     (string_of_float esfs_optimal)^"..............ok");
          (* Update the list of schedulables operations *)
	  update_schedulables operation_optimal schedulables;
	  (*Progress_box.tick progress_box;*)
	done;
	place_constant graph progress_box;

	schedule_length := Hashtbl.fold (fun  _  opn eefs_tmp -> (max eefs_tmp (eefs opn))) graph min_float;
	initialize_failure_rates_table file_name;

	(* Compute reliability *)
	let all_exe = ref 0. in
	let compute_rel opn = 
	  let opn_rel_comm = 
	    (List.fold_left (fun comm_rel send -> match (is_communication send) with
			     |false -> comm_rel;
			     |true -> 
				let lambda_mda = (Hashtbl.find failures_rate (pi send)) in 
				let com_exe    = (delta send.t_opn_class  (pi send)) in    
				all_exe := !all_exe +. com_exe;
			       comm_rel +. (lambda_mda*.com_exe);
			    ) 0. opn.t_opn_successors) in
	  let opn_exe    = delta opn.t_opn_class  (pi opn) in  
	  let lambda_opr = Hashtbl.find failures_rate (pi opn) in     
	  all_exe := !all_exe +. opn_exe;
	  debug_ps 10 debug_level ((name_of_operation opn)^" on "^(name_of_operator (pi opn))
				   ^" = "^(string_of_float (exp(-.(opn_rel_comm+.(lambda_opr*.opn_exe))))));
  	  exp(-.((opn_rel_comm *. 1.)+.(lambda_opr*.opn_exe)))  in
	
	schedule_reliability := (Hashtbl.fold (fun  _  opn tmp_rel -> match (is_communication opn) with
							| true  -> tmp_rel;
							| false -> tmp_rel *. compute_rel opn) graph 1.);

	debug_ps 10 debug_level ("\n           sum =   "^(string_of_float !all_exe)); 

	schedule_big_lambda := -. log(!schedule_reliability) /. !all_exe;

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
		    0 opns_sorted))
	  schedules;
	
	(match debug_level > 0 with
	| true -> print_adequation_result ()
	| false -> ());
	graph,schedules
      with exn -> (*Progress_box.close progress_box;*)raise exn

  end
