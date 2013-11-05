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

open Types
open Adequationtypes

(** This module contains the functions to transform the main algorithm
  for the adequation (expanding repetions and conditioning, flattening
  hierarchy).*)

let debug_level = 0

let globalexpression path expr =
  Algorithm.globalexpression_of_localexpression path expr
				   
let ps s =
  print_string (s^"\n");
  flush stdout

(** Prints information on dependence d *)
let psdpd d =
  let cond cl =
    cut (List.fold_left
	   (fun s (cond_src,vl) ->
	     s^(match cond_src,vl with
	     | None,Some 1 -> "T,"
	     | None,Some 0 -> "F,"
	     | (Some (opn,prt)),(Some vl) ->
		 (identifier_of_operation opn)^"."^prt.t_prt_name^"="^
		 (string_of_int vl)^","
	     | _ -> ""))
	   "" cl) 1 in
  let labeldependence = [Strong_Precedence_Data,"S";
			 Weak_Precedence_Data,"W";
			 Precedence,"P";
			 Types.Data,"D"] in
  let dpdtype = match d.t_dpd_class with
    | (Condition l),_ -> "Condition "^(string_of_int l)
    | Data,l -> "Data "^(List.assoc l labeldependence) in
    ps ("Dependence : "^(identifier_of_operation d.t_dpd_sopn)^"."^
	d.t_dpd_sprt.t_prt_name^"->"^(identifier_of_operation d.t_dpd_dopn)^
	"."^d.t_dpd_dprt.t_prt_name^"  "^dpdtype^" "^(cond d.t_dpd_condition))

(** Returns the repetition factor of operation according to its port sizes *)
let calculated_repetition_factor operation =
  let context = Algorithm.context_global () in
  let factor dpds dir =
    List.fold_left
      (fun maxvalue ({t_dpd_sopn=sopn;
		      t_dpd_sprt=sprt;
		      t_dpd_dprt=dprt;
		      t_dpd_dopn=dopn} as dpd) ->
			let sdim = sprt.t_prt_dim in
			let ddim = dprt.t_prt_dim in
			let nvalue, rest = match dir with
			| Port.In -> (sdim / ddim), (sdim mod ddim)
			| Port.Out -> (ddim / sdim), (ddim mod sdim) in
			let compatible_factors =
			  ((rest = 0) || (nvalue < 1)) &&
			  (((nvalue mod maxvalue) = 0) || ((maxvalue mod nvalue) = 0)) in
			(match compatible_factors with
			 | false ->
			     failwith ((identifier_of_operation operation)^
				       " has incompatible repetition factors")
			 | true -> ());
			  match nvalue > maxvalue with
			  | true -> nvalue
			  | false -> maxvalue)
      1 dpds in
    
  let alglib,algname,_ = Application.algo_main_get () in
  let data_dependences_in =
    List.filter
      (fun {t_dpd_class=(_,dpdclass)} ->
	dpdclass <> Precedence)
      operation.t_opn_dependences_predecessors in
  let factor_in = factor data_dependences_in Port.In in
    match factor_in > 1 with
    | true -> factor_in
    | false ->
	let data_dependences_out =
	  List.filter
	    (fun {t_dpd_class=(_,dpdclass)} -> dpdclass <> Precedence)
	    operation.t_opn_dependences_successors in
	  factor data_dependences_out Port.Out

(** Returns the repetition factor of operation. Factor is first
   calculated depending on port sizes. If it equals 0 then the
   specified factor is taken into account *)
let repetition_factor operation =
  let calc_factor_value = calculated_repetition_factor operation in
    match calc_factor_value = 1 with
    | true ->
	let alglib,algname =
	  operation.t_opn_referencing_alglib,operation.t_opn_referencing_algname in
	(match (alglib,algname) <> ("","") with
	 | true -> let (_,_,_,_,_,_,rep,_) =
	     Algorithm.referencename_content alglib
	       algname (name_of_operation operation) in
	     (match rep with
	      | Specified n -> n
	      | _ -> calc_factor_value)
	 | false -> calc_factor_value)
    | false -> calc_factor_value

(** Returns true if operation is atomic, false otherwise. *)
let atomic operation = match operation.t_opn_class with
  | Calcul (_,deflib,defname) -> Algorithm.atomic deflib defname
  | _ -> true

(** Returns false if operation is atomic, true otherwise. *)
let non_atomic operation = not (atomic operation)

(** Returns true if operation is conditioned, false otherwise. *)
let conditioned operation =
  match operation.t_opn_class with
  | Calcul (_,deflib,defname) -> Algorithm.conditioned deflib defname
  | _ -> false

(** Returns false if operation is conditioned, true otherwise. *)
let non_conditioned operation =
  not (conditioned operation)

(** Returns true if operation is factorized, false otherwise. *)
let factorized operation =
  let nvalue = repetition_factor operation in
    nvalue <> 1

(** Sets opn path to newpath. *)
let rename_operation graph operation newpath =
(*   ps ("RENAMING "^(identifier_of_operation operation)^
   " into "^(string_of_string_list newpath "/")); *)
  Hashtbl.remove graph (identifier_of_operation operation);
  operation.t_opn_path <- newpath;
  Hashtbl.add graph
    (unique_identifier graph newpath (name_of_operation operation)) operation;
  (* I don't now exactly why, but if you don't do this, successors and
  predecessors won't have their values t_opn_predecessors
  t_opn_successors up to date *)
  List.iter
    (fun ({t_dpd_sopn=sopn;
	   t_dpd_sprt=sprt;
	   t_dpd_dopn=dopn;
	   t_dpd_dprt=dprt;
	   t_dpd_class=dpdclass;
	   t_dpd_condition=dpdcond;
	   t_dpd_status=status} as dpd) -> dependence_remove dpd;
	    dependence_add sopn sprt dopn dprt dpdclass dpdcond status)
    (operation.t_opn_dependences_successors@operation.t_opn_dependences_predecessors)

(** Returns the operation and port which condition operation *)
let conditioning_port operation =
  let deflib,defname = deflibname operation in
  let cond_port,_ = Algorithm.cond_list deflib defname in
  let cond_dpd =
    try
      List.find
	(function {t_dpd_dprt=p} -> p.t_prt_name=cond_port)
	operation.t_opn_dependences_predecessors
    with Not_found ->
      failwith ("Conditioning port of operation "^
		(identifier_of_operation operation)^
		" is not in dependence with any output port") in
    cond_dpd.t_dpd_sopn,cond_dpd.t_dpd_sprt

(** Initializes a new graph structure containing alglib/algname *)
let initialize path original_path alglib algname parxsc =
  let condition (cond_name,cond_value) = match cond_name with
    | "" -> None,None
    | _ -> None,(Some cond_value) in
  let newgraph = Hashtbl.create 40 in
    (* Add an operation to the structure *)
  let add_opn (name,alglibref,algnameref,args,cond,_,_,_) =
    let sargs =
      List.fold_left
	(fun s arg -> s^","^(Symbolic.string_of_expression arg))
	"" args in
    (* ps ("Initializing "^alglib^"."^algname^" at "^
       (string_of_string_list path "/")); *)
      Algorithm.ports_order_sort_libname alglibref algnameref;
      let _,_,algoclass,_,ports,_,_,_,_,code_phases =
	Algorithm.algorithmname_content alglibref algnameref in
      let ports =
	List.map
	  (fun (n,d,dtn,dtd,prtclass,order,_) ->
	    let context =
	      Algorithm.context_global () in
	    let globaldim =
	      globalexpression (original_path@[name]) dtd in
	    let globaldim =
	      size_of_expression context (original_path@[name]) globaldim in
	    {t_prt_name=n;t_prt_dir=d;t_prt_typename=dtn;t_prt_dim=globaldim;
	     t_prt_class=prtclass;t_prt_order=order})
	  ports in
      let path_opn = path@[name] in
      let path_origin = original_path@[name] in
      let xsc = get_xsc original_path parxsc path_origin AttachRef in
      let opn =
	new_opn path_opn (Calcul (algoclass,alglibref,algnameref)) args ports
	  [] [] [condition cond] None 0 0. true (Ihm path_origin) None xsc
	  alglib algname code_phases in
	Hashtbl.add newgraph (identifier_of_operation opn) opn in
    (* Add a dependence to the structure *)
  let add_dpd ((srefname,sportname),(drefname,dportname),dpdclass,cond) =
    let sopn =
      Hashtbl.find newgraph (identifier_of_path_name path srefname)
    and dopn =
      Hashtbl.find newgraph (identifier_of_path_name path drefname) in
    let sprt =
      List.find
	(fun {t_prt_name=n;t_prt_dir=d} -> n=sportname && d=Port.Out)
	sopn.t_opn_ports
    and dprt =
      List.find
	(fun {t_prt_name=n;t_prt_dir=d} -> n=dportname && d=Port.In)
	dopn.t_opn_ports in
      dependence_add sopn sprt dopn dprt (Data,dpdclass) [condition cond] true in
    (* Initialize the graph structure *)
  let (_,_,_,_,_,refs,dpds,_,_,_) =
    Algorithm.algorithmname_content alglib algname in
  let dpds_external,dpds_internal =
    List.partition
      (fun ((srefname,_),(drefname,_),_,_) -> srefname="" || drefname="")
      dpds in
    List.iter add_opn refs;
    List.iter add_dpd dpds_internal;
    let dpds_external =
      List.map
	(fun (s,d,dpdclass,cond) -> s,d,(Data,dpdclass),[condition cond])
	dpds_external in
      newgraph,dpds_external

(** Adds a conditioning dependence between the conditionement vertice
   opncond.prtcond and the operation opn. *)
let add_conditioning_dependence opncond prtcond edgeclass opn order =
  match List.filter
      (fun {t_dpd_sopn=sopn;t_dpd_sprt=sprt} -> sopn=$opncond && sprt=%prtcond)
      opn.t_opn_dependences_predecessors with
  | dpd::_ ->
      dpd.t_dpd_class <- (edgeclass,(snd dpd.t_dpd_class));
      (* This dependence may be conditionned by another condition, already
	 conditionning opncond. So we produce and use dpd only if
	 opncond.t_opn_condition is satisfied. This won't be a problem, as long
	 as we test the conditions in a hierarchical way. Therefore, dpd will
	 be needed for testing the corresponding condition only if test
	 opncond.t_opn_condition, which is computed before test on dpd, is
	 true. *)
      dpd.t_dpd_condition <-  opncond.t_opn_condition
  | _ ->
      let condname =
	(name_of_identifier (identifier_of_operation opncond))^
	"_"^prtcond.t_prt_name in
      let condin =
	{t_prt_name=condname;
	 t_prt_dir=Port.In;
	 t_prt_typename=prtcond.t_prt_typename;
	 t_prt_dim=prtcond.t_prt_dim;
	 t_prt_class=Port.Data_Port;
	 t_prt_order=order} in
	opn.t_opn_ports <- condin :: opn.t_opn_ports;
	dependence_add opncond prtcond opn condin
	(edgeclass,Strong_Precedence_Data) (opncond.t_opn_condition) true

(** Copies all the conditionment edges of sopn to dopn *)
let copy_conditioning_dependences sopn dopn order =
  let condedges =
    List.filter
      (fun d ->
	match d.t_dpd_class with
	| (Condition _),_ -> true
	| _ -> false) sopn.t_opn_dependences_predecessors in
    List.iter
    (fun d ->
      add_conditioning_dependence d.t_dpd_sopn d.t_dpd_sprt
	(fst d.t_dpd_class) dopn order)
    condedges

(** Replaces in graph operation with subgraph, and connects subgraph to
graph with dpds *)
let replace_operation_subgraph graph operation subgraph dpds =
  debug_ps 1 debug_level ("replace_operation_subgraph..."^
			  (identifier_of_operation operation));
  let subpath = operation.t_opn_path in

  (* Operations of subgraph inherit from the condition of operation *)
  let opncond = conditioned operation in
  let newcondlist cond = match opncond with
    | true ->
	let opncond,prtcond = conditioning_port operation in
	(* let condname = (identifier_of_operation opncond)^"."^
	   prtcond.t_prt_name in*)
	  (match (List.hd cond) with
	   | _,None -> operation.t_opn_condition
	   | _,vl ->
	       (List.filter
		  (fun (cond_src,_) ->
		    match cond_src with
		    | None -> false
		    | Some _ -> true)
		  operation.t_opn_condition)@[(Some (opncond,prtcond)),vl])
    | false -> operation.t_opn_condition in
    Hashtbl.iter
    (fun n o ->
      o.t_opn_condition <- newcondlist o.t_opn_condition;
      let dpds =
	o.t_opn_dependences_successors @ o.t_opn_dependences_predecessors in
      List.iter
	(fun d -> d.t_dpd_condition <- newcondlist d.t_dpd_condition)
	dpds)
    subgraph;

    (* Connection of subgraph to graph *)
    List.iter
      (fun ((srefname,sportname),(drefname,dportname),dpdclass,cond) ->
	let slist = match srefname with
	| "" ->
	    (try
	      let dpd =
		List.find
		  (fun {t_dpd_dprt=p} -> p.t_prt_name=sportname)
		  operation.t_opn_dependences_predecessors in
	      [dpd.t_dpd_sopn,dpd.t_dpd_sprt,
	       Some (((fst dpdclass),(snd dpd.t_dpd_class)),(newcondlist cond))]
	    with Not_found ->
	      (* We'll do a complete concistancy check at the end, but we need to fail here too *)
	      failwith (Algorithm.string_of_port_ref ((identifier_of_operation operation),sportname)^
			" input port is not in dependence with any output port"))
	   | _ ->
	       let opn =
		 Hashtbl.find
		   subgraph
		   (identifier_of_path_name subpath srefname) in
	       let port =
		 List.find
		   (fun {t_prt_name=n;t_prt_dir=d} -> n=sportname && d=Port.Out)
		   opn.t_opn_ports in
		 [opn,port,None]
	 and dlist =
	  match drefname with
	  | "" ->
	      let dpdlist =
		List.filter
		  (fun {t_dpd_sprt=p} -> p.t_prt_name=dportname)
		  operation.t_opn_dependences_successors in
	       List.map
		(fun dpd -> dpd.t_dpd_dopn,dpd.t_dpd_dprt,
		  Some (((fst dpdclass),(snd dpd.t_dpd_class)),(newcondlist cond)))
		dpdlist
	 | _ ->
	     let opn =
	       Hashtbl.find
		 subgraph
		 (identifier_of_path_name subpath drefname) in
	     let port =
	       List.find
		 (fun {t_prt_name=n;t_prt_dir=d} -> n=dportname && d=Port.In)
		 opn.t_opn_ports in
	       [opn,port,None] in
	List.iter
	  (fun (sopn,sprt,sparameters) ->
	    List.iter
	      (fun (dopn,dprt,dparameters) ->
		let dpdclass,cond =
		  match sparameters,dparameters with
		  | Some (sdpdclass,scond), _ -> sdpdclass,scond
		  | None, Some (ddpdclass,dcond) -> ddpdclass,dcond
		  | _ -> dpdclass,cond in
		dependence_add sopn sprt dopn dprt dpdclass cond true)
	      dlist)
	  slist;
      ) dpds;

    (* Operations of subgraph inherit from the precedence edges of
    operation. We only add precedence on firsts or lasts due to
    precedence transitivity. *)
    let add_precedence sopn sprt dopn dprt =
      Hashtbl.iter
	(fun _ opn ->
	  match is_constant operation with
	  | true -> ()
	  | false ->
	      (* Only firsts inherit in precedences and lasts inherit
	      out precedences *)
	      let first_or_last =
		match dopn =$ operation with
		| true ->
		    List.for_all
		      (fun {t_dpd_sopn=sopn} ->
			(parentpath_of_path sopn.t_opn_path) <>
			(operation.t_opn_path))
		      opn.t_opn_dependences_predecessors (* in predence *)
		| false ->
		    List.for_all
		      (fun {t_dpd_dopn=dopn} ->
			(parentpath_of_path dopn.t_opn_path) <>
			(operation.t_opn_path))
		      opn.t_opn_dependences_successors in (* out precedence *)
	      match first_or_last with
	      | false -> ()
	      | true ->
		  let sopn,sprt,dopn,dprt = match dopn =$ operation with
		  | true -> sopn,sprt,opn,(find_precedence_port opn Port.In)
		  | false -> opn,(find_precedence_port opn Port.Out),dopn,dprt in
		  dependence_add sopn sprt dopn dprt (Data,Precedence)
		    [None,None] true) subgraph in
    
      Hashtbl.iter
      (fun _ o ->
	match is_constant o with
	| true -> ()
	| false ->
	    copy_conditioning_dependences operation o (-1)) subgraph;
      List.iter
      (fun {t_dpd_sopn=sopn;
	    t_dpd_sprt=sprt;
	    t_dpd_dopn=dopn;
	    t_dpd_dprt=dprt;
	    t_dpd_class=dpdclass} ->
	      match dpdclass with
	      | Data,Precedence -> add_precedence sopn sprt dopn dprt
	      | _ -> ())
      (operation.t_opn_dependences_predecessors@
       operation.t_opn_dependences_successors);

      (* Replace operation with subgraph *)
      operation_remove graph operation;
      Hashtbl.iter (function n -> function o -> Hashtbl.add graph n o) subgraph

(** Replaces hierarchical operation with its subgraph in graph. *)
let dehierarchize graph operation =
  (* let s =
     Hashtbl.fold
     (fun _ opn s -> s^"\n"^(identifier_of_operation opn))
     graph "Current graph :" in
     ps s;*)
  (*let origin_s =
     string_of_string_list (path_of_origin operation) "/" in
     ps ("Dehier "^(identifier_of_path operation.t_opn_path)^
     " with origin "^origin_s); *)
  let deflib,defname = deflibname operation in
  let original_path = path_of_origin operation in
  let subgraph,dpds =
    initialize operation.t_opn_path original_path deflib
      defname operation.t_opn_xsc_name in
    replace_operation_subgraph graph operation subgraph dpds

(** Adds a condition_in to graph. It is being added while
  deconditioning deconditioned_operation, condition is
  opncond.prtcond, the data of this condI is produced by sopn
  (possibly through several dependences), other parameters are
  required for naming purposes. *)
let add_condI_operation sopn parentPath parentDopnName opncond prtcond
    graph deconditioned_operation =
  debug_ps 2 debug_level ("Creating CondI for "^(identifier_of_operation sopn));
  let parpath = path_of_origin deconditioned_operation in
  let opnxsc =
    get_xsc parpath deconditioned_operation.t_opn_xsc_name
      parpath AttachCondI in
  let defname = "CondI" in
  (try
    Algorithm.algorithm_create "" defname Internal [] [] [] []
      Coord.No_coord2d "" [LoopSeq]
  with Failure _ -> ());
    let opnname =
      "CondI_"^(name_of_operation sopn)^"_"^parentDopnName in
    let opnname =
      unique_identifier graph deconditioned_operation.t_opn_path opnname in
    let opn =
      new_opn (parentPath@[opnname]) (Calcul (Internal,"",defname)) []
	(precedence_ports ()) [] [] deconditioned_operation.t_opn_condition
	None 0 0. true (Condition_In (path_of_origin deconditioned_operation))
	None opnxsc "" "" [LoopSeq] in
      (* ps ("decondition... adding : "^(identifier_of_operation opn)); *)
    Hashtbl.add graph (identifier_of_operation opn) opn;
    opn
  
(** This function performs some names updates, if some successors were
  in operations which have been deconditioned before
  deconditioned_operation. *)
(* To be more precise, we update the direct successors of the
   condition out operation condout.  dpdssucc are the dependences for
   which condout.portout is the source data opncond.prtcond is the
   condition of this condout This condition out is being added when
   deconditioning deconditioned_operation I recall that we however
   cannot fix this ordering problem by deconditioning in the order of
   the graph (as it's done for the factorization expansion), as there
   may be cycles between conditioned operations, which is in some
   cases not forbidden. *)
let update_successors dpdssucc condout portout opncond prtcond
    deconditioned_operation graph =
  debug_ps 2 debug_level ("Updating successors of "^
			  (identifier_of_operation condout));
  let condoutPortName =
    (name_of_operation condout)^"_"^portout.t_prt_name in
  let misnamed_cond cond condout =
(* ps ("Checking misname for condition "^(string_of_condition cond)^
   " compared to "^(identifier_of_operation condout));  *)
    match cond with
    | (Some (opn,prt),_) ->
	let opnparpath,opnname =
	  (parentpath_of_path opn.t_opn_path),(name_of_operation opn) in
      let misnamed =
	(identifier_of_operation condout) =
	identifier_of_path (opnparpath@["CondO_"^opnname^"_"^prt.t_prt_name]) in
	(* ps (string_of_bool misnamed); *)
	misnamed
    | _ -> false in
  let rename_cond cond condout portout =
    match cond with
    | Some _, Some vl -> let newCond = Some (condout,portout) in
	(* ps ("Renamed "^(string_of_condition cond)^" condition into "^
	   (string_of_condition (newCond,(Some vl)))); *)
      (newCond,(Some vl))
    | _ -> cond in
  let misnamed_port port =
    (* ps ("Checking misnamed port for "^port.t_prt_name); *)
    (name_of_operation condout)="CondO_"^port.t_prt_name in
  let rename_port port =
    (* ps ("Renamed "^port.t_prt_name^" port into "^condoutPortName); *)
    port.t_prt_name <- condoutPortName in
  let rename_ports operation =
    List.iter (fun port ->
		 match misnamed_port port with
		 | true -> rename_port port
		 | false -> ()) operation.t_opn_ports in
  let rename_or_split graph condI =
    (*     ps ("Splitting "^(name_of_operation condI)); *)
    let succ =
      (List.hd condI.t_opn_dependences_successors).t_dpd_dopn in
    let successorparent = (parentname_of_operation succ) in
    let dataDpds =
      List.filter
	is_dependence_with_data
	condI.t_opn_dependences_predecessors in
      match (List.length dataDpds) > 1 with
      | true ->
   (* operation used to be a CondI receiving several data from the same predecessor,
      but now this predecessor data has been split between several CondO.
      This CondI must therefore be split into several CondI...*)
	  let newCondI =
	    add_condI_operation condout (parentpath_of_operation succ)
	      successorparent opncond prtcond graph
	      deconditioned_operation in
	  condI.t_opn_ports <-
	    List.fold_left
	      (fun prts prt -> 
		match prt.t_prt_name = condoutPortName with
		| true ->
                (* transfer port and transfer dependences to newCondI *)
		    (match prt.t_prt_dir with
		    | Port.Out -> 
			newCondI.t_opn_ports <- newCondI.t_opn_ports@[prt];
			List.iter
			  (fun dpd ->
			    match dpd.t_dpd_sprt.t_prt_name = condoutPortName with
			    | true ->
				dependence_add newCondI prt dpd.t_dpd_dopn
				  dpd.t_dpd_dprt dpd.t_dpd_class
				  dpd.t_dpd_condition true;
				dependence_remove dpd
			    | false -> ())
			  condI.t_opn_dependences_successors;
				prts
			    | Port.In ->
				let dpdin =
				  try
				    List.find
				      (fun dpd ->
					dpd.t_dpd_dprt.t_prt_name = condoutPortName)
				      condI.t_opn_dependences_predecessors
				  with Not_found ->
				    failwith ("Could not find "^
					      (name_of_operation newCondI)^
					      " data dependence in") in
				newCondI.t_opn_ports <- newCondI.t_opn_ports@[prt];
 				dependence_remove dpdin;
				prts);
		| false -> prts@[prt]) [] condI.t_opn_ports;
	  copy_conditioning_dependences condI newCondI (-1);
	  (* Copy exiting precedences *)
	  let newcondIPrecprt =
	    List.find
	      (fun prt -> (is_precedence_port prt) && prt.t_prt_dir = Port.In)
	      newCondI.t_opn_ports in
	  List.iter
	    (fun dpd ->
	      match dpd.t_dpd_class with
	      | Data,Precedence ->
		  dependence_add dpd.t_dpd_sopn dpd.t_dpd_sprt newCondI
		    newcondIPrecprt (Data,Precedence) dpd.t_dpd_condition true
	      | _ -> ())
	    condI.t_opn_dependences_predecessors;
	      let newcondIDataprt =
		List.find
		  (fun prt ->
		    prt.t_prt_name = condoutPortName && prt.t_prt_dir = Port.In)
		  newCondI.t_opn_ports in
	      newCondI,newcondIDataprt
      | false ->
	  let newpath =
	    ((parentpath_of_operation condI)@
	     ["CondI_"^(name_of_operation condout)^"_"^successorparent]) in
	  rename_operation graph condI newpath;
	  let condIDataprt =
	    List.find
	      (fun prt ->
		prt.t_prt_name = condoutPortName && prt.t_prt_dir = Port.In)
	      condI.t_opn_ports in
	    condI,condIDataprt in
    (* Renames successors:
       - conditions
       - ports in case of CondI/CondO
       - operation in case of CondI *)
  let rename_successor_conds operation condout = 
    operation.t_opn_condition <-
    List.fold_left
	(fun conds cond ->
	  match misnamed_cond cond condout with
	  | true -> (* Changing opn successor dependences condition. *)
	      List.iter
		(fun dpd -> 
		  dpd.t_dpd_condition <-
		    List.fold_left
		      (fun conds cond ->
			match misnamed_cond cond condout with
			| true -> conds@[rename_cond cond condout portout]
			| false -> conds@[cond])
		      [] dpd.t_dpd_condition)
		operation.t_opn_dependences_successors;
	      (* Also changing operation condition *)
	      conds@[rename_cond cond condout portout]
	  | false -> conds@[cond])
	[] operation.t_opn_condition in
    List.iter
    (fun ({t_dpd_sopn=sopn;
	  t_dpd_dopn=dopn;
	  t_dpd_dprt=dprt;
	  t_dpd_class=dpdclass;
	  t_dpd_condition=dpdcond} as dpd) ->
	    (* ps ("Processing dependence "^(string_of_dpd dpd)); *)
	    let dopn,dprt = match dpdclass with
	    | (Condition _),_ ->
		rename_successor_conds dopn condout;
		(* Renaming CondI/CondO ports*)
		(match dopn.t_opn_origin with
		| Condition_Out _ | Condition_In _ -> 
		    rename_ports dopn			   
		| _ -> ());
		dopn,dprt
	    | _ -> (match dopn.t_opn_origin with
	      | Condition_In _ ->
		  (* Rename ports *)
		  rename_ports dopn;
		  let conditioning_dpd =
		    List.find
		      (fun dpd ->
			match dpd.t_dpd_class with
			| (Condition _),_ ->
			    (parentpath_of_operation sopn) =
			    (parentpath_of_operation condout)
			| _ -> false)
		      dopn.t_opn_dependences_predecessors in
		  let opncond,prtcond =
		    conditioning_dpd.t_dpd_sopn,conditioning_dpd.t_dpd_sprt in
		  (* Rename conditions *)
		  List.iter
		    (fun dpd -> 
		      dpd.t_dpd_condition <-
			List.fold_left
			  (fun conds cond ->
			    match misnamed_cond cond opncond with
			    | true -> conds@[rename_cond cond opncond prtcond]
			    | false -> conds@[cond])
			  [] dpd.t_dpd_condition)
		    dopn.t_opn_dependences_successors;
		  (* Rename operation *)
		  let successor =
		    try
		      List.hd dopn.t_opn_dependences_successors
		    with _ ->
		      failwith ("One input port of "^
				(identifier_of_path (path_of_origin dopn))^
				" is not in dependence with any input port (in any condition).") in
		  let successorparent =
		    (parentname_of_operation successor.t_dpd_dopn) in
                  (* In case the CondI is split,
		     dopn and dprt change! *)
		  let dopn,dprt = rename_or_split graph dopn in 
		  dopn,dprt
	      | _ -> dopn,dprt) in
	    (* Anyway, we must modify successor dependences src port *)
	    dependence_add condout portout dopn dprt dpdclass dpdcond true;
	    dependence_remove dpd)
    dpdssucc

(** Adds a conditioning input as input of [deconditioned_operation]. This
   data of this input is produced by [opnpred] *)
let add_conditioning_input deconditioned_operation graph levelcond opncond
    prtcond opnpred =
   (* second part of the test is in case
      decondition was done in wrong order *)
  match opnpred=$opncond && (not (conditioned opnpred) ) with
  | true -> ()
  | false ->
      debug_ps 2 debug_level ("Adding condition in "^
			      (identifier_of_operation opnpred));
      let dpdspred =
	List.filter
        (* second part of the test is in case
	 decondition was done in wrong order *)
	  (fun ({t_dpd_dopn = dopn;
		 t_dpd_sprt = sprt;
		 t_dpd_sopn = sopn} as dpd) ->
		   dopn=$deconditioned_operation &&
		   (not ((sprt =% prtcond) && sopn =$ opncond)))
	  opnpred.t_opn_dependences_successors in
      let dpdspred,_ =
	List.partition
	  (fun {t_dpd_class=dpdclass} ->
	    match dpdclass with
	    | (Condition _),_ -> false
	    | _ -> true)
	  dpdspred in
      (*ps "decondition...dpdspred";*)
      match dpdspred with
      | [] -> ()
      | _ ->
	  (*ps ("decondition... ports");*)
	  let opn =
	    add_condI_operation opnpred deconditioned_operation.t_opn_path
	      (name_of_operation deconditioned_operation)
	      opncond prtcond graph deconditioned_operation in
	  add_conditioning_dependence opncond prtcond (Condition levelcond) opn (-1);
	  copy_conditioning_dependences deconditioned_operation opn (-1);
	  (* Copy entering precedences *)
	  let condIPrecprt =
	    List.find
	      (fun prt -> (is_precedence_port prt) && prt.t_prt_dir = Port.In)
	      opn.t_opn_ports in
	  List.iter
	    (fun dpd ->
	      match dpd.t_dpd_class with
	      | Data,Precedence ->
		  dependence_add dpd.t_dpd_sopn dpd.t_dpd_sprt opn condIPrecprt
		    (Data,Precedence) dpd.t_dpd_condition true
	      | _ -> ())
	    deconditioned_operation.t_opn_dependences_predecessors;
	  (* process dependences grouping by port of predecessor *)
	  let rec process_dpds dpds order = match dpds with
	  | [] -> ()
	  | {t_dpd_sprt = prtpred;
	     t_dpd_class = dpdclass;
	     t_dpd_condition = dpdcond}::_ ->
	      let dpds,others =
		List.partition
		  (fun {t_dpd_sprt=prt} ->
		    prt=%prtpred)
		  dpds in
	      (* portin is the in port of the predecessor data, portout
	      is the out port of the conditioned data *)
	      let portname =
		(name_of_identifier (identifier_of_operation opnpred))^
		"_"^prtpred.t_prt_name in
	      let portin =
		{t_prt_name = portname;
		 t_prt_dir = Port.In;
		 t_prt_typename = prtpred.t_prt_typename;
		 t_prt_dim = prtpred.t_prt_dim;
		 t_prt_class = Port.Data_Port;
		 t_prt_order = order}
	      and portout =
		{t_prt_name = portname;
		 t_prt_dir = Port.Out;
		 t_prt_typename = prtpred.t_prt_typename;
		 t_prt_dim = prtpred.t_prt_dim;
		 t_prt_class = Port.Data_Port;
		 t_prt_order = order} in
	      opn.t_opn_ports <- opn.t_opn_ports@[portin;portout];
	      dependence_add opnpred prtpred opn portin dpdclass dpdcond true;
	      List.iter
		(fun ({t_dpd_dprt=prtsucc} as dpd) ->
		  dependence_add opn portout deconditioned_operation
		    prtsucc dpdclass dpdcond true;
		  dependence_remove dpd)
		dpds;
	      process_dpds others (order+1) in
	  process_dpds dpdspred 1

(** Adds a conditioning output as output of [deconditioned_operation]. The
   data of this output is produced by [port] of [deconditioned_operation] *)
let add_conditioning_output graph deconditioned_operation levelcond
    opncond prtcond port =
  let parpath =
    (path_of_origin deconditioned_operation) in
  let opnxsc =
    get_xsc parpath deconditioned_operation.t_opn_xsc_name
      parpath AttachCondO in
  let defname = "CondO" in
  (try
    Algorithm.algorithm_create "" defname Internal [] [] [] []
      Coord.No_coord2d "" [LoopSeq]
  with Failure _ -> ());
  let dpdssucc =
    List.filter
      (fun {t_dpd_sprt=sprt} -> sprt=%port)
      deconditioned_operation.t_opn_dependences_successors in
  let opnname =
    "CondO_"^(name_of_operation deconditioned_operation)^"_"^port.t_prt_name in
  let opnname =
    unique_identifier graph (parentpath_of_operation deconditioned_operation)
      opnname in
  let portname =
    (name_of_operation deconditioned_operation)^"_"^port.t_prt_name in
  let portin =
    {t_prt_name=portname;t_prt_dir=Port.In;t_prt_typename=port.t_prt_typename;
     t_prt_dim=port.t_prt_dim;t_prt_class=Port.Data_Port;t_prt_order=1}
  and portout =
    {t_prt_name=portname;t_prt_dir=Port.Out;t_prt_typename=port.t_prt_typename;
     t_prt_dim=port.t_prt_dim;t_prt_class=Port.Data_Port;t_prt_order=1} in
  let opn =
    new_opn ((parentpath_of_operation deconditioned_operation)@[opnname])
      (Calcul (Internal,"",defname)) [] ([portin;portout]@(precedence_ports ()))
      [] [] deconditioned_operation.t_opn_condition None 0 0.
      true (Condition_Out (path_of_origin deconditioned_operation))
      None opnxsc "" "" [LoopSeq] in
  Hashtbl.add graph (identifier_of_operation opn) opn;
  add_conditioning_dependence opncond prtcond (Condition levelcond) opn 0;
  copy_conditioning_dependences deconditioned_operation opn (-1);
  dependence_add deconditioned_operation port opn portin
    (Data,Strong_Precedence_Data)
    deconditioned_operation.t_opn_condition true;
  (* Copy precedences *)
  let condOPrecprt =
    List.find
      (fun prt -> (is_precedence_port prt) && prt.t_prt_dir = Port.Out)
      opn.t_opn_ports in
  List.iter
    (fun dpd ->
      match dpd.t_dpd_class with
      | Data,Precedence ->
	  dependence_add opn condOPrecprt dpd.t_dpd_dopn dpd.t_dpd_dprt
	    (Data,Precedence) dpd.t_dpd_condition true
      | _ -> ())
    deconditioned_operation.t_opn_dependences_successors;
  update_successors dpdssucc opn portout opncond prtcond
    deconditioned_operation graph


(** Deconditions operation in graph. *)
let decondition graph levelcond deconditioned_operation =
  debug_ps 1 debug_level ("Deconditioning "^
			  (identifier_of_operation deconditioned_operation));

  let opncond,prtcond = conditioning_port deconditioned_operation in

  (* Add conditioning In vertice for each predecessor (except conditioning
  predecessor). *)
    debug_ps 1 debug_level "decondition...in";
    let dpds_nonconst =
      List.filter
	(fun {t_dpd_sopn=sopn} ->
	  not (is_constant sopn))
	deconditioned_operation.t_opn_dependences_predecessors in
    let dpdpreds =
      remove_copies (List.map
		       (fun {t_dpd_sopn=sopn} -> sopn)
		       dpds_nonconst) in
      List.iter
      (add_conditioning_input deconditioned_operation graph levelcond
	 opncond prtcond)
      dpdpreds;

      (* Add conditioning Out vertice for each out port *)
      debug_ps 1 debug_level "decondition...out";
      let portsout =
	List.filter
	  (fun p ->
	    (not (is_precedence_port p)) && p.t_prt_dir=Port.Out)
	  deconditioned_operation.t_opn_ports in
	List.iter
	(add_conditioning_output graph deconditioned_operation
	   levelcond opncond prtcond)
	portsout;

      (* Replace operation by subgraph *)
	debug_ps 1 debug_level "decondition...replace";
	let deflib,defname = deflibname deconditioned_operation in
	let subgraph,dpds =
	  initialize deconditioned_operation.t_opn_path
	    (path_of_origin deconditioned_operation) deflib defname
	    deconditioned_operation.t_opn_xsc_name in
	replace_operation_subgraph graph deconditioned_operation subgraph dpds;

	(* Add conditioning dependences *)
	debug_ps 1 debug_level "decondition...ok";
	Hashtbl.iter
	  (fun _ opn ->
	    match is_constant opn with
	    | true -> ()
	    | false ->
		add_conditioning_dependence opncond prtcond
		  (Condition levelcond) opn (-1)) subgraph
	  
(** Copies precedences of opnrepeated to subopn, which is either a
   factorization boundary or a repetition of opnrepeated. *)
let copy_precedences opnrepeated subopn =
  (* Copy in precedences *)
  (match subopn.t_opn_origin with
  | (Implode _) | (Join _) -> ()
  | _ ->
      List.iter
	(fun dpd ->
	  match dpd.t_dpd_class with
	  | (_,Precedence) ->
	      dependence_add dpd.t_dpd_sopn (find_precedence_port dpd.t_dpd_sopn Port.Out)
		subopn (find_precedence_port subopn Port.In) dpd.t_dpd_class
		dpd.t_dpd_condition dpd.t_dpd_status
	  | _ -> ())
	opnrepeated.t_opn_dependences_predecessors);
      
  (*Copy out precedences *)
  (match subopn.t_opn_origin with
  | (Explode _) | (Fork _) | (Diffuse _) -> ()
  | _ ->
      List.iter
	(fun dpd ->
	  match dpd.t_dpd_class with
	  | (_,Precedence) ->
	      dependence_add subopn (find_precedence_port subopn Port.Out) dpd.t_dpd_dopn
		(find_precedence_port dpd.t_dpd_dopn Port.In) dpd.t_dpd_class
		dpd.t_dpd_condition dpd.t_dpd_status
	  | _ -> ())
	opnrepeated.t_opn_dependences_successors)

(** Depending on adequation_type :
  - either expands the factorized operation operation
  - or adds factorization boundaries.
  nexpr,nvalue is the factorization factor of operation. *)
let expand graph nvalue adequation_type operation =
  (* Repetition factor n *)
  debug_ps 1 debug_level ("expand "^(identifier_of_operation operation));
  (*   ps ("Factor of "^(identifier_of_operation operation)^" = "^(string_of_int nvalue)); *)
  (* n copies of operation *)
  let opnpath = path_of_origin operation in
  let opnxsc = get_xsc opnpath operation.t_opn_xsc_name opnpath AttachRef in
  let rec copy_opn i = match i with
  | 0 -> []
  | _ -> let opn =
      {operation with
       t_opn_path =
       (parentpath_of_operation operation)@
       [((name_of_operation operation)^"_"^(string_of_int (i-1)))];
       t_opn_dependences_predecessors=[];
       t_opn_predecessors=[];
       t_opn_dependences_successors=[];
       t_opn_successors=[];
       t_opn_origin=Ihm (path_of_origin operation);
       t_opn_referencing_alglib = "";
       t_opn_referencing_algname = "";
       t_opn_xsc_name = opnxsc} in
    (copy_opn (i-1))@[opn] in

  (* Adds a new repetition boundary between sopn.sprt and dopn.dprt *)
  let add_repetition_boundary boundary_name sopn sprt dopn dprt =
    let defname = boundary_name^"_"^sprt.t_prt_typename in
    (try Algorithm.algorithm_create "" defname Internal [] [] [] []
	Coord.No_coord2d "" [LoopSeq]
    with Failure _ -> ());
    let opnname =
      boundary_name^"_"^(name_of_operation sopn)^"_"^sprt.t_prt_name^
      "_"^(name_of_operation dopn) in
    let opnname =
      unique_identifier graph (parentpath_of_operation operation) opnname in
    let portin =
      {t_prt_name = sprt.t_prt_name;
       t_prt_dir = Port.In;
       t_prt_typename = sprt.t_prt_typename;
       t_prt_dim = sprt.t_prt_dim;
       t_prt_class = Port.Data_Port;
       t_prt_order = 1} in
    let portout =
      {t_prt_name = dprt.t_prt_name;
       t_prt_dir = Port.Out;
       t_prt_typename = dprt.t_prt_typename;
       t_prt_dim = dprt.t_prt_dim;
       t_prt_class = Port.Data_Port;
       t_prt_order = 2} in
    let opn_type =
      let type_params =
	(path_of_origin operation),nvalue in
      match boundary_name with
      | "Diffuse" -> Diffuse type_params
      | "Fork" -> Fork type_params
      | "Join" -> Join type_params
      | _ ->
	  failwith ("Transformation.expand: wrong repetition boundary type") in
    let opn =
      new_opn ((parentpath_of_operation operation)@[opnname])
	(Calcul (Internal,"",defname)) [] ([portin;portout]@(precedence_ports ()))
	[] [] operation.t_opn_condition None 0 0. true opn_type
	None "" "" "" [LoopSeq] in
    copy_precedences operation opn;
    copy_conditioning_dependences operation opn (-1);
    Hashtbl.add graph (identifier_of_operation opn) opn;
    opn,portout,portin in

  let opns = match adequation_type with
  | No_Repetition_Flatten -> []
  | _ -> copy_opn nvalue in
  List.iter (function opn -> copy_precedences operation opn;
    Hashtbl.add graph (identifier_of_operation opn) opn) opns;

  (* Process an in dependence *)
  let expand_dependence_in dpd =
    let sopn,sprt,dopn,dprt,dpdclass,dpdcond =
      dpd.t_dpd_sopn,dpd.t_dpd_sprt,dpd.t_dpd_dopn,dpd.t_dpd_dprt,
      dpd.t_dpd_class,dpd.t_dpd_condition in
    (*ps ((identifier_of_operation sopn)^"."^sprt.t_prt_name^"->"^
       (identifier_of_operation dopn)^"."^dprt.t_prt_name);
       psc context;
       ps ("sprtdim="^(Symbolic.string_of_expression sprtdim));
       ps ("dprtdim="^(Symbolic.string_of_expression dprtdim));*)
    match sprt.t_prt_dim = dprt.t_prt_dim with
    | true -> (* diffusion *)
	(match adequation_type with
	| No_Repetition_Flatten ->
	    (match List.exists
		(fun prt ->
		  prt.t_prt_name = dprt.t_prt_name && prt.t_prt_dir=Port.Out)
		operation.t_opn_ports with
	    | true -> () (* iterate, no diffuse *)
	    | false ->
		let diffuse,dportout,dportin =
		  add_repetition_boundary "Diffuse" sopn sprt dopn dprt in
		dependence_add sopn sprt diffuse dportin dpdclass dpdcond true;
		dependence_add diffuse dportout dopn dprt dpdclass dpdcond true;
		dependence_remove dpd)
	| _ ->
	    List.iter
	      (fun opn ->
		let opnprt =
		  List.find
		    (fun {t_prt_name=n;t_prt_dir=d} -> n=dprt.t_prt_name && d=Port.In)
		    opn.t_opn_ports in
		dependence_add sopn sprt opn opnprt dpdclass dpdcond true)
	      opns)
    | false -> (* explode *)
	(match adequation_type with
	| No_Repetition_Flatten ->
	    let fork,fportout,fportin =
	      add_repetition_boundary "Fork" sopn sprt dopn dprt in
	    dependence_add sopn sprt fork fportin dpdclass dpdcond true;
	    dependence_add fork fportout dopn dprt dpdclass dpdcond true;
	    dependence_remove dpd
	| _ -> let defname = "Explode_"^sprt.t_prt_typename in
	  (try
	    Algorithm.algorithm_create "" defname Internal ["dim"]
	      [] [] [] Coord.No_coord2d "" [LoopSeq]
	  with Failure _ -> ());
	  let opnname =
	    "Explode_"^(name_of_operation sopn)^"_"^sprt.t_prt_name^
	    "_"^(name_of_operation operation) in
	  let opnname =
	    unique_identifier graph (parentpath_of_operation operation) opnname in
	  let originpath = path_of_origin operation in
	  let xsc =
	    get_xsc originpath opnxsc originpath AttachExplode in
	  let portin =
	    {t_prt_name = sprt.t_prt_name;
	     t_prt_dir = Port.In;
	     t_prt_typename = sprt.t_prt_typename;
	     t_prt_dim = sprt.t_prt_dim;
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 1} in
	  let opn =
	    new_opn ((parentpath_of_operation operation)@[opnname])
	      (Calcul (Internal,"",defname))
	      [Symbolic.Float (float_of_int dprt.t_prt_dim)]
	      ([portin]@(precedence_ports ())) [] [] operation.t_opn_condition
	      None 0 0. true (Explode (path_of_origin operation))
	      None xsc "" "" [LoopSeq] in
	  Hashtbl.add graph (identifier_of_operation opn) opn;
	  dependence_add sopn sprt opn portin dpdclass dpdcond true;
	  copy_precedences operation opn;
	  copy_conditioning_dependences operation opn (-1);
	  ignore(List.fold_left
		   (fun i o ->
		     let portoutname =
		       sprt.t_prt_name^"_"^(string_of_int i) in
		     let portout =
		       {t_prt_name = portoutname;
			t_prt_dir = Port.Out;
			t_prt_typename = sprt.t_prt_typename;
			t_prt_dim = dprt.t_prt_dim; 
			t_prt_class = Port.Data_Port;
			t_prt_order = i+1} in
		     opn.t_opn_ports <- opn.t_opn_ports@[portout];
		     (*ps("cherche "^(identifier_of_operation o)^
			"."^dprt.t_prt_name);*)
		     let oprt =
		       List.find
			 (fun {t_prt_name=n;t_prt_dir=d} ->
			   n=dprt.t_prt_name && d=Port.In)
			 o.t_opn_ports in
		     (*ps("cherche ok");*)
		     dependence_add opn portout o oprt dpdclass dpdcond true;
		     i+1)
		   0 opns))

  (* Process an out port. We process out ports instead of out
  dependences in order to factorize joins or iterates used by
  several dependences on the same output port. *)
  and expand_port_out sprt =
    let portname = sprt.t_prt_name in
    let portdim = sprt.t_prt_dim in
    let dpds =
      List.filter
	(fun {t_dpd_sprt=sprt} ->
	  sprt.t_prt_name=portname)
	operation.t_opn_dependences_successors in
    let iteratedpds,joindpds =
      List.partition
	(fun {t_dpd_dopn=dopn;t_dpd_dprt=dprt} ->
	  match dprt.t_prt_dim = portdim with
	  | true -> (* we need an iterate port *)
	      List.exists
		(fun {t_prt_name=n;t_prt_dir=d} ->
		  n=portname && d=Port.In)
		operation.t_opn_ports
	  |	false -> false)
	dpds in
    (match iteratedpds with (* iterate *)
    | [] -> ()
    | _ ->
	(match adequation_type with
	| No_Repetition_Flatten ->
	    let portin =
	      List.find 
		(fun {t_prt_name=n;t_prt_dir=d} -> n=portname && d=Port.In)
	      operation.t_opn_ports in
	  let portout =
	    List.find
	      (fun {t_prt_name=n;t_prt_dir=d} -> n=portname && d=Port.Out)
	      operation.t_opn_ports in
	  let initdpd =
	    List.find
	      (fun {t_dpd_dprt=dprt} -> dprt=%portin)
	      operation.t_opn_dependences_predecessors in
	  let dpdclass,dpdcond =
	    initdpd.t_dpd_class,initdpd.t_dpd_condition in
	  let defname = "Iterate_"^portout.t_prt_typename in
	  (try
	    Algorithm.algorithm_create "" defname Internal [] [] [] []
	      Coord.No_coord2d "" [LoopSeq]
	  with Failure _ -> ());
	  let opnname =
	    "Iterate_"^(name_of_operation operation)^"_"^portname in
	  let opnname =
	    unique_identifier graph (parentpath_of_operation operation)
	      opnname in
	  let initPort =
	    {t_prt_name = "init";
	     t_prt_dir = Port.In;
	     t_prt_typename = portin.t_prt_typename;
	     t_prt_dim = portin.t_prt_dim; 
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 1} in
	  let itPortIn =
	    {t_prt_name = portname;
	     t_prt_dir = Port.In;
	     t_prt_typename = portin.t_prt_typename;
	     t_prt_dim = portin.t_prt_dim;
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 2} in
	  let finalPort =
	    {t_prt_name = "end";
	     t_prt_dir = Port.Out;
	     t_prt_typename = portout.t_prt_typename;
	     t_prt_dim = portout.t_prt_dim;
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 1} in
	  let itPortOut =
	    {t_prt_name = portname;
	     t_prt_dir = Port.Out;
	     t_prt_typename = portout.t_prt_typename;
	     t_prt_dim = portout.t_prt_dim;
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 2} in
	  let iterate =
	    new_opn ((parentpath_of_operation operation)@[opnname])
	      (Calcul (Internal,"",defname)) []
	      ([initPort;itPortIn;finalPort;itPortOut]@(precedence_ports ()))
	      [] [] operation.t_opn_condition None 0 0. true
	      (Iterate ((path_of_origin operation),nvalue)) None
	      "" "" "" [LoopSeq] in
	  Hashtbl.add graph (identifier_of_operation iterate) iterate;
	  dependence_add initdpd.t_dpd_sopn initdpd.t_dpd_sprt iterate initPort
	    dpdclass dpdcond true;
	  dependence_remove initdpd;
	  dependence_add operation portout iterate itPortIn dpdclass
	    dpdcond true;
	  dependence_add iterate itPortOut operation portin dpdclass
	    dpdcond true;
	  List.iter
	    (fun dpd ->
	      dependence_add iterate finalPort dpd.t_dpd_dopn dpd.t_dpd_dprt
		dpdclass dpdcond true;
	    dependence_remove dpd)
	    iteratedpds
	| _ ->
	    let rec f l =
	      match l with
	      | opn0::opn1::tl -> 
		  let opn0prt =
		    List.find
		      (fun {t_prt_name=n;t_prt_dir=d} ->
			n=portname && d=Port.Out)
		      opn0.t_opn_ports
		  and opn1prt =
		    List.find
		      (fun {t_prt_name=n;t_prt_dir=d} ->
			n=portname && d=Port.In)
		      opn1.t_opn_ports in
		  dependence_remove ((List.find
					(fun {t_dpd_dprt=dprt} -> dprt=%opn1prt)
					opn1.t_opn_dependences_predecessors));
		  dependence_add opn0 opn0prt opn1 opn1prt
		    (Data,Strong_Precedence_Data) operation.t_opn_condition true;
		  f (opn1::tl)
	      | opn::[] ->
		  let opnprt =
		    List.find
		      (fun {t_prt_name=n;t_prt_dir=d} -> n=portname && d=Port.Out)
		      opn.t_opn_ports in
		  List.iter
		    (fun {t_dpd_dopn = dopn;
			  t_dpd_dprt = dprt;
			  t_dpd_class = dpdclass;
			  t_dpd_condition = dpdcond} ->
			    dependence_add opn opnprt dopn dprt dpdclass
			      dpdcond true)
		    iteratedpds
	  | _ -> () in
	  f opns));
    match joindpds with (* implode *)
    | [] -> ()
    | _ ->
	(match adequation_type with
	| No_Repetition_Flatten ->
	    let dpd = List.hd joindpds in
	    let sopn,dopn,dprt,dpdclass,dpdcond =
	      dpd.t_dpd_sopn,dpd.t_dpd_dopn,dpd.t_dpd_dprt,dpd.t_dpd_class,
	      dpd.t_dpd_condition in
	    let join,jportout,jportin =
	      add_repetition_boundary "Join" sopn sprt dopn dprt in
	    dependence_add sopn sprt join jportin dpdclass dpdcond true;
	    List.iter
	      (fun dpd ->
		dependence_add join jportout dpd.t_dpd_dopn dpd.t_dpd_dprt
		  dpdclass dpdcond true;
		dependence_remove dpd)
	      joindpds
      | _ -> 
	  let defname = "Implode_"^sprt.t_prt_typename in
	  (try
	    Algorithm.algorithm_create "" defname Internal ["dim"] [] [] []
	      Coord.No_coord2d "" [LoopSeq]
	  with Failure _ -> ());
	  let opnname =
	    "Implode_"^(name_of_operation operation)^"_"^portname in
	  let opnname =
	    unique_identifier graph (parentpath_of_operation operation) opnname in
	  let opnpath =
	    (parentpath_of_operation operation)@[opnname] in
	  let originpath = path_of_origin operation in
	  let xsc = get_xsc originpath opnxsc originpath AttachImplode in
	  let portoutdim = sprt.t_prt_dim * nvalue in
	  let lib,name = deflibname operation in
	  let context = Algorithm.context_global () in
	  let portout =
	    {t_prt_name = portname;
	     t_prt_dir = Port.Out;
	     t_prt_typename = sprt.t_prt_typename;
	     t_prt_dim = portoutdim;
	     t_prt_class = Port.Data_Port;
	     t_prt_order = 0} in
	  let opn =
	    new_opn opnpath (Calcul (Internal,"",defname))
	      [Symbolic.Float (float_of_int sprt.t_prt_dim)]
	      ([portout]@(precedence_ports ())) [] [] operation.t_opn_condition
	      None 0 0. true 
	      (Implode (path_of_origin operation)) None xsc "" "" [LoopSeq] in
	  Hashtbl.add graph (identifier_of_operation opn) opn;
	  copy_precedences operation opn;
	  copy_conditioning_dependences operation opn (-1);
	  ignore(
	  List.fold_left
	    (fun i o ->
	      let portinname =
		portname^"_"^(string_of_int i) in
	      let portin =
		{t_prt_name = portinname;
		 t_prt_dir = Port.In;
		 t_prt_typename = sprt.t_prt_typename;
		 t_prt_dim = sprt.t_prt_dim;
		 t_prt_class = Port.Data_Port;
		 t_prt_order = i+1} in
	      opn.t_opn_ports <- opn.t_opn_ports@[portin];
	      let oprt =
		List.find
		  (fun {t_prt_name=n;t_prt_dir=d} -> n=portname && d=Port.Out)
		  o.t_opn_ports in
	      dependence_add o oprt opn portin (Data,Strong_Precedence_Data)
		operation.t_opn_condition true;
	      i+1)
	    0 opns);
	  List.iter
	    (fun {t_dpd_dopn = dopn;
		  t_dpd_dprt = dprt;
		  t_dpd_class = dpdclass;
		  t_dpd_condition = dpdcond} ->
		    dependence_add opn portout dopn dprt dpdclass dpdcond true)
	    joindpds) in

  (*ps"expand in";*)
  let dpds_in =
    List.filter
      (fun dpd -> not (is_precedence_port dpd.t_dpd_sprt))
      operation.t_opn_dependences_predecessors in
  List.iter expand_dependence_in dpds_in;
  (*ps"expand out";*)
  let portsout =
    List.filter
      (fun ({t_prt_dir=d} as p) ->
	d = Port.Out && (not (is_precedence_port p)))
      operation.t_opn_ports in
  List.iter expand_port_out portsout;
  (*ps"expand ok";*)
  match adequation_type with
  | No_Repetition_Flatten -> ()
  | _ -> operation_remove graph operation
	
(** Initializes a new graph containing (alglib/algname) algorithm graph *)
let initialize_graph alglib algname =
  let graph,_ =
    initialize [alglib;algname] [alglib;algname] alglib algname "" in
    graph

(** Checks that all ports of operations are connected to at least one port.
  Raises failure if an input port is not connected with any output port.
  Returns a list of warnings for the output ports. *)
let check_operation_ports operation =
  List.fold_left
    (fun warnings prt ->
      match prt.t_prt_class <> Port.Precedence_Port with
      | true -> (match prt.t_prt_dir with
	| Port.In ->
	    (match List.exists
		(fun {t_dpd_dprt=p} ->
		  p.t_prt_name = prt.t_prt_name)
		operation.t_opn_dependences_predecessors with
	    | true -> warnings
	    | false ->
		failwith (Algorithm.string_of_port_ref
			    ((identifier_of_operation operation),prt.t_prt_name)^
			  " input port is not in dependence with any output port"))
	| Port.Out ->
	    (match List.exists
		(fun {t_dpd_sprt=p} -> p.t_prt_name = prt.t_prt_name)
		operation.t_opn_dependences_successors with
	    | true -> warnings
	    | false ->
		let warning =
		  Algorithm.string_of_port_ref
		    ((identifier_of_operation operation),prt.t_prt_name)^
		  " output port is not in dependence with any input port" in
		warning::warnings))
      | false -> warnings)
    [] operation.t_opn_ports
    
(** Checks the consistancy of the transformed graph.
  This consists in checking that all ports are connected
   to at least one port. *)
let consistancy_check graph =
  Hashtbl.fold
    (fun _ operation warnings -> warnings@(check_operation_ports operation))
    graph []
  
(** Performs the transformation of the algorithm graph algolib/algoname,
in different ways depending on adequation_type. *)
let transform adequation_type algolib algoname =
  (* Notice that memories can't be factorized. This wouldn't make much sense*)
  let factorized_operations graph =
    let rec f opnlist processed = 
      match List.filter
	  (fun o -> not (is_memory o))
	  opnlist with
      | [] -> []
      | opnlist_without_memory ->
	  match List.filter factorized opnlist_without_memory with
	  | [] ->
	      let processed =
		processed@opnlist_without_memory in
	      let succs =
		union (List.map successors opnlist_without_memory) in
	      let succs =
		List.filter
		  (fun s ->
		    List.for_all 
		      (fun p -> (List.mem p processed) || (is_memory p))
		      (predecessors s))
		  succs in
	      f succs processed
	  | l -> let s =
	      List.fold_left
		(fun s opn ->
		  s^"\n"^(identifier_of_operation opn))
		"Factorized operations are :" l in
	    debug_ps 2 debug_level s;
	    l in
    f ((input_operations graph)@
       (hashtbl_filter
	  (fun opn ->
	    List.for_all is_memory (predecessors opn))
	  graph)) [] in

    (* We used to process conditioned in an ordered way but this can't
    be done when there are " false cycles" between conditioned
    operations *)
  let conditioned_operations graph =
    let l =
      hashtbl_filter (conditioned) graph in
    let s =
      List.fold_left
	(fun s opn -> s^"\n"^(identifier_of_operation opn))
	"Conditioned operations are :" l in
      debug_ps 2 debug_level s;
      l in
  let nonatomic_operations graph = 
    let l =
      hashtbl_filter (non_atomic) graph in
    let s =
      List.fold_left
	(fun s opn -> s^"\n"^(identifier_of_operation opn))
	"Non-atomic operations are :" l in
      debug_ps 2 debug_level s;
      l in
  let nonatomic_nonconditioned_operations graph =
    let l =
      List.filter
	(non_conditioned)
	(nonatomic_operations graph) in
    let s =
      List.fold_left
	(fun s opn -> s^"\n"^(identifier_of_operation opn))
	"Non-atomic non-conditionned operations are :" l in
      debug_ps 2 debug_level s;
      l in
    
  let graph = initialize_graph algolib algoname in
    
  let nc = ref 0 in
    
    (* Process graph *)
    (match adequation_type with
     | Flatten | No_Repetition_Flatten ->
	 while ((factorized_operations graph) <> []) ||
	 ((nonatomic_operations graph) <> []) ||
	 ((conditioned_operations graph) <> []) do
	   (* Process factorized or non atomic operations *)
	   while ((factorized_operations graph) <> []) ||
	   ((nonatomic_nonconditioned_operations graph) <> []) do
             (* Process factorized operations *)
	     while (factorized_operations graph) <> [] do
	       List.iter
		 (fun opn -> 
		   let nvalue = repetition_factor opn in
		   expand graph nvalue adequation_type opn)
		 (factorized_operations graph);
	     done;
	     debug_ps 1 debug_level "Graph defactorized";
             (* Process non atomics and non conditioned operations *)
	     List.iter
	       (dehierarchize graph)
	       (nonatomic_nonconditioned_operations graph)
	   done;
 	   debug_ps 1 debug_level "Graph dehierarchized";
           (* Process conditioned operations *)
 	   List.iter
	     (decondition graph !nc)
	     (conditioned_operations graph);
(* (match ((factorized_operations graph) = []) &&
   (conditioned_operations graph = []) && (nonatomic_operations graph <>[]) &&
   (nonatomic_nonconditioned_operations graph = []) with
   | true -> failwith "The algorithm graph contains a cycle"
   | false -> ());*)
	   nc := !nc + 1;
	 done;
	 debug_ps 1 debug_level "Graph deconditionned. Transformation complete"
     | No_Flatten ->
	 (* Process factorized operations *)
	 while (factorized_operations graph) <> [] do
	   List.iter
	     (fun opn ->
	       let nvalue = repetition_factor opn in
	       expand graph nvalue adequation_type opn)
	     (factorized_operations graph);
	 done);
  graph
