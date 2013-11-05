(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                          Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(** This module contains basis function to access/modify the data structures concerning the algorithm part *)

open Types

let precedence_in_name = "precedence_in"
and precedence_out_name = "precedence_out"

(** Returns the string corresponding to the reference path p. *)
let string_of_path p = string_of_string_list (""::p@[""]) "/"

(** Returns the string corresponding to the port p of reference r. r can be empty if p is a port of the current definition. *)
let string_of_port_ref (r,p) = match r with
	| "" -> p
	| _ -> r^"."^p

let port_content port =
  let name = Port.port_name port and
      dir = Port.port_direction port and
      typename, dim = Port.port_type port and
      port_class = Port.port_class port and
      order = Port.port_order port and
      pos = Port.port_position port in
  name, dir, typename, dim, port_class, order, pos

(** Returns the value of each field of port named portname with direction portdir of definition deflib/defname *)
let portname_content deflib defname portname portdir =
  let algo = Application.algorithmdef deflib defname in
  port_content (Port.find_port portname portdir algo.algo_ports)

(** Returns a new condition on variable varname, with value varvalue. *)
let condition_create (varname,varvalue) =
  match varname,varvalue with
    | "",0 -> Boolean false
    | "",1 -> Boolean true
    | _ -> Condition (varname,varvalue)

(** Returns the value of each field of condition c *)
let condition_content c =
  match c with
    | Boolean false -> "",0
    | Boolean true -> "",1
    | Condition (varname,varvalue) -> varname,varvalue

(** Returns the value of each field of reference ref *)
let reference_content ref =
  (ref.ref_name,ref.ref_algorithm.algo_library,ref.ref_algorithm.algo_name,ref.ref_arguments_values,(condition_content ref.ref_condition),ref.ref_position,ref.ref_repetition,ref.ref_description)

(** Returns the value of each field of reference named refname in definition deflib defname *)
let referencename_content deflib defname refname =
  let alg = Application.algorithmdef deflib defname in
  let ref = List.find (function {ref_name=n} -> n=refname) alg.algo_references in
    reference_content ref

(** Returns the value of each field of dependence d *)
let dependence_content d =
  let conv p = match p with
    | Port p -> "", (Port.port_name p)
    | Ref (r,p) -> r.ref_name, (Port.port_name p) in
    (conv d.dpd_source,conv d.dpd_destination,d.dpd_class,(condition_content d.dpd_condition))

(** Returns the value of each field of definition def *)
let algorithm_content def =
  let ports = List.map port_content def.algo_ports in
  let refs = List.map reference_content def.algo_references in
  let dpds = List.map dependence_content def.algo_dependences in
    (def.algo_library,def.algo_name,def.algo_class,def.algo_arguments_names,ports,refs,dpds,
     def.algo_dimension_window,def.algo_description,def.algo_code_phases)

(** Returns the value of each field of definition deflib/defname *)
let algorithmname_content deflib defname =
  let alg = Application.algorithmdef deflib defname in
    algorithm_content alg

(** Returns the value of each field of deflib/defname when the condition is cond. *)
let algorithm_cond_content deflib defname cond =
  let (alib,aname,atype,args,ports,refs,dpds,dim,desc,code_phases) = algorithmname_content deflib defname in
  let references = List.filter (function (_,_,_,_,c,_,_,_) -> c=cond) refs
  and dependences = List.filter (function (_,_,_,c) -> c=cond) dpds in
    (alib,aname,atype,args,ports,references,dependences,dim,desc,code_phases)

(** Returns true if reference refname in definition deflib defname, references a definition of type memory. *)
let is_memory_reference deflib defname refname =
  let (_,rdeflib,rdefname,_,_,_,_,_) = referencename_content deflib defname refname in
  let (_,_,algoclass,_,_,_,_,_,_,_) = algorithmname_content rdeflib rdefname in
    match algoclass with
      | Memory _ -> true
      | _ -> false

(** Moves port named portname with direction portdir of definition deflib/defname, taking (dx,dy) as offset. *)
let port_move deflib defname portname portdir (dx,dy) =
  let alg = Application.algorithmdef deflib defname in
  let port = Port.find_port portname portdir alg.algo_ports in
  let x,y = Coord.pos_of_coord2d (Port.port_position port) in
    Port.set_port_position port (Coord.Coord2d (x+dx,y+dy))

(** Moves reference named refname of definition deflib/defname, taking (dx,dy) as offset. *)
let ref_move deflib defname refname (dx,dy) =
  let alg = Application.algorithmdef deflib defname in
  let ref = List.find (function {ref_name=n} -> n=refname) alg.algo_references in
  let x,y = Coord.pos_of_coord2d ref.ref_position in
    ref.ref_position <- Coord.Coord2d (x+dx,y+dy)

(** Returns a new reference with given field values *)
let reference_create (name,alglib,algname,args,cond,pos,rep,description) =
  let algo = Application.algorithmdef alglib algname in
  {
   ref_name = name;
   ref_algorithm = algo;
   ref_arguments_values = args;
   ref_condition = cond;
   ref_position = pos;
   ref_repetition = rep;
   ref_description = description
 }

(** Returns true if definition deflib defname if an atomic definition (no refs, no dpds) *)
let atomic deflib defname =
  let (_,_,_,_,_,refs,dpds,_,_,_) = algorithmname_content deflib defname in
    refs=[] & dpds=[]

(** Returns the full hierarchy name of variables transmitted through parameters of reference of path path. *)
let globalvariablenames_of_localvariablenames path =
  let rec f path rest alg =
   let pathstring = string_of_path path in
    let alglist = List.map (function n -> n,pathstring^n) alg.algo_arguments_names in
    let restlist = match rest with
      | [] -> []
      | rname::tl ->
	  match List.exists (function {ref_name=n} -> n=rname) alg.algo_references with
	    | true ->
		let r = List.find (function {ref_name=n} -> n=rname) alg.algo_references in
		  f (path@[rname]) tl r.ref_algorithm
	    | false -> [] in
      restlist@alglist in
  let alglib,algname,rest = match path with
    | l::n::p -> l,n,p
    | _ -> "","",[] in
  let alg = Application.algorithmdef alglib algname in
    f [alglib;algname] rest alg

(** Returns the expression expr in which variables names have been substituted by their corresponding full hierarchy name *)
let globalexpression_of_localexpression path expr =
  let varlist = globalvariablenames_of_localvariablenames path in
    Symbolic.renamevariables expr varlist

(** Returns true if there is a path from srcrefname to dstrefname in definition deflib defname *)
let exists_path deflib defname srcrefname dstrefname = 
  match srcrefname,dstrefname with 
    | "",_ | _,"" -> false (* No possible cycle if connecting a local port *)
    | _ ->
	match srcrefname = dstrefname with
	  | true -> true
	  | false ->
	      (match is_memory_reference deflib defname srcrefname with 
		 | true -> false
		 | false -> 
		     let (_,_,_,_,_,_,dpds,_,_,_) = algorithmname_content deflib defname in
		       (* Looking for a dependence which source is srefname *)
		       (* The port doesn't matter, we only consider the reference *)
		     let find_succs srefname =
		       let ds = List.filter (function ((srefname2,_),(drefname2,_),_,_) -> 
					       (srefname2 = srefname) && drefname2 <> "" &&
					       not (is_memory_reference deflib defname drefname2)) dpds in
			 List.map (function (_,(drefname,_),_,_) ->drefname) ds in
		       (* Looking for a path from sname to srcrefname *)
		     let rec path_from srcs = match srcs with
		       | [] -> false
		       | _ ->
			   let dsts = List.concat (List.map find_succs srcs) in
			     match (List.mem dstrefname dsts) with
			       | true -> true
			       | false -> path_from dsts in		
		       path_from [srcrefname])

(** Returns the list associating each parameter used in any algorithm to its expression, resolving hierarchy. *)
let context_global () =
  let rec combine path pathvar n v =
    match n,v with
    | n::tn,v::tv ->
	let v = globalexpression_of_localexpression path v in
	  (pathvar^n,v)::(combine path pathvar tn tv)
    | n::tn,[] -> (n,Symbolic.Var "")::(combine path pathvar tn [])
    | [],_ -> [] in
  let rec f alg path =
    let refcontext = List.map (function r ->
				 match r.ref_algorithm.algo_arguments_names with
				 | [] -> []
				 | _ ->
				     let pathvar = string_of_path (path@[r.ref_name]) in
				       combine path pathvar r.ref_algorithm.algo_arguments_names r.ref_arguments_values) alg.algo_references in
    let subrefcontext = List.fold_left (function l -> function r ->
					  match atomic r.ref_algorithm.algo_library r.ref_algorithm.algo_name with
					  | true -> l
					  | false -> l@(f r.ref_algorithm (path@[r.ref_name]))) [] alg.algo_references in
      subrefcontext@(List.concat refcontext) in
    
    match Application.algo_main_get () with
    | "","",_ -> []
    | alglib,algname,mainvalues ->
	let alg = Application.algorithmdef alglib algname in
	let path = [alglib;algname] in
	let maincontext = combine path (string_of_path path) alg.algo_arguments_names mainvalues
	and submaincontext = f alg path in
	  maincontext@submaincontext
	    
(** Returns a boolean, true if creating a dependence between src and dst (refs and ports) is allowed plus, if false, the message justifying unconnectability. *)
let connectable deflib defname defcond ((srcrefname,srcport),(dstrefname,dstport)) loading = 
  let get_port_type refname portname =
    let (rdeflib,rdefname) = match refname with
    | "" -> (deflib,defname)
    | _ ->
	let (_,rdeflib,rdefname,_,_,_,_,_) = referencename_content deflib defname refname in
	(rdeflib,rdefname) in
    let (_,_,_,_,ports,_,_,_,_,_) = algorithmname_content rdeflib rdefname in
    let (_,_,prttype,prtdim,prtclass,_,_) = List.find (function (pname,_,_,_,_,_,_) -> pname = portname) ports in
      (prttype,prtdim,prtclass) in
    (* Forbid dependence on precedence ports *)
  let srctype,srcdim,srcportclass = get_port_type srcrefname srcport
  and dsttype,dstdim,dstportclass = get_port_type dstrefname dstport in
    match ((srcportclass = Port.Precedence_Port) && (dstportclass <> Port.Precedence_Port)) || ((srcportclass <> Port.Precedence_Port) && (dstportclass = Port.Precedence_Port)) with
  | true -> (false,"Forbidden to connect precedence port with data port.")
  | false ->
      (* Forbid two dependences on the same input port of the same condition *)
      let (_,_,_,_,_,_,dpds,_,_,_) = algorithmname_content deflib defname in
      let already_connected = (dstportclass <> Port.Precedence_Port) && (List.exists (function (_,(dstrefname2,dstport2),_,(_,condval)) -> dstrefname2 = dstrefname && dstport2 = dstport && defcond=condval) dpds) in
      match already_connected with
      | true ->
	  let refprintname = match dstrefname with
	    | "" -> "current definition"
	    | other -> other in
	  (false,("Input port "^dstport^" of "^refprintname^" already connected to another output port."))
      | false ->
          (* Forbid dependences between ports of different types. *)
	  match (srctype=dsttype) with
	  | false -> (false,"No dependence between ports of different types.")
	  | true -> (*let compatible_factors = match loading with
	    | true -> true
	    | false -> let context = context_global () in
	    let compatible_src =
	      match srcrefname <> "" && srcportclass <> Port.Precedence_Port with
	      | true -> let (_,_,_,_,_,_,repetition) = referencename_content deflib defname srcrefname in
		  compatible_factor deflib defname ((srcrefname,srcport),(dstrefname,dstport)) repetition Out context
	      | false -> true in
	    let compatible_dst =
	      match dstrefname <> "" && dstportclass <> Port.Precedence_Port with
	      | true -> let (_,_,_,_,_,_,repetition) = referencename_content deflib defname dstrefname in
		  compatible_factor deflib defname ((srcrefname,srcport),(dstrefname,dstport)) repetition In context
	      | false -> true in
	      match compatible_src && compatible_dst with
	      | false -> (false,"Incompatible repetition factors")
	      | true -> *)match loading with
		| false -> (true,"")
		| true ->
		    (* Looking for cycles : the dependence will create a cycle if there's already a path from dstrefname to srcrefname*)
		    match (exists_path deflib defname dstrefname srcrefname) with
		    | true -> (false,"This dependence would create a cycle.")
		    | false -> (true,"")	  
			
(** Returns a new dependence with given field values *)
let dependence_create (s,d,c,cond) ports refs =
  let invertdir dir = match dir with
  | Port.In -> Port.Out
  | Port.Out -> Port.In in
  let portref_conv (rname,pname) pdir =
    match rname with
    | "" -> Port (Port.find_port pname pdir ports)
    | _ ->
	let ref = List.find (function {ref_name=n} -> n=rname) refs in
	Ref (ref, (Port.find_port pname (invertdir pdir) ref.ref_algorithm.algo_ports)) in
  {
   dpd_source = portref_conv s Port.In;
   dpd_destination = portref_conv d Port.Out;
   dpd_class = c;
   dpd_condition = cond;
 }

(** Adds a new port to definition deflib/defname.
  Raises Failure if pname with pdir as direction already exists in this definition *)
let port_add deflib defname ((name,dir,typename,dim,port_class,_,pos)) =
  let alg = Application.algorithmdef deflib defname in
  try
    ignore (Port.find_port name dir alg.algo_ports);
    failwith ("Port "^name^" defined twice (with the same direction) : ignoring second definition.")
  with
    Not_found ->
      let ports = Port.ports_order_sort alg.algo_ports in
      let order = (list_max_value Port.port_order alg.algo_ports 0) + 1 in
      let port =
        Port.create name dir typename dim
          (match alg.algo_class, port_class with
          | (Memory _), Port.Data_Port -> Port.Delay_Port
          | _ -> port_class)
          order pos in
      alg.algo_ports <- Port.ports_order_sort (alg.algo_ports@[port])

(** Adds a new reference to definition deflib/defname.
  Raise Failure if refname already exists in this definition. *)
let reference_add deflib defname ((rname,_,_,_,_,_,_,_) as ref) =
  let alg = Application.algorithmdef deflib defname in
    match List.exists (function {ref_name=refname} -> refname = rname) alg.algo_references with
      | true -> failwith ("Reference "^rname^" defined twice : ignoring second definition.")
      | false ->
	  let ref = reference_create ref in
	    alg.algo_references <- alg.algo_references @ [ref]

(** Returns the list of definitions (library,name) in the application *)
let algo_list () = List.map (function {algo_name=name;algo_library=lib} -> (lib,name)) Application.application.app_algorithms

(** Returns the list of definitions (deflib,defname) referenced in main definition and its hierarchy *)
let referenced_definitions_list () =
  let rec referenced_definitions (deflib,defname) =
    let (_,_,_,_,_,refs,_,_,_,_) = algorithmname_content deflib defname in
    let toplevel_defs = List.map (fun (_,rdeflib,rdefname,_,_,_,_,_) -> rdeflib,rdefname) refs in
      toplevel_defs@(union (List.map referenced_definitions toplevel_defs)) in
      
  let mainlib,mainname,_ = Application.algo_main_get () in
    match mainlib,mainname with
    | "","" -> failwith "No main algorithm defined"
    | _ -> referenced_definitions (mainlib,mainname)

(** Returns the list of port types used in main definition *)
let port_types_list () =
  let def_port_types (deflib,defname) =
    let (_,_,_,_,ports,_,_,_,_,_) = algorithmname_content deflib defname in
    let ports = List.filter (fun (_,_,_,_,prt_class,_,_) -> prt_class <> Port.Precedence_Port ) ports in
      List.map (fun (_,_,prt_typename,_,_,_,_) -> prt_typename) ports in
    union (List.map def_port_types (referenced_definitions_list ()))
      
(** Adds a new dependence to definition deflib defname.
  Raise Failure if the predicate connectable is false for this dependence. *)
let dependence_add alglib algname (((sref,sport) as s),((dref,dport) as d),dpd_class,cond) loading =
  let dependence_title = "Dependence from "^(string_of_port_ref s)^" to "^(string_of_port_ref d) in
  let alg = Application.algorithmdef alglib algname in
  let check_ref rname =
    match (rname = "") || (List.exists (fun r -> r.ref_name=rname) alg.algo_references) with
    | true -> ()
    | false -> failwith ("Can't create "^dependence_title^" : \n Definition "^alglib^"/"^algname^" has no reference "^rname) in
  let check_port rname pname =
    let refDef = 
    match rname = "" with
    | true -> Application.algorithmdef alglib algname
    | false -> let r = List.find (fun r -> r.ref_name=rname) alg.algo_references in
	r.ref_algorithm in
      match ((List.exists (fun p -> (Port.port_name p) = pname)) refDef.algo_ports) || (pname = "") with
      | true -> ()
      | false -> failwith ("Can't create "^dependence_title^" : \n There is no port "^(string_of_port_ref (rname,pname))) in
    check_ref sref;
    check_ref dref;
    check_port sref sport;
    check_port dref dport;
  let (_,condval) = condition_content cond in
  let sport,dport = match dpd_class with
  | Precedence -> precedence_out_name,precedence_in_name
  | _ -> sport,dport in
  let s,d = ((sref,sport),(dref,dport)) in (* Must update s and d if sport and dport have been updated *)
  match connectable alglib algname condval (s,d) loading with 
  | false,msg ->
      failwith ("Can't create "^dependence_title^" : \n"^msg)
  | true,_ ->
      let dpd = dependence_create (s,d,dpd_class,cond) alg.algo_ports alg.algo_references in
	  alg.algo_dependences <- alg.algo_dependences@[dpd]
	    
(** Returns a new algorithm definition with given field values. Creates included refs, ports and dpds.
   Raise Failure if some elements can't be created (however creating the others)*)
let algorithm_create lib name algtype args ports refs dpds dim desc code_phases =
  let errors = ref "" in 
  match List.exists (function alg -> alg.algo_name = name && alg.algo_library = lib) Application.application.app_algorithms with
  | true -> failwith ("Definition "^name^" defined twice : ignoring second definition.")
  | false ->
      (* The algorithm needs to be created before adding ports, dependences, references for error checking *)
      let algo = {
	algo_name = name;
	algo_library = lib;
	algo_class = algtype;
	algo_arguments_names = args;
	algo_ports = [];
	algo_references = [];
	algo_dependences = [];
	algo_dimension_window = dim;
	algo_description = desc;
	algo_code_phases = code_phases;
      } in
      Application.application.app_algorithms <- Application.application.app_algorithms@[algo];
      (* Adding ports *)
      let precedence_in = (precedence_in_name,Port.In,"prec_synchro",Symbolic.Float 1.,Port.Precedence_Port,0,Coord.No_coord2d)
      and precedence_out = (precedence_out_name,Port.Out,"prec_synchro",Symbolic.Float 1.,Port.Precedence_Port,0,Coord.No_coord2d) in
      List.iter (function p -> try 
	port_add lib name p
      with (Failure msg) -> errors := !errors^"\n"^msg) (precedence_in::precedence_out::ports);
      (* Adding references *)
	List.iter (function r -> try
          reference_add lib name r;
	with (Failure msg) -> errors := !errors^"\n"^msg) refs;
      (* Adding dependences *)
      List.iter (function d -> try 
	dependence_add lib name d false
      with (Failure msg) -> errors := !errors^"\n"^msg) dpds;

      match !errors with
      | "" -> ()
      | some -> failwith some
	    
(** Returns a new constant definition with given field values.
  Raise Failure if some elements can't be created (however creating the others)*)
let constant_create lib name args ports dim desc =
  algorithm_create lib name Constant args ports [] [] dim desc [InitSeq]

(** Returns a new sensor definition with given field values.
  Raise Failure if some elements can't be created (however creating the others)*)
let sensor_create lib name args ports dim desc =
  algorithm_create lib name Sensor args ports [] [] dim desc [InitSeq;LoopSeq;EndSeq]

(** Returns a new actuator definition with given field values.
  Raise Failure if some elements can't be created (however creating the others)*)
let actuator_create lib name args ports dim desc =
  algorithm_create lib name Actuator args ports [] [] dim desc [InitSeq;LoopSeq;EndSeq]

(** Returns a new memory definition with given field values.
  Raise Failure if some elements can't be created (however creating the others)*)
let memory_create lib name args ports dim desc ab =
  (*match List.length args < 1 with
  | true -> failwith "Delays must have at least one argument for the delay range. It must be last in arguments list."
  | false -> *)algorithm_create lib name (Memory ab) args ports [] [] dim desc [InitSeq;LoopSeq;EndSeq]

(** Returns a new operation definition with given field values.
  Raise Failure if some elements can't be created (however creating the others)*)
let operation_create lib name args ports conds_refs_dpds_list dim desc code_phases = 
  let refs = List.concat (List.map (function (cond,refs,_) ->
    let condition = condition_create cond in
    List.map (function (name,(alglib,algname),args,pos,rep,descr) ->
		let rep = match rep > 1 with
		  | true -> Specified rep
		  | false -> Calculated (Repeat 1) in
      (name,alglib,algname,args,condition,pos,rep,descr)) refs) conds_refs_dpds_list) in
  let dpds = List.concat (List.map (function (cond,_,dpds) ->
				      let condition = condition_create cond in
					List.map (function (s,d,c) -> (s,d,c,condition)) dpds) conds_refs_dpds_list) in
    algorithm_create lib name Operation args ports refs dpds dim desc code_phases

(** Returns the condition list, where one condition is (Condition (variable,value)), of definition deflib/defname *)
let cond_list alglib algname =
  let alg = Application.algorithmdef alglib algname in
  let cond_list = 
    (List.map (function r -> r.ref_condition) alg.algo_references) @
    (List.map (function d -> d.dpd_condition) alg.algo_dependences) in
  let conds = remove_copies cond_list in
  let conditions = List.filter (function c -> match c with
				  | Condition _ -> true
				  | _ -> false) conds in
    match conditions with
      | [] -> "",[]
      | (Condition (variable,_))::_ -> (variable, (List.sort compare (List.map (function c -> match c with
										  | Condition (_,v) -> v
										  | _ -> 0) conditions)))
      | _ -> "",[]

(** Sorts the ports of definition deflib defname, according to prt_order. *)
let ports_order_sort_libname deflib defname =
  let alg = Application.algorithmdef deflib defname in
    alg.algo_ports <- Port.ports_order_sort alg.algo_ports

(** Removes dependence with source sref and destination dref,dprt and condition cond from definition deflib defname *)
let dependence_delete deflib defname dref dprt sref cond =
  let alg = Application.algorithmdef deflib defname in
  let condition = condition_create cond in
    alg.algo_dependences <- List.filter (function {dpd_destination=d;dpd_condition=c} ->
					   not ((c=condition) && match d with
						  | Port p -> dref = "" && dprt = (Port.port_name p)
						  | Ref ({ref_name=nr}, port) -> dref = nr && dprt = (Port.port_name port))) alg.algo_dependences

(** Removes port named portname from definition deflib/defname. Also removes dependences for which:
  - dependences in this definition for which source or destination is this port
  - source or destination is a reference on this definition and port is portname. *)
let port_delete deflib defname portname =
  let alg = Application.algorithmdef deflib defname in
  let algs = List.filter (function d -> List.exists (function {ref_algorithm=a} -> a=alg) d.algo_references) Application.application.app_algorithms in
  let not_dpd_port pr = match pr with
    | Port p -> (Port.port_name p) <> portname
    | _ -> true in
    alg.algo_dependences <- List.filter (function {dpd_source=s;dpd_destination=d} -> not_dpd_port s && not_dpd_port d) alg.algo_dependences;
    let not_dpd_ref pr = match pr with
      | Ref ({ref_algorithm=a}, port) -> a<>alg || (Port.port_name port) <>portname
      | _ -> true in
      List.iter (function d -> 
		   d.algo_dependences <- List.filter (function {dpd_source=s;dpd_destination=d} -> not_dpd_ref s && not_dpd_ref d) d.algo_dependences) algs;
      alg.algo_ports <- Port.ports_order_sort (List.filter (function port -> (Port.port_name port) <>portname) alg.algo_ports)

(** Removes reference named refname in definition deflib defname.
  Also removes dependences on this definition. *)
let ref_delete deflib defname refname =
  let alg = Application.algorithmdef deflib defname in
  let not_dpd_ref pr = match pr with
    | Ref ({ref_name=n},_) -> n<>refname
    | _ -> true in
    alg.algo_dependences <- List.filter (function {dpd_source=s;dpd_destination=d} -> not_dpd_ref s && not_dpd_ref d) alg.algo_dependences;
    alg.algo_references <- List.filter (function {ref_name=n} -> n<>refname) alg.algo_references

(** Removes definition deflib defname from application definitions list *)
let algo_delete deflib defname =
  List.iter (function algo ->
	       List.iter (function r -> if r.ref_algorithm.algo_name=defname && r.ref_algorithm.algo_library=deflib then ref_delete algo.algo_library algo.algo_name r.ref_name) algo.algo_references) Application.application.app_algorithms;
  Application.application.app_algorithms <- List.filter (function d -> d.algo_library<>deflib || d.algo_name<>defname) Application.application.app_algorithms;
  (match Application.algo_main_get () with
     | l,n,_ when l=deflib && n=defname -> Application.algo_main_clear ()
     | _ -> ())

(** Sets definition deflib/defname as main algorithm with arguments args. *)
let algo_main_set (deflib,defname) args =
  let algo = Application.algorithmdef deflib defname in
  let args = match (List.length args) = (List.length algo.algo_arguments_names) with
    | true -> args
    | false -> List.map (function _ -> Symbolic.Float 0.) algo.algo_arguments_names in
    Application.algo_main_set algo args

(** Returns true if definition deflib defname is the main algorithm *)
let algo_is_main deflib defname =
  match Application.algo_main_get () with
    | l,a,_ when l=deflib && a=defname -> true
    | _ -> false

(** Returns true if definition deflib defname is conditionned *)
let conditioned deflib defname =
  cond_list deflib defname <> ("",[])

(** Resizes definition deflib/defname window to size a b. *)
let algo_dimension_window_change deflib defname a b =
  let alg = Application.algorithmdef deflib defname in
    alg.algo_dimension_window <- Coord.Coord2d(a,b)

(** Changes definition deflib/defname to deflib/newname.
  Raises failure if definition with newname already exists *)
let definition_modify_name deflib defname newname =
  match (defname <> newname) && (List.exists (function alg -> alg.algo_name = newname && alg.algo_library = deflib) Application.application.app_algorithms) with
  | true -> failwith ("Definition "^newname^" defined twice : ignoring second definition.")
  | false ->
      let algo = Application.algorithmdef deflib defname in
	algo.algo_name <- newname (* There's no need to update the references as their structure includes the whole definition *)

(** Changes description of definition deflib/defname to description *)
let definition_modify_description deflib defname description =
      let algo = Application.algorithmdef deflib defname in
	algo.algo_description <- description

(** Changes code_phases of definition deflib/defname to code_phases *)
let definition_modify_code_phases deflib defname code_phases =
  let algo = Application.algorithmdef deflib defname in
    algo.algo_code_phases <- code_phases

(** Changes refence refname, in definition deflib defname, name to newname and args to newargs.
  Raises failure if name has changed and reference with newname already exists *)
let reference_modify deflib defname refname newname newargs =
  let alg = Application.algorithmdef deflib defname in
    (match refname<>newname && List.exists (function {ref_name=refname} -> refname = newname) alg.algo_references with
    | true -> failwith ("Reference "^newname^" defined twice : ignoring second definition.")
    | false -> ());
    let ref = List.find (function {ref_name=n} -> n=refname) alg.algo_references in
      ref.ref_name <- newname;
      ref.ref_arguments_values <- newargs

let reference_modify_description deflib defname refname description =
  let alg = Application.algorithmdef deflib defname in
  let ref = List.find (function {ref_name=n} -> n=refname) alg.algo_references in
    ref.ref_description <- description

(** Changes refence refname, in definition deflib/defname, repetition factor to rep *)
let repetition_factor_modify deflib defname refname rep =
  let alg = Application.algorithmdef deflib defname in
  let ref = List.find (function {ref_name=n} -> n=refname) alg.algo_references in
    ref.ref_repetition <- rep

(** Modifies port named portname, with direction portdir in definition deflib/defname, given field values.
 Raise failure if port with newname already exists *)
let port_modify deflib defname portname portdir newname newtypename newdim order =
  let alg = Application.algorithmdef deflib defname in
    match (portname <> newname) && (List.exists (function p -> (Port.port_name p) = newname && (Port.port_direction p) = portdir) alg.algo_ports) with
      | true -> failwith ("Port "^newname^" defined twice (with the same direction) : ignoring second definition.")
      | false ->
	  let port = Port.find_port portname portdir alg.algo_ports in
	    Port.set_port_name port newname;
	    Port.set_port_type port newtypename newdim;
	    Port.set_port_order port order

(** Changes port portname, with direction portdir in definition deflib defname, class to portclass. *)
let port_modify_class deflib defname portname portdir portclass =
  let alg = Application.algorithmdef deflib defname in
  (match portclass,portdir with
  | Port.Init_Memory_Port, Port.In -> (match alg.algo_class with
    | Memory _ -> (match List.filter (function port -> (Port.port_class port) = Port.Init_Memory_Port) alg.algo_ports with
      | [] -> ()
      | portslist -> List.iter (function p -> Port.set_port_class p Port.Data_Port) portslist)
    | _ -> raise (Failure "This definition isn't of type Memory."))
  | Port.Init_Memory_Port, Port.Out -> raise (Failure "Only an Input port can initialize a Memory.")
  | _ -> ());
  let port = Port.find_port portname portdir alg.algo_ports in
  Port.set_port_class port portclass

(** Sets definition deflib defname arguments names *)
let algo_arguments_names_set deflib defname args =
  let alg = Application.algorithmdef deflib defname in
    alg.algo_arguments_names <- args

(** Sets main algorithm arguments values *)
let algo_main_arguments_values_set args =
  Application.algo_main_arguments_values_set args

(** Returns the references and dependences  of definition deflib/defname when condition is cond *)
let condition deflib defname cond =
  let alg = Application.algorithmdef deflib defname
  and condition = condition_create cond in
    List.iter (function r -> r.ref_condition <- condition) alg.algo_references;
    List.iter (function d -> d.dpd_condition <- condition) alg.algo_dependences

(** Removes the condition cond of definition deflib/defname (references and dependences contained by this condition) *)
let condition_delete deflib defname cond =
  let alg = Application.algorithmdef deflib defname
  and condition = condition_create cond in
    alg.algo_references <- List.filter (function r -> r.ref_condition <> condition) alg.algo_references;
    alg.algo_dependences <- List.filter (function d -> d.dpd_condition <> condition) alg.algo_dependences

(** Returns the string corresponding to repetition rep *)
let string_of_repetition rep =
  match rep with
  | Calculated (Repeat n) -> string_of_int n
  | Specified n -> string_of_int n
  | Calculated Error -> "ERROR"
  | Calculated Undefinied -> "?"



