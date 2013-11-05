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

(* This module contains data structures used during the adequation
   and code generation phases.  Those structures have more "direct"
   data accesses and describe only flattened graphs.  We don't
   distinguish algorithm definitions and references but instead use
   only operations.  Operator now also groups "standard calcul"
   operator and medium. *)

open Types




(* Type of data from a communication point of view. *)
type data_type = operation_type*port_type

(* Communication class. It depends on the medium on which it occurs
  and on if it is seen on the send or receive side. *)
and communication_class =
  | Write of data_type * operator_reference_type
  | Read of data_type * operator_reference_type
      (* data, sender, receivers *)
  | Transfer of data_type * operator_reference_type *
      (operator_reference_type list)
  | Send    of data_type * operator_reference_type * (operator_reference_type list)
      (* data, sender, receivers, executor *)
  | Receive of data_type * operator_reference_type * (operator_reference_type list) * operator_reference_type
  (********************************************************)
  (****** RELIABLE COMMUNICATION PRIMITIVEs FOR SAMMP *****)
  | Reliable_Send    of data_type * (operator_reference_type list) * (operator_reference_type list) 
          (*              data,              senders,                          receivers *) 
  | Reliable_Receive of data_type * (operator_reference_type list) * (operator_reference_type list) * operator_reference_type
          (*              data,              senders,                          receivers,                   executor *) 
  | Reliable_Sync of data_type * (operator_reference_type list) *
      (operator_reference_type list) * operator_reference_type
  (********************************************************)
  | Send_Synchro of operator_reference_type * operator_reference_type
  | Receive_Synchro of operator_reference_type * operator_reference_type
      (* data, sender, receivers, executor *)
  | Sync of data_type * operator_reference_type *
      (operator_reference_type list) * operator_reference_type

(* Union of communication and calculs. *)
and operation_class =
  | Calcul of algorithm_class * string * string
  | Communication of communication_class

(* The origin of an operation, ie when it was created. *)
and origin_type = 
  | Ihm of (string list)
  | Condition_In of (string list)
  | Condition_Out of (string list)
  | Explode of (string list)
  | Implode of (string list)
  | Synchro_Constant of (string list)
  | Diffuse of ((string list)*int)
  | Iterate of ((string list)*int)
  | Fork of ((string list)*int)
  | Join of ((string list)*int)
      (* string list is the path to the original operation, int is the
	 repetition factor for factorization boundaries *)

(* Data or conditioning dependence. *)
and edge_type = Condition of int | Data

(* The class and edge type of a dependence. *)
and dependence_class = edge_type * Types.dependence_class

(* Type of conditions used for edges or operations. *)
and condition_type = (data_type option)*(int option)

(* Type of an operation port. *)
and port_type = {
  mutable t_prt_name : string;
  t_prt_dir : Port.direction;
  t_prt_typename : string;
  t_prt_class : Port.port_class;
  (* We need to directly get the result when computing the adequation
     therefore we don't keep the symbolic expression, only its
     result. *)
  mutable t_prt_dim : int;
  t_prt_order : int;
}

(* Type of a dependence. *)
and dependence_type = {
  mutable t_dpd_sopn : operation_type;
  mutable t_dpd_sprt : port_type;
  t_dpd_dopn : operation_type;
  t_dpd_dprt : port_type;
  mutable t_dpd_backups : (dependence_type list) option;
  mutable t_dpd_routing : (operation_type list) option;
  mutable t_dpd_class : dependence_class;
  (* Condition list is (and must be) sorted by "condition level" ie,
     in the hierarchical condition order *)
  mutable t_dpd_condition : condition_type list;
  t_dpd_status : bool;
}

(* Type of an operation. *)
and operation_type = {
  mutable t_opn_path : string list;
  mutable t_opn_class : operation_class;
  t_opn_arguments_values : Symbolic.expression list;
  mutable t_opn_ports : port_type list;
  (* There's a lot of memory duplication with those 4 fields, but this
     enables constant time access to all these data *)
  mutable t_opn_dependences_predecessors : dependence_type list;
  mutable t_opn_dependences_successors : dependence_type list;
  mutable t_opn_successors : operation_type list;
  mutable t_opn_predecessors : operation_type list;
  (* Condition list is (and must be) sorted by "condition level" ie,
     in the hierarchical condition order *)
  mutable t_opn_condition : condition_type list;
  (* operator on which the operation is scheduled *)
  mutable t_opn_operator : operator_class option;
  (* Rank in the final schedule of an operator (used for schedule
     display only). *)
  mutable t_opn_rank : int;
  (* becomes available only when definitely placed on an operator *)
  mutable t_opn_esfs : float;
  (* the worst end execution time in the presence of failures *)
  mutable t_opn_weefs : float;
  (* operation which is predecessor of this one in the schedule order *)
  mutable t_opn_pred_scheduled : operation_type option;
  (* wether the operation has been just temporarily created for esfs
     calculations or definitely *)
  t_opn_status : bool;
  t_opn_origin : origin_type;
  mutable t_opn_xsc_name : string;
  (* definition in which this operation was referenced *)
  t_opn_referencing_alglib : string;
  t_opn_referencing_algname : string;
  (* this is used to keep an order between successive operations of null
     duration *)
  mutable t_opn_adequation_order : int;
  (* phases in which code must be generated *)
  t_opn_code_phases : code_generation_phase list
}

(* Transformed algorithm graph type. *)
and graph_type = (string,operation_type) Hashtbl.t

(* Type of a schedule (for a specific condition) *)
and schedule_type = {
  (* operation * esfs * eefs *)
  mutable t_sch_operations : (operation_type*float*float) list;
  mutable t_sch_last_opn : operation_type option;
  (* intervals for this schedule when no operation is scheduled. *)
  mutable t_sch_gaps : (float*float) list;
  mutable t_sch_gaps_backup : (float*float) list
}

(* Operator schedule type : one schedule for each condition *)
and operator_schedule_type =
    (condition_type list,schedule_type) Hashtbl.t

(* Complete schedule : simple associations between an operator name
  and its schedule *)
and full_schedule_type =
    (string, operator_class * operator_schedule_type) Hashtbl.t

(* Specifies on which operator a data is available for a communication. *)
and available_data_type =
    (data_type,((operator_class*operation_type*port_type*float) list)) Hashtbl.t


(* Returns the short name of operation opn (not the complete path) *)
let name_of_operation opn =
  List.hd (List.rev opn.t_opn_path)

(* Returns the name of operator o. *)
let name_of_operator o = match o with
| Operator opr -> opr.oprref_name
| Media mda -> mda.mdaref_name


(* new operators to compare two ports *)
let (=%) prt1 prt2 = 
  (prt1.t_prt_name = prt2.t_prt_name)

let (<>%) prt1 prt2 = 
  (prt1.t_prt_name <> prt2.t_prt_name)

(* new operators to compare two operations *)
let (=$) opn1 opn2 = 
  ((name_of_operation opn1) = (name_of_operation opn2))

let (<>$) opn1 opn2 = 
  ((name_of_operation opn1) <> (name_of_operation opn2))

(* new operators to compare two operators *)
let (=@) opr1 opr2 = 
  ((name_of_operator opr1) = (name_of_operator opr2))

let (<>@) opr1 opr2 = 
  ((name_of_operator opr1) <> (name_of_operator opr2))

let (=@@) opr1 opr2 = 
  ((name_of_operator (Operator opr1)) = (name_of_operator (Operator opr2)))

let (<>@@) opr1 opr2 = 
  ((name_of_operator (Operator opr1)) <> (name_of_operator (Operator opr2)))







(* Returns a new port with given field values *)
let new_port prt_name prt_dir prt_type_name prt_dim prt_class prt_order =
  {t_prt_name = prt_name;
   t_prt_dir = prt_dir;
   t_prt_typename = prt_type_name;
   t_prt_dim = prt_dim;
   t_prt_class = prt_class;
   t_prt_order = prt_order
 }


(* Returns a copy of port prt *)
let port_copy prt =
  {t_prt_name = prt.t_prt_name;
   t_prt_dir = prt.t_prt_dir;
   t_prt_typename = prt.t_prt_typename;
   t_prt_dim = prt.t_prt_dim;
   t_prt_class = prt.t_prt_class;
   t_prt_order = prt.t_prt_order
 }

(* Adds port with given field values to the port list of operation *)
let port_add prt_name prt_dir prt_type_name prt_dim prt_class prt_order
    operation =
  let port =
    new_port prt_name prt_dir prt_type_name prt_dim prt_class prt_order in
  operation.t_opn_ports <- operation.t_opn_ports@[port]

(* Returns a new dependence with given field values *)
let new_dependence sopn sprt dopn dprt dpdclass cond status =
  {t_dpd_sopn=sopn;
   t_dpd_sprt=sprt;
   t_dpd_dopn=dopn;
   t_dpd_dprt=dprt;
   t_dpd_backups= None;
   t_dpd_routing= None;
   t_dpd_class=dpdclass;
   t_dpd_condition=cond;
   t_dpd_status=status}

(* Adds a dependence between sopn.sprt and dopn.dprt *)
let dependence_add sopn sprt dopn dprt dpdclass cond status =
  let dpd = new_dependence sopn sprt dopn dprt dpdclass cond status in
  sopn.t_opn_dependences_successors <-
    dpd :: sopn.t_opn_dependences_successors;
  sopn.t_opn_successors <-
    dopn::sopn.t_opn_successors;
  dopn.t_opn_dependences_predecessors <-
    dpd :: dopn.t_opn_dependences_predecessors;
  dopn.t_opn_predecessors <-
    sopn::dopn.t_opn_predecessors
				     
(* Removes dependence dpd from its predecessors and successors. *)
let dependence_remove dpd =
  let sopn,dopn = dpd.t_dpd_sopn,dpd.t_dpd_dopn in
  sopn.t_opn_dependences_successors <-
    List.filter
      (fun d -> d!=dpd)
      sopn.t_opn_dependences_successors;
  sopn.t_opn_successors <-
    List.filter
      (fun opn -> opn<>$dopn)
      sopn.t_opn_successors;
  dopn.t_opn_dependences_predecessors <-
    List.filter
      (fun d -> d!=dpd)
      dopn.t_opn_dependences_predecessors;
  dopn.t_opn_predecessors <-
    List.filter
      (fun opn -> opn<>$sopn)
      dopn.t_opn_predecessors
      
(* Returns a new operation with given fields values. *)
let new_opn opn_path opn_class args opn_ports opn_dependences_predecessors
    opn_dependences_successors opn_condition opn_operator rank opn_esfs
    opn_status origin opn_pred xscname alglib algname code_phases =
  {
   t_opn_path = opn_path;
   t_opn_class = opn_class;
   t_opn_arguments_values = args;
   t_opn_ports = opn_ports;
   t_opn_dependences_predecessors = opn_dependences_predecessors;
   t_opn_dependences_successors = opn_dependences_successors;
   t_opn_predecessors =
   List.map (fun {t_dpd_sopn=sopn} -> sopn) opn_dependences_predecessors;
   t_opn_successors =
   List.map (fun {t_dpd_dopn=dopn} -> dopn) opn_dependences_successors;
   t_opn_condition = opn_condition;
   t_opn_operator = opn_operator;
   t_opn_rank = rank;
   t_opn_esfs = opn_esfs;
   t_opn_weefs = max_float ;
   t_opn_status = opn_status;
   t_opn_origin = origin;
   t_opn_pred_scheduled = opn_pred;
   t_opn_xsc_name = xscname;
   t_opn_referencing_alglib = alglib;
   t_opn_referencing_algname = algname;
   t_opn_adequation_order = -1;
   t_opn_code_phases = code_phases
 }



(* Returns the string corresponding to path path. *)
let identifier_of_path path =
  List.fold_left (fun s p -> s^"/"^p) "" path

(* Returns the full identifier of operation opn (path and name) as a string. *)
let identifier_of_operation opn =
  identifier_of_path opn.t_opn_path

(* Returns the identifier corresponding to path/name as a string. *)
let identifier_of_path_name path name = identifier_of_path (path@[name])

(* Returns the parent path of path path. *)
let parentpath_of_path path =
  List.rev (List.tl (List.rev path))

(* Returns *)
let parentname_of_operation opn = 
  List.hd (List.rev (parentpath_of_path opn.t_opn_path))

(* Returns the parent path of operation operation. *)
let parentpath_of_operation operation =
  parentpath_of_path operation.t_opn_path

(* Returns the short name from identifier identifier (not the complete
path). *)
let name_of_identifier identifier =
  let idx = (String.rindex identifier '/')+1 in
  String.sub identifier idx ((String.length identifier)-idx)

(* Returns the path of identifier identifier (excluded its short name). *)
let path_of_identifier identifier =
  let idx = (String.rindex identifier '/') in
  String.sub identifier 0 idx

(* Adds operation to the adequation graph graph *)
let operation_add graph operation =
  Hashtbl.add graph (identifier_of_operation operation) operation

(* Removes operation from the adequation graph graph *)
let operation_remove graph operation =
  List.iter dependence_remove operation.t_opn_dependences_predecessors;
  List.iter dependence_remove operation.t_opn_dependences_successors;
  Hashtbl.remove graph (identifier_of_operation operation)

(* Returns the operation of identifier opn_id in adequation graph graph *)
let operation_get_of_id graph opn_id =
  try
    Hashtbl.find graph opn_id
  with Not_found -> failwith ("Operation "^opn_id^" doesn't exist")

(* Returns the operation of path opn_path in adequation graph graph *)
let operation_get_of_path graph opn_path =
  operation_get_of_id graph (identifier_of_path opn_path)

(* Sets the condition of operation of path opnpath to cond *)
let operation_cond_set graph opn_path cond =
  let opn = operation_get_of_path graph opn_path in
  opn.t_opn_condition <- cond


(* Returns a new adequation graph *)
let new_adequation_graph () =
  let graph = Hashtbl.create 40 in
  (* This is a null operation used to avoid an abstract for the graph *)
  let null_opn = new_opn ["empty"] (Calcul (Constant,"","")) [] [] [] [] [] 
      None 0 0. true (Ihm []) None "" "" "" [] in
  operation_add graph null_opn;
  operation_remove graph null_opn;
  graph

(* Removes operations and dependences of status false in graph. This
   function has an heavy cost it should not be used during the
   adequation. Currently it is used as a trick to filter operations and
   dependences created during code generation. *)
let remove_false_status graph =
  Hashtbl.iter
    (fun key opn ->
      match opn.t_opn_status with
      | true ->
	  opn.t_opn_dependences_predecessors <-
	    List.filter
	      (fun dpd -> dpd.t_dpd_status = true)
	      opn.t_opn_dependences_predecessors;
	  opn.t_opn_dependences_successors <-
	    List.filter
	      (fun dpd -> dpd.t_dpd_status = true)
	      opn.t_opn_dependences_successors
      | false -> Hashtbl.remove graph key)
    graph
    
(* Returns the port of name prt_name and of direction dir from
   operation operation *)
let port_get operation prt_name dir =
  try
    List.find
      (fun prt -> (prt.t_prt_name = prt_name) && (prt.t_prt_dir = dir))
      operation.t_opn_ports
  with Not_found ->
    failwith ("Operation "^(identifier_of_operation operation)^
	      " doesn't have any port"^prt_name^"of direction "^
	      string_of_direction dir)

let data_get graph operation_path prt_name dir =
  let opn = operation_get_of_path graph operation_path in
  let port = port_get opn prt_name dir in
  opn,port

(* Returns (type,size) of data (operation,port). *)
let type_and_size_of_data (operation,port) =
  port.t_prt_typename^","^(string_of_int (port.t_prt_dim))

(* Returns the name of data (operation,port) as a string. *)
let name_of_data (operation,port) =
  (identifier_of_operation operation)^"."^(port.t_prt_name)

(* Returns the name of communication class commclass as a string. It
   roughly takes form comm_type_senders_receivers. *)
let name_of_comclass comclass =
  let string_of_senders senders =
    cut (List.fold_left 
	   (fun s opr -> s^(name_of_operator (Operator opr))^",")
	   "" senders) 1 in
  let string_of_receivers receivers =
    cut (List.fold_left 
	   (fun s opr -> s^(name_of_operator (Operator opr))^",")
	   "" receivers) 1 in
  match comclass with
  | Transfer (data,sender,receivers) -> "Transfer_"^
      (name_of_operator (Operator sender))^"_"^(string_of_receivers receivers)^
      "("^(name_of_data data)^")"
  | Write (data,writer) -> "Write_"^(name_of_operator (Operator writer))^
      "("^(name_of_data data)^")"
  | Read (data,reader) -> "Read_"^(name_of_operator (Operator reader))^
      "("^(name_of_data data)^")"
  | Send (data,sender,receivers) -> "Send_"^(name_of_operator (Operator sender))^
      "_"^(string_of_receivers receivers)^"("^(name_of_data data)^")"
  | Receive (data,sender,_,executor) -> "Receive_"^
      (name_of_operator (Operator sender))^"_"^
      (name_of_operator (Operator executor))^"("^(name_of_data data)^")"

  | Reliable_Send (data,senders,receivers) -> 
      "Reliable_Send_"^(string_of_senders senders)^
      "_"^(string_of_receivers receivers)^"("^(name_of_data data)^")"
  | Reliable_Receive (data,senders,_,executor) -> 
      "Reliable_Receive_"^(string_of_senders senders)^"_"^
      (name_of_operator (Operator executor))^"("^(name_of_data data)^")"
  | Reliable_Sync (data,senders,_,executor) -> 
      "Reliable_Sync_"^(string_of_senders senders)^"_"^
      (name_of_operator (Operator executor))^"("^(name_of_data data)^")"
 
  | Send_Synchro (sender,receiver) -> "Send_Synchro_"^
      (name_of_operator (Operator sender))^"_"^
      (name_of_operator (Operator receiver))
  | Receive_Synchro (sender,receiver) -> "Receive_Synchro_"^
      (name_of_operator (Operator sender))^"_"^
      (name_of_operator (Operator receiver))
  | Sync (data,sender,_,executor) -> "Sync_"^
      (name_of_operator (Operator sender))^"_"^
      (name_of_operator (Operator executor))^"("^
      (type_and_size_of_data data)^")"


(* Returns true if identifier exists in graph graph. *)
let exists graph identifier = Hashtbl.mem graph identifier

(* Returns an unique identifier, adding an unique number in case of
   collisions, for operation operation_path/name in graph graph. *)
let unique_identifier graph operation_path name =
  let rec unique_name number =
    let opnname = name^"_"^(string_of_int number) in
    let opnid = identifier_of_path (operation_path@[opnname]) in
    match hashtbl_filter 
	(fun o -> (identifier_of_operation o)=opnid)
	graph with
    | [] -> opnname
    | _ -> unique_name (number+1) in
  match exists graph (identifier_of_path (operation_path@[name])) with
  | true -> unique_name 0
  | false -> name
	
(* Returns true if operator o is a media. *)
let is_media o = 
  match o with 
  | Media mda -> true
  | _ -> false

(* Returns the definition name of operator o. *)
let type_of_operator o =
  match o with
  | Operator opr -> opr.oprref_definition.oprdef_name
  | Media mda -> mda.mdaref_definition.mdadef_name

(* Returns the medium corresponding to operator_class media.
   Raises Failure if media is a calcul operator. *)
let media_of_operator_class media = match media with
| Media mda -> mda
| Operator _ -> raise (Failure "Adequationtypes.media_of_operator_class error")

(* Returns the calcul operator corresponding to operator_class operator.
   Raises Failure if operator is a medium. *)
let operator_of_operator_class operator = match operator with
| Operator opr -> opr
| Media _ -> raise (Failure "Adequationtypes.operator_of_operator_class error")

(* Returns data transmitted by communication com. *)
let data_of_communication com = match com.t_opn_class with
| Communication comclass -> (match comclass with
  | Write (data,_) -> data
  | Read (data,_) -> data
  | Transfer (data,_,_) -> data
  | Send (data,_,_) -> data
  | Receive (data,_,_,_) -> data
  | Reliable_Send (data,_,_) -> data
  | Reliable_Receive (data,_,_,_) -> data
  | Reliable_Sync (data,_,_,_) -> data
  | Sync (data,_,_,_) -> data
  | Send_Synchro (_,_) | Receive_Synchro (_,_) ->
      failwith "Adequationtypes.data_of_communication synchro")
| _ -> raise (Failure "Adequationtypes.data_of_communication error 1")





(* Returns all operations of graph scheduled on operator (not anymore
sorted as it used to be). *)
let operations_of_operator graph operator =
  hashtbl_filter
    (fun o ->
      match o.t_opn_operator with
      | Some opr -> opr=@operator
      | None ->
	  raise (Failure "Adequationtypes.operations_of_operator error"))
    graph

(* Returns all operations of graph scheduled on media which are
   "related" to operator, ie for which operator is : sender, receiver,
   etc. *)
let operations_of_operator_media graph operator media =
  let operator = operator_of_operator_class operator in
  let opns = operations_of_operator graph media in
  List.filter 
    (fun {t_opn_class=opnclass} -> match opnclass with
    | Communication comclass ->
	(match comclass with
	| Write (_,writer) -> writer=@@operator
	| Read (_,reader) -> reader=@@operator
	| Transfer (_,send_opr,recv_oprs) -> 
	    send_opr=@@operator || List.mem operator recv_oprs 
	| Send (_,sender,_) -> sender=@@operator
	| Receive (_,_,_,executor) -> operator=@@executor 
	| Reliable_Send (_,senders,_) -> List.exists (fun sender -> sender=@@operator) senders
	| Reliable_Receive (_,_,_,executor) -> operator=executor 
	| Reliable_Sync (_,_,_,executor) -> operator=executor
	| Send_Synchro (sender,_) -> sender=operator
	| Receive_Synchro (_,receiver) -> receiver=operator
	| Sync (_,_,_,executor) -> operator=executor)
    | _ ->
	raise (Failure "Adequationtypes.operations_of_operator_media error")) opns
    
(* Returns the path of operation operation. *)
let path operation = match operation.t_opn_origin with
| Ihm path -> path
| _ -> operation.t_opn_path

(* Notice that this function cost is now constant while it was
   O(nb_dependences) in it's former version *)
(* Returns predecessor operations of operation opn. *)
let predecessors opn =
(*   List.map (function {t_dpd_sopn=sopn} -> sopn) opn.t_opn_dependences_predecessors *)
  opn.t_opn_predecessors

(* Returns successor operations of operation opn. *)
let successors opn =
 (* List.map (function {t_dpd_dopn=dopn} -> dopn) opn.t_opn_dependences_successors  *)
  opn.t_opn_successors

(* Operation types *)
(* Returns true if operation operation is a calcul operation. *)
let is_calcul operation = match operation.t_opn_class with
| Calcul (_,_,_) -> true
| _ -> false

(* Returns true if operation operation is a communication operation. *)
let is_communication operation = match operation.t_opn_class with
| Communication _ -> true
| _ -> false

(* Returns true if operation operation is an operation created at the
   transformation (which didn't explicitely exist in user specication,
   like Implode, etc). *)
let is_implantation operation = match operation.t_opn_origin with
| Ihm _ -> false
| _ -> true

(* Returns true if operation operation is a delay operation. *)
let is_memory operation =
  match operation.t_opn_class with
  | Calcul(algoclass,_,_) -> (match algoclass with
    | Memory _ -> true
    | _ -> false)
  | _ -> false

(* Returns true if operation operation is a constant operation. *)
let is_constant operation =
  match operation.t_opn_class with
  | Calcul(algoclass,_,_) -> (match algoclass with
    | Constant -> true
    | _ -> false)
  | _ -> false

(* Returns true if operation operation is a factorization boundary,
   in case repetition haven't been expanded. *)
let is_factorization_boundary operation =
  match operation.t_opn_origin with
  | Diffuse _ | Iterate _ | Fork _ | Join _ -> true
  | _ -> false

(* Returns true if operation is conditioned. *)
let conditioned_operation operation =
  not (List.mem (None,None) operation.t_opn_condition)

(* Returns true if dependence is conditioned. *)
let conditioned_dependence dpd =
  not (List.mem (None,None) dpd.t_dpd_condition)

(* Returns true if port is a precedence port. *)
let is_precedence_port port =
  Str.string_match (Str.regexp "precedence") port.t_prt_name 0

(* Returns true if opncom is a precedence communication. *)
let is_precedence_com opncom =
  let (_,dataprt) = data_of_communication opncom in
  is_precedence_port dataprt

(* Returns true if dpd transmits data *)
let is_dependence_with_data dpd =
  match dpd.t_dpd_class with
  | (Condition _,_) | (Data,Precedence) -> false
  | _ -> true

(* Returns the precedence port of direction dir in operation operation. *)
let find_precedence_port operation dir =
  try List.find
      (fun p -> (is_precedence_port p) && (p.t_prt_dir=dir))
      operation.t_opn_ports
  with _ ->
    failwith ("No precedence port for operation "^(name_of_operation operation))

(* Returns the operator on which calcul operation opn is currently scheduled.
   Raises Failure if opn is not yet scheduled. *)
let operator_of_operation opn = match opn.t_opn_operator with
| Some opr -> opr
| None ->
    raise (Failure ("Adequationtypes.operator_of_operation error ("^
		    (identifier_of_operation opn)^")"))

(* Returns the operator on which communication operation com is executed. *)
let execution_operator_of_communication com = match com.t_opn_class with
| Communication (Send (_,sender,_)) -> sender
| Communication (Receive (_,_,_,executor)) -> executor
| Communication (Reliable_Send (_,senders,_)) -> (List.hd senders)
| Communication (Reliable_Receive (_,_,_,executor)) -> executor 
| Communication (Reliable_Sync (_,_,receivers,executor)) -> executor
| Communication (Write (_,writer)) -> writer
| Communication (Read (_,reader)) -> reader
| Communication (Send_Synchro (sender,_)) -> sender
| Communication (Receive_Synchro (_,receiver)) -> receiver
| Communication (Sync (_,_,receivers,executor)) -> executor
| _ -> raise (Failure "Adequationtypes.execution_operators_of_communication")


(* Returns the operator on which operation opn (com or calcul) is executed. *)
let execution_operator_of_operation opn = match (is_calcul opn) with
| true -> operator_of_operator_class (operator_of_operation opn)
| false -> execution_operator_of_communication opn

(* Returns (deflib,defname) of operation operation. *)
let deflibname operation = match operation.t_opn_class with
| Calcul (_,deflib,defname) -> deflib,defname
| _ -> raise (Failure "Adequationtypes.deflibname error")

(* Returns operation with identifier identifier in graph graph. *)
let operation_of_identifier graph identifier =
  Hashtbl.find graph identifier

(* Returns the duration of operation operation class when executed on
   operator operator. *)
let delta operationclass operator = match operationclass with
| Calcul (algoclass,algolib,algoname) ->
    (match algoclass with
    | Constant -> 0.
    | _ -> (match operator with
      | Operator opr ->
	  Architecture.operator_reference_duration opr algolib algoname
      | _ ->
	  raise (Failure "Adequationtypes.delta : error 1 (Operation on media)")))
| Communication comclass -> 
    let typename,dim = match comclass with
    | Write ((_,prt),_) -> prt.t_prt_typename,prt.t_prt_dim
    | Read ((_,prt),_) -> prt.t_prt_typename,prt.t_prt_dim
    | Send ((_,prt),_,_) -> prt.t_prt_typename,prt.t_prt_dim
	  (*(Symbolic.Bin (Symbolic.div,prt.t_prt_dim,Symbolic.Float 2.))*)
    | Receive ((_,prt),_,_,_) -> prt.t_prt_typename,0
    | Reliable_Send ((_,prt),_,_) -> prt.t_prt_typename,prt.t_prt_dim
    | Reliable_Receive ((_,prt),_,_,_) -> prt.t_prt_typename,0
    | Reliable_Sync ((_,prt),_,_,_) -> prt.t_prt_typename,prt.t_prt_dim 
    | Transfer ((_,prt),_,_) -> prt.t_prt_typename,prt.t_prt_dim
	  (* As we don't really have a characterisation we arbitrarily
	     choose int 1 *)
    | Receive_Synchro (_,_) -> "int",1
	  (* As we don't really have a characterisation we arbitrarily
	     choose int 1 *)
    | Send_Synchro (_,_) -> "int",1
    | Sync ((_,prt),_,_,_) -> prt.t_prt_typename,prt.t_prt_dim in
    (match operator with
    | Media mda ->
	(Architecture.media_reference_duration mda typename)*.
	  (float_of_int dim)
    | _ ->
	raise (Failure "Adequationtypes.delta : error 2 (Communication on operator)"))

(* Returns the average duration of operation operationclass on all
   the operators of the architecture. *)
let delta_average operationclass = match operationclass with
| Calcul (algoclass,algolib,algoname) -> (match algoclass with
  | Constant -> 0.
  | _ -> Architecture.operator_duration_average algolib algoname)
| Communication _ -> raise (Failure "delta_average : Communication")

(* Returns median duration of opnlib/opnname on all operator definitions. *)
let delta_median operations_median operation =   
  try Hashtbl.find operations_median operation
  with Not_found -> 
    let opnlib,opnname = deflibname operation in
    let oprs = Application.application.app_operator_definitions in
    let durations,nb_oprs = List.fold_left (fun (tmp,nb_oprs) opr -> try
				      let duration = Hashtbl.find opr.oprdef_operation_durations (opnlib,opnname) in
				      duration::tmp,(nb_oprs+1)
				    with Not_found -> tmp,nb_oprs) ([],0) oprs in
    let sort_durations  =  List.sort compare durations in 
    let moy = (nb_oprs/2) in
    let median = match (nb_oprs mod 2) with
      | 1 -> List.nth sort_durations moy
      | _ -> ((List.nth sort_durations (moy-1)) +.(List.nth sort_durations moy)) /. 2. in
    Hashtbl.add operations_median operation median;
    (*ps ("............................... median of "^(name_of_operation operation)^" = "^(string_of_float median));*)
    median

(*get median of list *)
let get_median values = 
  let sort_values =  List.sort compare values in 
  let len = List.length values in
  let moy = len/2 in
  let median = match (len mod 2) with
    | 1 -> float_of_int (List.nth sort_values moy)
    | _ -> ((float_of_int(List.nth sort_values (moy-1))) +. (float_of_int (List.nth sort_values moy))) /. 2. in 
  median

(*get median of list *)
let get_average values = 
  let sum = List.fold_left (fun t v -> t + v) 0 values in
  (float_of_int sum) /. (float_of_int (List.length values))


(* Returns the earliest end from start of operation opn.
   Raises Failure if operation opn is not yet scheduled. *)
let eefs operation = match operation.t_opn_operator with
| Some opr -> operation.t_opn_esfs +. delta operation.t_opn_class opr
| None ->
    raise (Failure ("Adequationtypes.eefs "^(name_of_operation operation)))

(* Returns newly created in/out precedence ports. *)
let precedence_ports () =
  let portin =
    {t_prt_name=Algorithm.precedence_in_name;
     t_prt_dir=Port.In;t_prt_typename="prec_synchro";
     t_prt_dim=1;
     t_prt_class=Port.Precedence_Port;
     t_prt_order=0}
  and portout =
    {t_prt_name=Algorithm.precedence_out_name;
     t_prt_dir=Port.Out;
     t_prt_typename="prec_synchro";
     t_prt_dim=1;
     t_prt_class=Port.Precedence_Port;
     t_prt_order=0} in
  [portin;portout]

(* Returns true if operation0 and operation1 are simultaneous *)
let simultaneous s0 e0 s1 e1 =
  (s0<>e0) && (s1<>e1) && ((s1<=s0 && s0<e1) ||
  (s1<e0 && e0<=e1) || (s0<=s1 && s1<e0) || (s0<e1 && e1<=e0))

(* Returns true if conditions cl1 and cl2 are exculsive. *)
let exclusion_condition cl1 cl2 =
  List.fold_left
    (fun exclusion (vr1,vl1) ->
      exclusion || (List.exists (fun (vr2,vl2) -> vr2=vr1 && vl2<>vl1) cl2))
    false cl1

(* Returns the string corresponding to condition c. *)
let string_of_condition c =
  match c with
  | (Some data,Some value) -> (name_of_data data^" = "^(string_of_int value))
  |  (None,None) -> "true"
  | _ -> "err"
	
(* Returns the string corresponding to conditions list l. *)
let string_of_condlist l =
  let cond_string = List.fold_left (fun rest cond -> match rest with
  | "" -> string_of_condition cond
  | _ -> rest^" & "^(string_of_condition cond)) "" l in
  match cond_string with
  | "" -> "true"
  | _ -> cond_string

(* Returns the string corresponding to dependence dpd. *)
let string_of_dpd dpd =
  match dpd.t_dpd_class with
  | _,Precedence -> (identifier_of_operation dpd.t_dpd_sopn)^" -> "^
      (identifier_of_operation dpd.t_dpd_dopn)
  | _ -> (name_of_data (dpd.t_dpd_sopn,dpd.t_dpd_sprt))^" -> "^
      (name_of_data (dpd.t_dpd_dopn,dpd.t_dpd_dprt))

(* Returns the string corresponding to port *)
let string_of_port port =
  let dim_value = port.t_prt_dim in
  let dim_str = "["^(string_of_int dim_value)^"]" in
  let dir = string_of_direction port.t_prt_dir in
  dir^" "^port.t_prt_typename^" "^dim_str^" "^port.t_prt_name
						 
(* Returns the string corresponding the schedule gaps gaps (only for
   debug) *)
let string_of_gaps_list gaps =
  List.fold_left
    (fun s (inf,sup) -> s^",["^(string_of_float inf)^";"^
      (string_of_float sup)^"]")
    "" gaps

(* Returns the result of the evaluation of expression expr, for which
   variables values are given by context context, and evaluation is done
   for operation of path path. *)
let size_of_expression context path expr =
  let expr = Algorithm.globalexpression_of_localexpression path expr in
  try match Symbolic.result_of_expression context expr with
  | Symbolic.RFloat i -> int_of_float i
  | _ -> 1
  with Failure s ->
    let context = List.fold_left (fun s (cs,_) -> s^" \n "^cs) "" context in
    failwith (s^" Could not evaluate "^(Symbolic.string_of_expression expr)^
	      " for "^(identifier_of_path path)^"\n Existing vars are: "^
	      context)

(* Returns the factor by which ports of operation operation must be
   multiplied to obtain the real port size ie the port size once
   defactorized. The factor will be different from 1 only if operation is
   a factorization boundary of type Fork or Join. *)
let port_factor_of_boundary operation =
  match operation.t_opn_origin with
  | Fork (_,rep) | Join (_,rep) -> rep
  | _ -> 1

(* Returns the path of the operation from which operation has been
   created. *)
let path_of_origin operation =
  match operation.t_opn_origin with
  | Ihm p | Condition_In p | Condition_Out p| Explode p| Implode p
  | Synchro_Constant p | Diffuse (p,_) | Iterate (p,_) | Fork (p,_)
  | Join (p,_) -> p

(* Returns the software component which should be herited from
   operation identified (in non-transformed graph) by parpath, for which
   software component is parxsc *)
let herited_xsc parpath parxsc =
  match parxsc with
  | "" -> Application.xscname_of_ref parpath AttachRef
  | name -> name

(* Returns the xsc which must be given to operation identified by
   childpath, taken into account herited xsc and specified xsc on this
   operation *)
let get_xsc parpath parxsc childpath attachtype =
  let parent_xsc = herited_xsc parpath parxsc in
  match Application.xscname_of_ref childpath attachtype with
  | "" -> parent_xsc
  | x -> x

(* Returns the list of operations without predecessors in graph. *)    
let input_operations graph =
  let f o = o.t_opn_dependences_predecessors = [] in
  hashtbl_filter f graph

(* Returns the list of operations of graph in the width first order
   (ie in the order of the dependences)*)
(* Not used anymore, this assumes that the graph doesn't contain any
   cycle, not even delays !!! *)
let width_first_path graph =
  let inputs = input_operations graph in
  let all_precs_memories =
    (hashtbl_filter
       (fun opn -> List.for_all is_memory (predecessors opn))
       graph) in
  let first_width = inputs@all_precs_memories in

  (* Returns true if all the predecessors of operation are already
     in the path *)
  let predecessors_processed operation processed=
    List.for_all
      (fun pred -> List.mem pred processed)
      (predecessors operation) in
  (* Returns the successors of operation which are next in the path *)

  let next_in_path operation processed =
    List.filter
      (fun succ -> predecessors_processed succ processed)
      (successors operation) in
  
  let rec width_path_from sources processed =
    let nexts = List.map (fun opn -> next_in_path opn processed) sources in
    let nexts_flat = union nexts in
    match nexts_flat with
    | [] -> processed
    | _ -> width_path_from nexts_flat (processed@nexts_flat) in

  width_path_from all_precs_memories all_precs_memories

let psll ll =
  List.fold_left
    (fun s (o1,o2) ->
       s^(name_of_operator o1)^"->"^(name_of_operator o2)^";")
    "   " ll 

let dbg graph =
  let o,d,ds=ref 0,ref 0,ref 0 in
  Hashtbl.iter 
    (fun n opn  ->
      o := !o + 1; ds:=0;
      List.iter 
	(function d -> ds:= !ds + 1) 
	opn.t_opn_dependences_successors; 
      d:=!d + !ds; 
      ps(n^" : "^(string_of_int !ds)^" dependences")) 
    graph; 
  ps ("\nOperations : "^(string_of_int !o)^", Dependences : "^(string_of_int !d))  


(* failures rate type *)
let frequency : (Types.operator_class, (float * float * float * float)) Hashtbl.t = Hashtbl.create 30
let failures_rate : (Types.operator_class, float) Hashtbl.t = Hashtbl.create 30
let replication_process_proc : (string, (int * int * int)) Hashtbl.t = Hashtbl.create 6
let scheduled_opns = ref []

(* get failure rates *)
(*let initialize_failure_rates_table file_name oprs mdas = 
  let name_fileRel = (String.sub file_name 0 (String.rindex file_name '.'))^".rel" in
  let fileRel = open_in name_fileRel  in
  let ch = ref "" in
  List.iter (fun  opr -> ch := (input_line fileRel); 
  Hashtbl.add failures_rate opr (float_of_string !ch)) (oprs@mdas);
    close_in fileRel*)
   
(* get failure rates *)
let initialize_failure_rates_table  file_name  =     
  let (algo_lib,algo_name,_),(archilib,archiname) = 
    Application.algo_main_get (), Application.archi_main_get () in
  let all_operators = (Application.operators_list archilib archiname)@(Application.media_list archilib archiname) in
  let name_fileRel = (String.sub file_name 0 (String.rindex file_name '.'))^".rel" in
  let fileRel = open_in name_fileRel  in
  let ch = ref "" in
  let index = ref 1 in
  List.iter (fun   opr -> ch := (input_line fileRel);
	       if not (is_media opr) 
	       then 
                   begin 
                     Hashtbl.add replication_process_proc (name_of_operator opr) (!index,0,0);index := !index +1; 
                   end;
               Hashtbl.add failures_rate opr (float_of_string !ch)) all_operators;
  close_in fileRel


(* get procs frequency *)
let initialize_frequency_table  file_name  =     
  let (algo_lib,algo_name,_),(archilib,archiname) = 
    Application.algo_main_get (), Application.archi_main_get () in
  let all_operators = (Application.operators_list archilib archiname)@(Application.media_list archilib archiname) in
  let name_fileFreq = (String.sub file_name 0 (String.rindex file_name '.'))^".frq" in
  let fileFreq = open_in name_fileFreq  in 
  let freqs =  [|0.;0.;0.;0. |] in 
  for i = 0 to 3 do 
     freqs.(i) = (float_of_string (input_line fileFreq));   
   done; 
  List.iter (fun   opr -> if not (is_media opr) then Hashtbl.add frequency opr (freqs.(0),freqs.(1),freqs.(2),freqs.(3)) ) all_operators;             
  close_in fileFreq


       
let get_lambda _R _t =
  -. (log _R) /. _t


