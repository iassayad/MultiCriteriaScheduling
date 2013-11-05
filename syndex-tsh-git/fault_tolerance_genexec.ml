(*************************************************************************)
7(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                             Julien Forget                             *)
(*                          Christophe Macabiau                          *)
(*                          Thierry Grandpierre                          *)
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

(** This module contains the functions used to generate executive from
   the adequation result.*)

let debug_level = 0

let processor_macro = "processor_"
and endprocessor_macro = "\nendprocessor_"

let architecture_macro = "architecture_"
and connect_macro = "connect_"
and end_architecture_macro = "endarchitecture_"

let processor_init_macro = "proc_init_"
and processor_end_macro = "proc_end_"

let thread_macro = "thread_"
and end_thread_macro = "endthread_"

let alloc_macro = "alloc_"
let alias_macro = "alias_"

let sems_begin_macro = "semaphores_(\n"
and sems_end_macro = ")\n"

let compute_begin_macro = "main_\n"
and compute_end_macro = "endmain_\n"

let spawn_thread_macro = "spawn_thread_"
and wait_thread_macro = "wait_endthread_"

let chrono_macro = "Chrono_"

let loadfrom_macro = "loadFrom_"
and loaddownto_macro = "loadDnto_"
and savefrom_macro = "saveFrom_"
and saveupto_macro = "saveUpto_"

let if_macro = "if_"
and endif_macro = "endif_"
and switch_macro = "switch_"
and case_macro = "case_"
and endcase_macro = "endcase_"
and endswitch_macro = "endswitch_"

let processors_failures_macro = "processors_failures_"
let media_failures_macro = "media_failures_"
let npf = ref 0 
and nmf = ref 0

let semaphore_thread_name = "Semaphore_Thread_"

let indent = "  "

type suc_pre = Suc | Pre
and empty_full = Empty | Full

let context = ref []


let archilib = ref ""
let archiname = ref ""

let load_downto = Hashtbl.create 50
let load_from = Hashtbl.create 50

let filename_of_name directory name = directory^name^".m4"
let filename_of_opr directory opr =
  filename_of_name directory (name_of_operator opr)

let eval_expr expr = match Symbolic.result_of_expression !context expr with
| Symbolic.RFloat dim -> string_of_float dim
| _ -> raise (Failure "Genexec.eval_expr error")

let sem_thread_name media = name_of_operator media

(*****************************************************************)
(************* AFFICHAGE PREDS AND SUCCS OFF OPERATIONS **********)
(*****************************************************************)
  let ps_dpds operation_AN = 
    ps (List.fold_left (function s -> function dpd -> s^(name_of_operation dpd.t_dpd_sopn)^" , ") ("preds dpd "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_dependences_predecessors))(*;
    ps (List.fold_left (function s -> function dpd -> s^(name_of_operation dpd.t_dpd_dopn)^" , ") ("succs dpd "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_dependences_successors))*)

(*****************************************************************)
(************* AFFICHAGE PREDS AND SUCCS OFF OPERATIONS **********)
(*****************************************************************)
let ps_preds operation_AN = 
  ps (List.fold_left (function s -> function sopn -> s^(name_of_operation sopn)^" , ") ("preds : "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_predecessors));
  ps (List.fold_left (function s -> function dopn -> s^(name_of_operation dopn)^" , ") ("succs : "^(name_of_operation operation_AN)^" = ") (operation_AN.t_opn_successors)) 
										
 (*  test if operation opn is original  *)  
  let is_original opn = 
    let name = name_of_operation opn in
      try
	if (String.contains name '#')
	then false
	else true
      with _  -> true

(*   get original operation name *)
  let get_original nameopn =
    try 
      let original =(String.sub nameopn 0 (String.rindex nameopn '#')) in 
	(String.sub original 0 (String.rindex original '_'))
    with _ -> nameopn

let buffername_of_data (opn,prt) =
  (* the second tl is to cut the application name *)
  let path = List.tl (List.tl opn.t_opn_path) in
  let name = string_of_string_list path "_" in
  (get_original name)^"_"^prt.t_prt_name

let gate_of_gateoption gate = match gate with
| Some g -> g
| None -> raise (Failure "Genexec.gate_of_gateoption error")

(** Calculates the code load-tree starting from root operator and
   basicaly using a graph width-first path algorithm *)
let load_tree () =
  let operators = Application.operators_list !archilib !archiname in
  let root = Architecture.operator_main_get !archilib !archiname in
  let root =
    Operator (Architecture.operator_reference !archilib !archiname root) in

  let rec aux oprs =
    let neighbour opr = 
      List.flatten (List.map
		      (fun ((o1,g1),(m,_)) -> match m with
		      | Media mda ->
			  List.map
			    (fun ((_,_),(o2,g2)) -> (o1,g1),(o2,g2))
			    mda.mdaref_neighbours
		      | _ -> failwith "Genex.loadtree")
		      (operator_of_operator_class opr).oprref_neighbours) in
    let neighbours =
      List.flatten (List.map (fun opr -> neighbour opr) oprs) in
    let neighbours =
      List.fold_left
	(fun l ((_,(o,_)) as lnk) ->
	  match (List.exists (fun (_,(op,_)) -> o=op) l) || (List.mem o oprs) with
	  | true -> l
	  | false -> l@[lnk])
	[] neighbours in
    (*       ps ((name_of_operator opr)^" neighbours are : "^
       (List.fold_left (fun s (_,(opr,_)) -> s^"; "^(name_of_operator opr)) "" neighbour_oprs)); *)
    let changed = ref false in
    List.iter (fun ((o1,g1),(o2,g2)) ->
      let o1,o2 = (name_of_operator o1),(name_of_operator o2) in
      let g1,g2 = (gate_of_gateoption g1).gte_name,(gate_of_gateoption g2).gte_name in
      match (Hashtbl.mem load_from o2)||(o2=(name_of_operator root)) with
      | true -> ()
      | false ->
	  changed := true;
	  let gates_oprs = match Hashtbl.mem load_downto o1 with
	  | true ->
	      let gates_oprs = Hashtbl.find load_downto o1 in
	      Hashtbl.remove load_downto o1;
	      (match List.mem_assoc g1 gates_oprs with
	      | true ->
		  let oprs = List.assoc g1 gates_oprs in
		  (g1,(o2::oprs))::(List.remove_assoc g1 gates_oprs)
	      | false -> (g1,[o2])::gates_oprs)
	  | false -> [g1,[o2]] in
	  Hashtbl.add load_downto o1 gates_oprs;
	  Hashtbl.add load_from o2 (g2,o1)) neighbours;
    match !changed with
    | true -> let nexts =
	List.fold_left
	  (fun l ((_,_),(onext,_)) ->
	    match Hashtbl.mem load_downto (name_of_operator onext) with
	    | true -> l
	    | false -> onext::l)
	  [] neighbours in
      aux nexts
    | false -> () in
  Hashtbl.clear load_from;
  Hashtbl.clear load_downto;
  aux [root]

(** Returns true if operation is a Read or Write operation *)      
let is_read_write operation = match operation.t_opn_class with
| Communication (Read _)
| Communication (Write _) -> true
| _ -> false

(** Returns the name of the gate through which communication operation
   opn is transfered *)
let gate_name opn = match is_communication opn with
| true ->
    let media = operator_of_operation opn in
    let operator = Operator (execution_operator_of_communication opn) in
    let links = Architecture.links !archilib !archiname media in
    (match List.filter (fun (_,(opr,_)) -> opr=operator) links with
    | (_,(_,gate))::_ -> (gate_of_gateoption gate).gte_name
    | _ -> raise (Failure "Genexec.gate_name error 2"))
| false -> ""

(** Returns the name of the gate through which dependence sopn->dopn is
   transmitted.*)
let operator_gate_name sopn dopn =
  let com = match is_communication dopn with
  | true -> dopn
  | false -> (match is_communication sopn with
    | true -> sopn
    | false -> raise (Failure "Genexec.operator_gate_name error")) in
  let media = operator_of_operation com in
  let operator = Operator (execution_operator_of_communication com) in
  (name_of_operator operator)^"_"^(gate_name com)

let semaphore_name opn opndpd fullempty =
  (* This may produce irrelevant result for Send_Synchro ,
     Receive_Synchro, Sync. Anyway this function will never be used in
     those cases *) 
  let com = match opn.t_opn_class with
  | Communication comclass -> opn
  | _ ->
      match opndpd.t_opn_class with
      | Communication comclass -> opndpd
      | _ ->
	  raise (Failure ("Genexec.semaphore_name error 1 ("^
			  ((identifier_of_operation opn)^" <-> "^
			   (identifier_of_operation opndpd)^")"))) in
  let data,cond = (data_of_communication com),com.t_opn_condition in
  let buffer_name = buffername_of_data data in
  let cond = 
    List.fold_left
      (fun s (v,vl) -> s^"_"^
	(match v,vl with
	| (Some v),(Some vl) ->
	    (buffername_of_data v)^"_"^(string_of_int vl)
	| None,None -> ""
	| _ ->
	    raise (Failure "Genexec.semaphore_name error 2")))
      "" cond in
  let fullempty = match fullempty with
  | Empty -> "empty"
  | Full -> "full" in
  let ram = match is_read_write opn && is_read_write opndpd with
  | true -> "_RAM"
  | false -> "" in
  let sopn,dopn = match List.mem opn (successors opndpd) with
  | true -> opndpd,opn
  | _ -> opn,opndpd in
  let cond =
    List.fold_left
      (fun s (v,vl) -> s^"_"^
	(match v,vl with
	| (Some v),(Some vl) ->
	    (buffername_of_data v)^"_"^(string_of_int vl)
	| None,None -> ""
	| _ ->
	    raise (Failure "Genexec.semaphore_name error 3")))
      "" sopn.t_opn_condition in
  let operator_gate_name = operator_gate_name sopn dopn in
  buffer_name^cond^ram^"_"^operator_gate_name^"_"^fullempty

(** Returns the macro string for a list of semaphores *)
let string_of_semaphores sems offset =
  let sems = remove_copies sems in 
  (*  let sems_string =
     List.fold_left (fun sems_string (sopn,dopn) ->
     sems_string^offset^indent^(semaphore_name sopn dopn Empty)^","^
     (semaphore_name sopn dopn Full)^",\n") "" sems in
     cut sems_string 2*)
  let sems =
    List.map (fun (sopn,dopn) ->
      [(semaphore_name sopn dopn Empty);
       (semaphore_name sopn dopn Full)]) sems in
  let sems = remove_copies (List.concat sems) in 
  List.fold_left (fun sems_string sem -> 
    sems_string^offset^indent^sem^",\n") "" sems

(** Returns the semaphores (a string of macro list) corresponding to a
  set of operations (scheduled on the same operator). *)
let alloc_semaphores nghbrs opns =
  let rec sems_opn opns sems = match opns with
  | opn::tl ->
      let sems = match opn.t_opn_class with
      | Communication (Send _) | Communication (Write _) ->
	  (* this filters precedences added at synchronization *)
	  let dpd =
	    try
	      List.find
		(fun {t_dpd_sopn=sopn} ->
		  sopn.t_opn_operator <> opn.t_opn_operator)
		opn.t_opn_dependences_predecessors
	    with _ -> failwith "Damned" in
	  (dpd.t_dpd_sopn,opn)::sems
      | Communication (Reliable_Send _) ->
	  (* this filters precedences added at synchronization *)
	  let dpd =
	    try
	      List.find
		(fun {t_dpd_sopn=sopn} ->
		  sopn.t_opn_operator <> opn.t_opn_operator)
		opn.t_opn_dependences_predecessors
	    with _ -> failwith "Damned" in
	  (dpd.t_dpd_sopn,opn)::sems
      | Communication (Reliable_Receive _) ->
	  let dpds =
	    (* this filters precedences added at synchronization *)
	    List.filter
	      (fun {t_dpd_dopn=dopn} ->
		dopn.t_opn_operator <> opn.t_opn_operator)
	      opn.t_opn_dependences_successors in
	  (List.map (fun {t_dpd_dopn=dopn} -> (opn,dopn)) dpds)@sems
      | Communication (Receive _) | Communication (Read _) ->
	  let dpds =
	    (* this filters precedences added at synchronization *)
	    List.filter
	      (fun {t_dpd_dopn=dopn} ->
		dopn.t_opn_operator <> opn.t_opn_operator)
	      opn.t_opn_dependences_successors in
	  (List.map (fun {t_dpd_dopn=dopn} -> (opn,dopn)) dpds)@sems
      | Communication (Transfer _) -> raise (Failure "Genexec.sems_opn error")
      |	_ -> sems in
      sems_opn tl sems
  | _ -> sems in
  let sems = sems_opn opns [] in
  match nghbrs,sems with 
  | [],[] -> ""
  | _ ->
      let thread_sems_string =
	List.fold_left
	  (fun s ((_,gate),_) ->
	    s^indent^semaphore_thread_name^
	    (gate_of_gateoption gate).gte_name^",\n")
	  "" nghbrs in 
      let thread_sems_string = match sems with 
      | [] -> cut thread_sems_string 2
      | _ -> thread_sems_string in
      let sems_string = string_of_semaphores sems "" in
      sems_begin_macro^thread_sems_string^sems_string^sems_end_macro 

let alloc_shared_semaphores opns =
  let rec sems_opn opns sems = match opns with
  | opn::tl ->
      let sems = match opn.t_opn_class with
      | Communication (Write _) ->
	  (* this filters precedences added at synchronization *)
	  let dpds =
	    List.filter
	      (fun {t_dpd_dopn=dopn} -> match dopn.t_opn_class with
	      | Communication (Read _) -> true
	      | _ -> false)
	      opn.t_opn_dependences_successors in
	  (List.map (fun {t_dpd_dopn=dopn} -> (opn,dopn)) dpds)@sems
      | Communication (Read _) ->
	  (* this filters precedences added at synchronization *)
	  let dpd =
	    try
	      List.find
		(fun {t_dpd_sopn=sopn} -> match sopn.t_opn_class with
		| Communication (Write _) -> true
		| _ -> false)
		opn.t_opn_dependences_predecessors
	    with _ -> failwith "shared" in 
	  (dpd.t_dpd_sopn,opn)::sems
      | _ -> raise (Failure "Genexec.sems_opn error") in
      sems_opn tl sems
  | _ -> sems in

  match sems_opn opns [] with
  | [] -> ""
  | sems ->
      let sems = string_of_semaphores sems indent in
      indent^sems_begin_macro^sems^indent^sems_end_macro

let globalexpression path expr =
  Algorithm.globalexpression_of_localexpression path expr

(** Returns a new port corresponding to the buffer required to store
values for a delay. *)
let delay_buffer opn =
  (* We assume there's only one delay port *)
  let data_port = try
    List.find (fun prt -> prt.t_prt_class = Port.Delay_Port) opn.t_opn_ports
  with Not_found -> failwith ("Delay "^(name_of_operation opn)^
			      " doesn't have any delay port") in
  let typename = data_port.t_prt_typename in
  let nb_delays =
    try
      List.hd (List.rev (opn.t_opn_arguments_values))
    with Failure _ ->
      failwith ("Delay "^(name_of_operation opn)^
		" doesn't have the delay range required parameter.") in
  let global_parameter =
    globalexpression (parentpath_of_path (path_of_origin opn)) nb_delays in
  let result = Symbolic.result_of_expression !context global_parameter in
  let nb_delays = match result with
  | Symbolic.RFloat f -> (int_of_float f)
  | _ ->
      failwith ("Delay "^(name_of_operation opn)^
		" range (last parameter) is not an integer.") in
  let dim = nb_delays * data_port.t_prt_dim in
  {t_prt_name="buf";
   t_prt_dir=Port.Out;
   t_prt_typename=typename;
   t_prt_dim=dim;
   t_prt_class=Port.Delay_Buffer;
   t_prt_order=(-1)}

(** [delay_alias (opn,port)] returns an alias macro for delay
   operation [opn]; [port] is an alias on [opn]'s internal delay buffer *)
let delay_alias (opn,port) = 
  match (is_precedence_port port),port.t_prt_dir with
  | false, Port.Out ->
      let delay_buffer = delay_buffer opn in
      let buffer_size = delay_buffer.t_prt_dim in
      let nb_delays = List.hd (List.rev opn.t_opn_arguments_values) in
      let global_parameter =
	globalexpression (parentpath_of_path (path_of_origin opn)) nb_delays in
      let result = Symbolic.result_of_expression !context global_parameter in
      let nb_delays =
	match result with
	| Symbolic.RFloat i -> int_of_float i
	| _ ->
	    failwith ("Delay "^(name_of_operation opn)^
		      " range (last parameter) is not a float.") in
      let elts_per_repetition = buffer_size / nb_delays in
      (match (buffer_size mod elts_per_repetition = 0) with
      | false ->
	  (* is not an integer *)
	  failwith ("Delay "^(name_of_operation opn)^
		    " parameters are not consistant with its ports sizes.")
      | true ->
	  alias_macro^"("^(buffername_of_data (opn,port))^","^
	  (buffername_of_data (opn,delay_buffer))^",0,"^
	  (string_of_int elts_per_repetition)^")\n")
  | _ -> ""


(** [alloc_databuffer opr opns] returns alloc macros for operations
   [opns], which are scheduled on [opr] *)
let alloc_databuffer opr opns =
  let alloc_databuffer_port (opn,port) =
    debug_ps 3 debug_level ("Allocating data"^(buffername_of_data (opn,port)));
    match (is_precedence_port port),port.t_prt_dir with
    | false, Port.Out ->
	let dim = string_of_int port.t_prt_dim in
	alloc_macro^"("^port.t_prt_typename^","^
	(buffername_of_data (opn,port))^","^dim^")\n"
    | _ -> "" in
  
  let datas = List.map
      (fun opn -> match opn.t_opn_class with
      | Communication (Read ((sopn,sprt),_)) ->
	  [sopn,sprt]
      | Communication (Receive ((sopn,sprt),_,_,_)) ->
	  [sopn,sprt]
      | Communication (Reliable_Receive ((sopn,sprt),_,_,_)) ->
	  [sopn,sprt]
      | Calcul _ ->
	  (match is_memory opn with
	  | true ->
	      let delay_buffer = delay_buffer opn in
	      [(opn,delay_buffer)]
	  | false ->
	      List.map (fun port -> opn,port) opn.t_opn_ports)
      | _ -> [])
      opns in
  let datas = remove_copies (List.concat datas) in
  let datas_string = List.map alloc_databuffer_port datas in
  let delays = List.filter is_memory opns in
  let delay_aliases = List.map
      (fun opn ->
	List.fold_left
	  (fun l port ->
	    match port.t_prt_class <> Port.Delay_Buffer with
	    | true -> l@[opn,port]
	    | false -> l)
	  [] opn.t_opn_ports)
      delays in
  let delay_aliases = List.concat delay_aliases in
  let delay_aliases_string = List.map delay_alias delay_aliases in
  (string_of_string_list datas_string "")^"\n"^
  (string_of_string_list delay_aliases_string "")
    
let alloc_shareddata opr opns =
  let alloc sopn sprt =
    let dim = string_of_int sprt.t_prt_dim in
    indent^"alloc_("^sprt.t_prt_typename^","^(buffername_of_data (sopn,sprt))^
    "_RAM"^","^dim^")\n" in
  let opr = operator_of_operator_class opr in
  List.fold_left
    (fun s opn -> 
      let alloc_opn =
	match opn.t_opn_class with
	| Communication comclass ->
	    (match comclass with
	    | Write ((sopn,sprt),writer) when writer=opr ->
		alloc sopn sprt
	    | Read ((sopn,sprt),reader) when reader=opr ->
		alloc sopn sprt
	    | _ -> "")
	| _ -> raise (Failure "Genexec.alloc_shareddata error") in
      s^alloc_opn)
    "" opns

(** Returns the macro for a communication operation. *)    
let gen_communication_macro_name comclass operator =
  let string_of_receivers receivers =
    cut (List.fold_left
	   (fun s opr -> s^(name_of_operator (Operator opr))^",")
	   "" receivers) 1 in
  let string_of_senders senders =
    cut (List.fold_left
	   (fun s opr -> s^(name_of_operator (Operator opr))^",")
	   "" senders) 1 in
  match comclass with
  | Write (data,opr) ->
      let buffername = buffername_of_data data in
      "write_("^buffername^","^buffername^"_RAM)"
  | Read (data,opr) ->
      let buffername = buffername_of_data data in
      "read_("^buffername^"_RAM,"^buffername^")"
  | Send (data,sender,receivers) ->
      (match is_precedence_port (snd data) with
      | true ->
	  "send_synchro_("^(type_of_operator (Operator sender))^
	  ","^(name_of_operator (Operator sender))^
	  ","^(name_of_operator (Operator (List.hd receivers)))^")"
      | false -> let buffername = buffername_of_data data in
	let receivers = string_of_receivers receivers in
	"send_("^buffername^","^(type_of_operator (Operator sender))^
	","^(name_of_operator (Operator sender))^","^receivers^")")
  | Reliable_Send (data,senders,receivers) ->
      let typename,dim = (snd data).t_prt_typename,(snd data).t_prt_dim  in 
      let delta = (match operator with
            | Media mda -> (Architecture.media_reference_duration mda typename)*.(float_of_int dim)
            | _ -> raise (Failure "Fault_tolerance_genexec.delta : error")) in
      let sender = List.hd senders in         
      let receivers = string_of_receivers receivers in
      let senders   = string_of_senders senders in
      (match is_precedence_port (snd data) with
      | true -> 
	  "reliable_send_synchro_("^(type_of_operator (Operator sender))^
	  ","^senders^","^receivers^")"
      | false -> let buffername = buffername_of_data data in
	"reliable_send_("^buffername^","^(type_of_operator (Operator sender))^
	              ","^(string_of_float delta)^","^senders^","^receivers^")")
  | Reliable_Receive (data,senders,receivers,_) ->
      let typename,dim = (snd data).t_prt_typename,(snd data).t_prt_dim  in 
      let delta = (match operator with
            | Media mda -> (Architecture.media_reference_duration mda typename)*.(float_of_int dim)
            | _ -> raise (Failure "Fault_tolerance_genexec.delta : error")) in
      let sender  = List.hd senders in
      let senders = string_of_senders senders in
      let receivers = string_of_receivers receivers in
      (match is_precedence_port (snd data) with
      | true -> "reliable_recv_synchro_("^(type_of_operator (Operator sender))^
	        ","^senders^","^receivers^")"
      | false ->
	  let buffername = buffername_of_data data in
	  "reliable_recv_("^buffername^","^(type_of_operator (Operator sender))^
	  ","^(string_of_float delta)^","^senders^","^receivers^")")
  | Reliable_Sync (data,senders,receivers,_) -> 
          let sender = List.hd senders in
          let senders = string_of_senders senders in
          let receivers = string_of_receivers receivers in
	  let buffername = buffername_of_data data in
	  "reliable_sync_("^buffername^","^(type_of_operator (Operator sender))^
	  ","^senders^","^receivers^")"
  | Receive (data,sender,receivers,_) ->
      (match is_precedence_port (snd data) with
      | true -> "recv_synchro_("^(type_of_operator (Operator sender))^
	  ","^(name_of_operator (Operator sender))^
	  ","^(name_of_operator (Operator (List.hd receivers)))^")"
      | false ->
	  let buffername = buffername_of_data data in
	  "recv_("^buffername^","^(type_of_operator (Operator sender))^
	  ","^(name_of_operator (Operator sender))^
	  ","^(string_of_receivers receivers)^")")
  | Send_Synchro (sender,receiver) ->
      "send_synchro_("^(type_of_operator (Operator sender))^","^
      (name_of_operator (Operator sender))^
      ","^(name_of_operator (Operator receiver))^")"
  | Receive_Synchro (sender,receiver) ->
      "recv_synchro_("^(type_of_operator (Operator sender))^","^
      (name_of_operator (Operator sender))^
      ","^(name_of_operator (Operator receiver))^")"
  | Sync (data,sender,receivers,_) -> 
      let receivers = string_of_receivers receivers in
      "sync_("^(type_and_size_of_data data)^","^
      (type_of_operator (Operator sender))^
      ","^(name_of_operator (Operator sender))^","^receivers^")"
  | _ -> raise (Failure "Genexec.gen_communication_macro_name error")

(** Returns the macro for a calcul operations. *)	
let gen_calcul_macro_name graph operation phase =
  let gen_macro =
    List.mem phase operation.t_opn_code_phases in
  match gen_macro with
  | false -> ""
  | true ->  
      let alglib,algname = deflibname operation in 
        (*ps ((name_of_operation operation)^"  "^alglib^" "^algname);*)
      let code_name_of_operation operation =  
	let opnname = (match alglib with
	| "" -> ""
	| _ -> alglib^"_")^algname in
	match operation.t_opn_class with
	| Calcul ((Memory _),_,_) ->
	    (match phase with
	    | LoopSeq -> "memory_shift_"
	    | InitSeq | EndSeq -> opnname)
	| _ -> match operation.t_opn_origin with
	  | Explode _ -> "Explode"
	  | Implode _ -> "Implode"
	  | _ -> opnname in
      let opn_name = code_name_of_operation operation in       
      let parameters = 
	List.map
	  (fun parameter ->
	    let globalexpression path expr =
	      Algorithm.globalexpression_of_localexpression path expr in
	    let global_parameter =
	      globalexpression (parentpath_of_path (path operation)) parameter in
	    let result = Symbolic.result_of_expression !context global_parameter in
	    match result with
	    | Symbolic.RList rl ->
		(match rl with
		| [] -> ""
		| _ ->
		    let elements =
		      List.fold_left
			(fun s r -> s^","^(Symbolic.string_of_result r))
			"" rl in
		    (string_of_int (List.length rl))^elements)
	    | _ -> Symbolic.string_of_result result)
	  operation.t_opn_arguments_values in 
      let _,_,_,_,ports,_,_,_,_,_ =
	Algorithm.algorithmname_content alglib algname in
      let actualized_order_of_port port =
	match
	  List.filter
	    (fun (n,d,_,_,_,_,_) -> (n=port.t_prt_name) && (d=port.t_prt_dir))
	    ports
	with
	| (_,_,_,_,_,order,_)::_ -> order
	| _ -> port.t_prt_order in

      let in_dpds =
	List.filter
	  (fun {t_dpd_dprt=dprt;t_dpd_class=dpdclass} ->
	    match dprt.t_prt_order>=0 with
	    | true ->
		(match dpdclass with
		| _,Precedence -> false
		| _ ->
		    match operation.t_opn_class with
		    | Calcul (opnclass,_,_) ->
			(match opnclass with
			| Memory _ ->
			    (match phase with
			    | LoopSeq -> dprt.t_prt_class <> Port.Init_Memory_Port
			    | _ -> true)
			| _ -> true)
		    | _ -> true)
	    | false -> false)
	  operation.t_opn_dependences_predecessors in
      
      let in_dpds =
	List.sort
	  (fun {t_dpd_dprt=port1} {t_dpd_dprt=port2} ->
	    let order1,order2 =
	      (actualized_order_of_port port1),(actualized_order_of_port port2) in
	    compare order1 order2)
	  in_dpds in
      let in_args =
	List.fold_left
	  (fun args dpd ->
	    let opn,prt =
	      match dpd.t_dpd_sopn.t_opn_class with
	      | Communication _ -> data_of_communication dpd.t_dpd_sopn
	      | _ -> dpd.t_dpd_sopn,dpd.t_dpd_sprt in
	    let new_args =
	      (match operation.t_opn_origin,dpd.t_dpd_class with
	      | (Condition_Out _),((Condition _),_) -> []
	      | (Condition_Out _),_ ->
		  (match List.rev dpd.t_dpd_condition with
		  | (_,(Some vl))::_ -> [string_of_int vl]
		  | _ -> [])
	      | _ -> [])@[buffername_of_data (opn,prt)] in
	    args@new_args)
	  [] in_dpds in

      let out_ports =
	List.filter
	  (fun p -> (p.t_prt_dir = Port.Out) && (not (is_precedence_port p) &&
					    (p.t_prt_order>=0)))
	  operation.t_opn_ports in
      let out_ports =
	List.sort
	  (fun port1 port2 ->
	    let order1,order2 =
	      (actualized_order_of_port port1),(actualized_order_of_port port2) in
	    compare order1 order2)
	  out_ports in
      let out_args =
	List.map
	  (fun port -> buffername_of_data (operation,port))
	  out_ports in
      let out_args =
	match (is_memory operation),phase with
	| true,LoopSeq -> 
	    let path =
	      (* the second tl is to cut the application name *)
	      List.tl (List.tl operation.t_opn_path) in
	    let name = string_of_string_list path "_" in
	    let delayBufferName = name^"_buf" in
	    out_args@[delayBufferName]
	| _ -> out_args in
      let args = string_of_string_list (parameters@in_args@out_args) "," in
      opn_name^"("^args^")" 

(** Returns the macro for an operation (either communication or computation). *)
let gen_macro_name graph operation operator offset phase =
  let opn =
    match operation.t_opn_class with
    | Communication comclass -> (match comclass with 
        | Reliable_Receive (_,_,_,_) ->
                  gen_communication_macro_name comclass (Adequation_core.pi operation) 
        |   _  -> gen_communication_macro_name comclass (Adequation_core.pi operation))
    | _ -> gen_calcul_macro_name graph operation phase in
  match opn with
  | "" -> ""
  | _ -> offset^opn^"\n"

(** Returns a Suc/Pre macro. *)
let suc_pre operator opn opndpd sucpre fullempty offset =
  let sem_name = semaphore_name opn opndpd fullempty in
  let sucpre,wait_type =
    match sucpre with
    | Suc -> "Suc",(
	match is_communication opn with
	| true -> "1"
	| false -> "0")
    | Pre -> "Pre",(
	match is_communication opndpd with
	| true -> "1"
	| false -> "0") in
  let ram =
    let com =
      match is_communication opn with
      | true -> opn
      | false -> opndpd in
    match Architecture.bustype (media_of_operator_class (operator_of_operation com)) with
    | Ram,_ -> "R"
    | _ -> "" in
  let thread_name =
    match is_communication opndpd with
    | false -> ""
    | true -> ","^(gate_name opndpd) in 
  match fullempty with
    | Empty -> offset^sucpre^ram^wait_type^"_("^sem_name^thread_name^")\n" 
    | Full  -> offset^sucpre^ram^wait_type^"_("^sem_name^thread_name^")\n"    

(** Returns the successors of operation sopn on port sprt *)
let successors_of_port sopn sprt condition =
  List.fold_left
    (fun succs {t_dpd_sprt=prt;t_dpd_dopn=dopn;t_dpd_condition=dpd_cond} ->
      match prt=sprt && (not (exclusion_condition dpd_cond condition)) with
      | true -> dopn::succs
      | false -> succs)
    [] sopn.t_opn_dependences_successors

(** Returns the successors, included in operations, of operation sopn
   on port sprt *)
let successors_in_opns sopn sprt condition operations =
  intersection operations (successors_of_port sopn sprt condition)
    
(** Returns true if operation is the first operation using data
   sopn,sprt (on the operator on which operation is scheduled, in case
   of multiple operations on an operator using the same data)*)
let first_successor operation sopn sprt operations =
  let succs =
    successors_in_opns sopn sprt operation.t_opn_condition operations in
  match succs with
  | firstopn::_ -> firstopn=operation
  | _ -> raise (Failure "Genexec.first_successor error")
	
let last_successor operation sopn sprt operations =
  let succs =
    List.rev
      (successors_in_opns sopn sprt operation.t_opn_condition operations) in
  match succs with
  | firstopn::_ -> firstopn=operation
  | _ -> raise (Failure "Genexec.last_successor error")

(** Returns the operations preceeding operation which need
   synchronization with operation via semaphores *)
let preds_for_sems operation =
  let dpds = operation.t_opn_dependences_predecessors in
  let preds =
    match operation.t_opn_class with
    | Communication (Send _) | Communication (Write _) ->
	List.filter
	  (fun dpd -> dpd.t_dpd_sopn.t_opn_operator <> operation.t_opn_operator)
	  dpds
    | Communication (Reliable_Send _) ->
	List.filter
	  (fun dpd -> dpd.t_dpd_sopn.t_opn_operator <> operation.t_opn_operator)
	  dpds
    | Communication (Read _) ->
	List.filter
	  (fun {t_dpd_sopn=sopn} ->
	    match sopn.t_opn_class with
	    | Communication (Write _) -> true
	    | _ -> false)
	  dpds
    | Communication (Transfer _) ->
	raise (Failure "Genexec.suc_opn_pre error")
    | Communication _ ->
	[]
    | _ ->
	List.filter
	  (fun dpd -> dpd.t_dpd_sopn.t_opn_operator <> operation.t_opn_operator)
	  dpds in
  let preds = List.fold_left
      (fun processed pred -> 
	match List.exists 
	    (fun dpd ->
	      dpd.t_dpd_sopn = pred.t_dpd_sopn &&
	      dpd.t_dpd_sprt = pred.t_dpd_sprt &&
	      dpd.t_dpd_condition = pred.t_dpd_condition) processed with
	| true -> processed
	| false -> processed@[pred])
      [] preds in
  (*ps ("PREDS of "^(identifier_of_operation operation));
   List.iter (fun pred -> ps (string_of_dpd pred)) preds; ps"";*)
  preds 

(** Returns the operations succeeding operation which need
   synchronization with operation via semaphores *)
let succs_for_sems operation =
  let dpds = operation.t_opn_dependences_successors in
  let succs =
    match operation.t_opn_class with
    | Communication (Receive _) | Communication (Read _) ->
	List.filter
	  (fun dpd -> dpd.t_dpd_dopn.t_opn_operator <> operation.t_opn_operator)
	  dpds
    | Communication (Reliable_Receive _) ->
	List.filter
	  (fun dpd -> dpd.t_dpd_dopn.t_opn_operator <> operation.t_opn_operator)
	  dpds
    | Communication (Write _) ->
	List.filter
	  (fun {t_dpd_dopn=dopn} ->
	    match dopn.t_opn_class with
	    | Communication (Read _) -> true
	    | _ -> false) dpds
    | Communication (Transfer _) ->
	raise (Failure "Genexec.suc_opn_pre error")
    | Communication _ ->
	[]
    | _ ->
	List.filter
	  (fun {t_dpd_dopn=dopn} -> dopn.t_opn_operator <> operation.t_opn_operator)
	  dpds in
  let succs =
    fst (List.fold_left
	   (fun (dpds,oprs) ({t_dpd_dopn=dopn;t_dpd_sprt=sprt} as dpd) ->
	     let opr =
	       match operation.t_opn_class with
	       | Communication (Write _) ->
		   Operator (execution_operator_of_communication dopn)
	       | _ -> operator_of_operation dopn in
	     (* Don't forget to check the condition. In the case of successors coms
		of a condI, you may have several times the same com but with different
		conditions. Each needs a semaphore synchronization. *)
	     match operation.t_opn_origin with
	     | Condition_In _ ->
		 (match List.mem ((opr,sprt),dopn.t_opn_condition) oprs with
		 | true -> dpds,oprs
		 | false -> (dpd::dpds),(((opr,sprt),dopn.t_opn_condition)::oprs))
	     | _ -> match List.exists
		   (fun ((operator,prt),_) -> operator = opr && prt=sprt) oprs with
	       | true -> dpds,oprs
	       | false -> (dpd::dpds),(((opr,sprt),dopn.t_opn_condition)::oprs)) 
	   ([],[]) succs) in
  (*ps ("SUCCS of "^(identifier_of_operation operation));
     List.iter (fun succ -> ps (string_of_dpd succ)) succs;*)
  succs

(** Returns the conditioning macros for a suc or a pre on a conditionned
   com predecessor of a condO or successor of a condI. These macros
   correspond to the difference of condition between condI/O and dpd *)
let condIO_suc_pre_cond_macro condIO dpd offset =
  let additionnal_conditions = 
    exclusion dpd.t_dpd_condition condIO.t_opn_condition in
  if (List.length additionnal_conditions = 0) then
    (* The conditioning value is also used as a data input, no
       condition difference *)
    "","",""
  else
    if (List.length additionnal_conditions = 1) then
      let add_cond = List.hd additionnal_conditions in
      let cond_var,cond_val = match add_cond with
      | (Some c_var, Some c_val) -> c_var, c_val
      | _ -> failwith "CondO Sems_suc_pre cond_var,cond_val" in
      let cond_var_string = buffername_of_data cond_var in
      let cond_val_string = string_of_int cond_val in
      (offset^if_macro^"("^cond_var_string^","^cond_val_string^")\n"),
      (offset^endif_macro^"\n"), indent
    else
      failwith ("Additionnal condition error in CondIO_suc_pre_cond_macro, for CondIO "^
		(identifier_of_operation condIO)^" dpd "^(string_of_dpd dpd))


(** Returns suc semaphores for operation *)
let sems_suc operation preds succs operations operator offset =
  let preds_first =
    List.filter
      (fun {t_dpd_sopn=sopn;t_dpd_sprt=sprt} ->
	first_successor operation sopn sprt operations)
      preds in
  let sems_suc_full =
    List.fold_left
      (fun s ({t_dpd_sopn=sopn;t_dpd_sprt=sprt} as dpd) ->
	let if_s,endif_s,if_offset =
	  match (is_precedence_port sprt),(operation.t_opn_origin) with
	  | false,(Condition_Out _) ->
	      condIO_suc_pre_cond_macro operation dpd offset
	  | _,_ -> "","","" in
	let suc =
	  suc_pre operator operation sopn Suc Full (offset^if_offset) in
	s^if_s^suc^endif_s)
      "" preds_first in
  let sems_suc_empty =
    List.fold_left
      (fun s ({t_dpd_dopn=dopn;t_dpd_dprt=dprt} as dpd) ->
	let if_s,endif_s,if_offset =
	  match (is_precedence_port dprt),(operation.t_opn_origin) with
	  | false,(Condition_In _) ->
	      condIO_suc_pre_cond_macro operation dpd offset
	  | _,_ -> "","","" in
	let suc = suc_pre operator operation dopn Suc Empty (offset^if_offset) in
	s^if_s^suc^endif_s)
      "" succs in
  sems_suc_full^sems_suc_empty

(** Returns pre semaphores for operation *)
let sems_pre operation preds succs operations operator offset =
  let preds_last =
    List.filter
      (fun {t_dpd_sopn=sopn;t_dpd_sprt=sprt} ->
	last_successor operation sopn sprt operations)
      preds in
  let sems_pre_empty =
    List.fold_left
      (fun s  ({t_dpd_sopn=sopn;t_dpd_sprt=sprt;t_dpd_class=dpd_class} as dpd) ->
	let if_s,endif_s,if_offset =
	  match (is_precedence_port sprt),operation.t_opn_origin with
	  | false,(Condition_Out _) ->
	      (match dpd_class with
		(* This is the conditioning dependence, we don't need to add
		   a if macro *)
	      | (Condition _),_ -> "","",""
	      | _ -> condIO_suc_pre_cond_macro operation dpd offset)
	  | _,_ -> "","","" in
	let pre = suc_pre operator operation sopn Pre Empty (offset^if_offset) in
	s^if_s^pre^endif_s)
      "" preds_last in
  let sems_pre_full =
    List.fold_left
      (fun s ({t_dpd_dopn=dopn;t_dpd_dprt=dprt} as dpd) ->
	let if_s,endif_s,if_offset =
	  match (is_precedence_port dprt),(operation.t_opn_origin) with
	  | false,(Condition_In _) ->
	      condIO_suc_pre_cond_macro operation dpd offset
	  | _,_ -> "","","" in
	let suc = (suc_pre operator operation dopn Pre Full (offset^if_offset)) in
	s^if_s^suc^endif_s)
      "" succs in
  sems_pre_empty^sems_pre_full
		    
(** Returns sucs_^opn_macro^pres_ for operation opn. Used only for
   LoopSeq, for communication as well as for computation operations.*)
let gen_macro_and_sems graph opn preds succs operator opns_on_operator offset =
  let sucs = sems_suc opn preds succs opns_on_operator operator offset in
  let pres = sems_pre opn preds succs opns_on_operator operator offset in
  let macro_name = gen_macro_name graph opn operator offset LoopSeq in
  sucs^macro_name^pres

(** Returns the Suc_ and Pre_, if required, on the conditionning
   dependence of cond_variable, for the conditionned operations
   opns_conded_by_cond_variable *)
let cond_variable_pre_suc cond_variable opns_conded_by_cond_variable
    operator opns_on_operator offset =
  (* Operations are still sorted by rank *)
  let (first_opn_conded_by_cond_variable,_,first_opn_preds,_) =
    try
      List.hd opns_conded_by_cond_variable
    with Failure "hd" ->
      failwith ("Genexec.gen_macro_opns: first_opn hd error") in
  let (last_opn_conded_by_cond_variable,_,_,_) = 
    try
      List.hd (List.rev opns_conded_by_cond_variable)
    with Failure "hd" ->
      failwith ("Genexec.gen_macro_opns: last_opn hd error") in
  let cond_data =
    match is_communication first_opn_conded_by_cond_variable with
    | true -> None
    | false ->
	try
	  Some (
	  let dpd =
	    List.find (fun {t_dpd_sopn=pred} -> match pred.t_opn_class with
	    | Communication (Receive (data,_,_,_)) ->
		data = cond_variable
	    | Communication (Reliable_Receive (data,_,_,_)) ->
		data = cond_variable
	    | _ -> false) first_opn_preds in
	  dpd.t_dpd_sopn,dpd.t_dpd_sprt)
	with Not_found -> None in
  let cond_variable_suc = match cond_data with
  | None -> ""
  | Some (cond_com, cond_prt) ->
      match first_successor first_opn_conded_by_cond_variable
	  cond_com cond_prt opns_on_operator with
      | true ->
	  suc_pre operator first_opn_conded_by_cond_variable cond_com
	    Suc Full offset
      | false -> "" in
  let cond_variable_pre = match cond_data with
  | None -> ""
  | Some (cond_com,cond_prt) ->
      match last_successor last_opn_conded_by_cond_variable
	  cond_com cond_prt opns_on_operator with
      | true ->
	  suc_pre operator first_opn_conded_by_cond_variable cond_com
	    Pre Empty offset
      | false -> "" in
  cond_variable_pre,cond_variable_suc

(** Returns the macro calls for opns. In common cases, opns is
   actually only one operation but becomes a list when dealing with
   conditioned "simultaneous" operations *)
let gen_macro_opns opns counter graph operator opns_on_operator =
  (* This recursive call will either directly produce the macro_call for
     a one-level condition or imbricated if(s) for imbricated conditions
   *)
  let rec aux opns counter offset =
    match opns with
    | [] -> ""
    | _ ->
	let opns_not_conditioned,opns_conditioned =
	  List.partition
	    (fun (_,conds,_,_) -> (conds=[]) ||
	    (List.exists (fun (v,_) -> v=None) conds))
	    opns in
	let opns_not_conditioned_macros =
	  List.fold_left
	    (fun s (opn,_,preds,succs) ->
	      s^(gen_macro_and_sems graph opn preds succs operator opns_on_operator offset))
	    "" opns_not_conditioned in
	let opns_conditioned_macros =
	  match opns_conditioned with
	  | [] -> ""
	  | (_,((cond_variable_opt,_)::_),_,_)::_ ->
	      (* Starting conditioning macros with this conditionning
		 variable *)
	      let cond_variable =
		match cond_variable_opt with
		| Some v -> v
		| None ->
		    raise (Failure "Genexec.gen_macro_opns_conditionned error 1") in
	      let cond_variable_string = buffername_of_data cond_variable in
	      let opns_conded_by_cond_variable,opns_other_condition =
		List.partition
		  (fun (_,conds,_,_) ->
		    match conds with
		    | [] ->
			raise (Failure "Genexec.gen_macro_opns_conditionned error 2")
		    | (v,_)::_ -> v=cond_variable_opt)
		  opns_conditioned in
	      let opns_with_cond_values =
		List.map
		  (fun (opn,conds,preds,succs) ->
		    let cond_value =
		      List.assoc cond_variable_opt conds in
		    let conds_without_cond_variable =
		      List.remove_assoc cond_variable_opt conds in
		    (* We don't want cond_variable to be in the Suc_
		       of opn as it will be a Suc_ common to all the
		       operations conditionned by cond_variable
		       instead. However this is for the computation
		       seq only, it won't do anything in the
		       communication seq. *)
		    let preds_without_cond_variable =
		      List.filter
			(fun {t_dpd_sopn=opn} -> match opn.t_opn_class with
			| Communication (Receive (data,_,_,_)) ->
			    data <> cond_variable
			| Communication (Reliable_Receive (data,_,_,_)) ->
			    data <> cond_variable
			| _ -> true)
			preds in
		    cond_value,(opn,conds_without_cond_variable,preds_without_cond_variable,succs))
		  opns_conded_by_cond_variable in
	      let cases =
		remove_copies (List.map fst opns_with_cond_values) in
	      let cond_variable_pre, cond_variable_suc =
		cond_variable_pre_suc cond_variable opns_conded_by_cond_variable
		  operator opns_on_operator offset in
	      let opns_conded_by_cond_variable_macros = match cases with
	      | (Some vl)::[] ->
		  (* Generating if_/then_ *)
		  let opns_conded_by_cond_variable =
		    List.map snd opns_with_cond_values in
		  let opns_conded_by_cond_variable_macros =		      
		    aux opns_conded_by_cond_variable counter (offset^indent) in
		  cond_variable_suc^
		  offset^if_macro^"("^cond_variable_string^","^(string_of_int vl)^")\n"^
		    opns_conded_by_cond_variable_macros^offset^endif_macro^"\n"^
		    cond_variable_pre
	      | _ ->
		  (* Generating switch_ *)
		  let opns_grouped_by_cond_values =
		    List.fold_left (fun l (vl,opn) ->
		      match List.mem_assoc vl l with
		      | true ->
			  let others = List.assoc vl l in
			  (vl,(others@[opn]))::(List.remove_assoc vl l)
		      | false -> (vl,[opn])::l)
		      [] opns_with_cond_values in
		  let opns_grouped_and_sorted_by_cond_values =
		    List.sort (fun vl1 vl2 -> compare vl1 vl2) opns_grouped_by_cond_values in
		  let all_cases_opns_macros =
		    List.fold_left
		      (fun s (vl,this_case_opns) ->
			match vl with
			| Some vl ->
			    let this_case_opns_macros =
			      aux this_case_opns counter (offset^indent^indent) in
			    s^offset^indent^case_macro^"("^(string_of_int vl)^
			    ")\n"^this_case_opns_macros^offset^indent^endcase_macro^"\n"
			| _ -> raise (Failure "Genexec.gen_macro_opns : error 3"))
		      "" opns_grouped_and_sorted_by_cond_values in
		  cond_variable_suc^
		  offset^switch_macro^"("^cond_variable_string^")\n"^all_cases_opns_macros^
		  offset^endswitch_macro^"\n"^
		  cond_variable_pre in
	      let opns_other_condition_macros =
		aux opns_other_condition counter offset in
	      opns_conded_by_cond_variable_macros^opns_other_condition_macros
	  | _ -> raise (Failure "Genexec.gen_macro_opns error 4") in
	opns_not_conditioned_macros^opns_conditioned_macros in
  aux opns counter (indent^indent)
    
(** generate semaphores only for original preds for  Reliable_send operations *) 
let filter_original_dpds opn dpds = 
  match opn.t_opn_class with 
    |  Communication (Reliable_Send (data,senders,receivers)) ->
          List.filter (fun {t_dpd_sopn=sopn}-> is_original sopn) dpds    
    | _ -> dpds

(** Returns the loop macro sequence for operator which contains
   operations. This is as well for the communication as for the calcul
   sequence. In both cases it consists in a list of
   (sucs_,opn_macros_,pres_) *)
let gen_loop_seq graph operator opns_on_operator chrono chrono_counter =   
  (*ps (List.fold_left (fun s opn -> s^"  +  "^(name_of_operation opn)) 
	((name_of_operator operator)^"  opns  =  ") opns_on_operator); ps "";*)
  let chrono counter counter_label = 
    match chrono with
      | true -> (indent^indent^chrono_macro^"("^(string_of_int counter)^
		 ","^counter_label^")\n"),(counter+1)
      | false -> "",counter in

  let rec process opns opns_string counter =
    match opns with
      | [] -> opns_string,counter
      | ((o1,_,_,_) as opn)::others ->
	  let smltns,others =
	    List.partition
	      (fun (o2,_,_,_) ->
		 simultaneous o1.t_opn_esfs (eefs o1) o2.t_opn_esfs (eefs o2))
	      others in
	  let smltns = opn::smltns in

	  let opns_code = gen_macro_opns smltns counter graph operator opns_on_operator in

	  let opr_label = (name_of_operator operator) in
	  let gate_label = gate_name o1 in
	  let opns_label =
	    string_of_string_list (List.map
				     (fun (o,_,_,_) -> name_of_operation o)
				     smltns) "_" in
	  let counter_label =
	    opr_label^"_"^(match gate_label with
			     |	"" -> ""
			     |	_ -> gate_label^"_")^opns_label in
	  let chrono,counter = chrono counter counter_label in	
	    process others (opns_string^opns_code^chrono) counter in

  let opns_on_operator =
    List.map
      (fun opn ->
         (*ps (List.fold_left (fun s dpd -> s^" , "^(name_of_operation dpd.t_dpd_sopn))  (name_of_operation opn) (preds_for_sems opn));ps "";*)
	 (opn,opn.t_opn_condition,(filter_original_dpds opn (preds_for_sems opn)),(succs_for_sems opn)))
      opns_on_operator in
    process opns_on_operator "" chrono_counter

(** Returns the download macros for operator through gate. Can be used
   for init as well as for end sequence *)
let gen_download_string operator gate phase =
  let from_macro,to_macro =
    match phase with
    | InitSeq -> loadfrom_macro,loaddownto_macro
    | EndSeq -> saveupto_macro,savefrom_macro
    | _ ->
	raise (Failure "Genexec.gen_download_string error") in
  let from_string =
    match Hashtbl.mem load_from operator with
    | true ->
	let father_gate,father_opr =
	  Hashtbl.find load_from operator in
	(match father_gate = gate with
	| true ->
	    let comms_string =
	      (match Hashtbl.mem load_downto operator with
	      | true ->
		  let comms =
		    List.map
		      (fun (comm,_) -> comm)
		      (Hashtbl.find load_downto operator) in
	 	  List.fold_left
		    (fun s comm -> s^","^comm)
		    "" comms
	      | false -> "") in
	    indent^from_macro^"("^father_opr^comms_string^")\n"
	| false -> "")
    | false -> "" in
  let to_string =
    match Hashtbl.mem load_downto operator with
    | true ->
	let sons =
	  Hashtbl.find load_downto operator in
	(match List.mem_assoc gate sons with
	| true ->
	    let sons = List.assoc gate sons in
	    let sons = List.sort compare sons in
	    let sons_string =
	      List.fold_left (fun s son -> s^","^son) "" sons in
	    let father_gate =
	      match Hashtbl.mem load_from operator with
	      | true -> fst (Hashtbl.find load_from operator)
	      | false -> "" in
	    indent^to_macro^"("^father_gate^sons_string^")\n"
	| false -> "")
    | false -> "" in
  from_string^to_string
		 
(** Returns the init phase macros for the communication sequence of operator
   concerning media *)
let gen_seq_communication_init graph operator media =
  let opns = operations_of_operator graph media in
  let opns_comm =
    operations_of_operator_media graph operator media in
  debug_ps 2 debug_level "Opns comm are";
  List.iter
    (fun opn -> debug_ps 2 debug_level (name_of_operation opn))
    opns_comm;
  let sems =
    List.fold_left
      (fun sems operation ->
	let sems =
	  match operation.t_opn_class with
	  | Communication (Receive _) ->
	      let (opndata,_) = data_of_communication operation in
	      (match (is_memory opndata) with
	      | true -> sems@[(suc_pre operator operation opndata Pre Full indent)]
	      | false -> sems)
	  | Communication (Reliable_Receive _) ->
	      let (opndata,_) = data_of_communication operation in
	      (match (is_memory opndata) with
	      | true -> sems@[(suc_pre operator operation opndata Pre Full indent)]
	      | false -> sems)
	  | Communication (Send _) ->
	      let (opndata,_) = data_of_communication operation in
	      (match (is_memory opndata) with
	      | true -> sems@[(suc_pre operator operation opndata Pre Empty indent)]
	      | false -> sems)
	  | Communication (Reliable_Send _) ->
	      let (opndata,_) = data_of_communication operation in
	      (match (is_memory opndata) with
	      | true -> sems@[(suc_pre operator operation opndata Pre Empty indent)]
	      | false -> sems)
	  | _ -> sems in
	let preds =
	  match operation.t_opn_class with
	  | Communication (Read _) -> operation.t_opn_dependences_predecessors
	  | _ ->
	      List.filter
		(fun {t_dpd_sopn=sopn} ->
		  sopn.t_opn_operator <> operation.t_opn_operator &&
		  (not (is_memory sopn)))
		operation.t_opn_dependences_predecessors in
	let preds =
	  List.map (fun {t_dpd_sopn=sopn} -> sopn) preds in
	sems@(List.map
		(fun sopn -> suc_pre operator operation sopn Pre Empty indent)
		preds))
      [] opns_comm in
  (string_of_string_list (remove_copies sems) "")^"\n"
						     
(** Returns the loop phase macros for the communication sequence of
   operator concerning media *)
let gen_seq_communication_loop graph operator media chrono chrono_counter =
  let coms_operator_media =
    operations_of_operator_media graph operator media in
  let coms_operator_media_sorted =
    List.sort
      (fun {t_opn_rank = rank1} {t_opn_rank = rank2} -> compare rank1 rank2)
      coms_operator_media in
  let communication_seq,chrono_counter =
    gen_loop_seq graph operator coms_operator_media_sorted chrono chrono_counter in
  (indent^"loop_\n"^communication_seq^indent^"endloop_\n"),chrono_counter

(** [gen_seq_communication_end] returns the end part of the
communication sequence for [media] on [opr]. *)
let gen_seq_communication_end opr media chrono =
  end_thread_macro^"\n"

(** [gen_communication_seq graph operator gate media chrono
   chrono_counter] returns the communication sequence for [media] on
   [operator]. *)
let gen_communication_seq graph operator gate media chrono chrono_counter =
  let links = Architecture.links !archilib !archiname media in
  let oprmain = ref "" in
  let recv_oprs =
    List.fold_left
      (fun l (_,(opr,_)) ->
	let oprname = (name_of_operator opr) in
	match oprname = (Architecture.operator_main_get !archilib !archiname) with
	| true -> oprmain := oprname; l
	| false -> l@[oprname])
      [] links in
  (* This should be generalized. This is usefull to have the same
     order in LoadDnto macro *)
  let recv_oprs = !oprmain::(List.sort compare recv_oprs) in
  let recv_oprs = string_of_string_list recv_oprs "," in
  let communication_header =
    thread_macro^"("^(type_of_operator media)^","^
    gate.gte_name^","^recv_oprs^")\n" in
  let code_download =
    gen_download_string (name_of_operator operator) gate.gte_name InitSeq in
  let communication_init =
    gen_seq_communication_init graph operator media in
  let seq,chrono_counter =
    gen_seq_communication_loop graph operator media chrono chrono_counter in
  let chrono_save =
    gen_download_string (name_of_operator operator) gate.gte_name EndSeq in
  let communication_end =
    gen_seq_communication_end operator media chrono in
  (communication_header^code_download^communication_init^seq^chrono_save^
   communication_end),chrono_counter

(** Returns the included libs macros *)
let included_libs () =
  let libraries = "syndex"::"reliability"::(Application.included_libs ()) in
  let libraries = List.map (fun lib -> "include("^lib^".m4x)dnl") libraries in 
  (string_of_string_list libraries "\n")^"\n\n"

(** Returns the date string *)
let date () =
  let align_number nb =
    let s = string_of_int nb in
    match String.length s with
    | 1 -> "0"^s
    | _ -> s in
  let tm = Unix.localtime (Unix.time ()) in
  (string_of_int tm.Unix.tm_mday)^"/"^(string_of_int (tm.Unix.tm_mon+1))^"/"^
  (string_of_int (tm.Unix.tm_year+1900))^" "^(align_number tm.Unix.tm_hour)^
  ":"^(align_number tm.Unix.tm_min)^":"^(align_number tm.Unix.tm_sec)

(** Returns the header macros for an operator macro code file *)
let gen_header application_name opr =
  let inclu = included_libs ()
  and proc =
    processor_macro^"("^(type_of_operator opr)^","^(name_of_operator opr)^
    ","^application_name^",\n"
  and stamp = "SynDEx-"^Version.version^" (c)INRIA 2002, "^(date ())^"\n)\n\n" in
  inclu^proc^stamp

let gen_end () = endprocessor_macro

(** Returns either (depending on [phase] value)the init or the end part
of the main (compute) sequence of an operator *)
let gen_seq_compute_initend graph operator chrono phase =
  let oprdef = (operator_of_operator_class operator).oprref_definition in
  let macro_thread,thread_name = match phase with
  | InitSeq -> spawn_thread_macro,""
  | EndSeq -> wait_thread_macro,semaphore_thread_name
  | _ -> raise (Failure "Genexec.gen_seq_compute_initend error") in
  let processor_init = match List.mem InitSeq oprdef.oprdef_code_phases with
  | true -> indent^processor_init_macro^"\n"
  | false -> "" in
  let processor_end = match List.mem EndSeq oprdef.oprdef_code_phases with
  | true -> indent^processor_end_macro^"\n"
  | false -> "" in

  let nghbrs = Architecture.links !archilib !archiname operator in
  let thread =
    List.fold_left
      (fun s ((_,gate),(media,_)) ->
	s^indent^macro_thread^"("^thread_name^
	(gate_of_gateoption gate).gte_name^")\n")
      "" nghbrs in
  let opns =
    List.filter
      (fun o -> (is_calcul o) && (not (is_implantation o)))
      (operations_of_operator graph operator) in
  (* I'm not sure that sorting is necessary *)
  let opns_sorted =
    List.sort
      (fun {t_opn_rank = rank1} {t_opn_rank = rank2} -> compare rank1 rank2)
      opns in
  let initend = 
    List.fold_left
      (fun s operation ->
	s^(gen_macro_name graph operation operator indent phase))
      "" opns_sorted in
  let chrono = match chrono with
  | true -> indent^chrono_macro^"\n"
  | false -> "" in
  match phase with 
  | InitSeq -> processor_init^chrono^thread^initend
  | EndSeq -> initend^thread^chrono^processor_end
  | _ -> raise (Failure "Genexec.gen_seq_compute_initend error")

(** Returns the init part of the main (compute) sequence of an operator *)
let gen_seq_compute_init graph operator opns_comp chrono =
  let init_thread =
    gen_seq_compute_initend graph operator chrono InitSeq in
  debug_ps 2 debug_level "Opns comp are";
  List.iter
    (fun opn -> debug_ps 2 debug_level (name_of_operation opn))
    opns_comp;
  let mem_inits =
    List.fold_left
      (fun inits operation -> 
	let preds_mem =
	  List.filter
	    (fun {t_dpd_sopn=sopn;t_dpd_class=(_,dpdclass)} -> 
	      let is_memory_data =
		match sopn.t_opn_class with
		| Communication (Receive _) | Communication (Sync _) ->
		    is_memory (fst (data_of_communication sopn))
		| Communication (Reliable_Receive _) ->
		    is_memory (fst (data_of_communication sopn))
		| _ -> false in
	      is_memory_data)
	    operation.t_opn_dependences_predecessors in
	let preds_mem =
	  List.map
	    (fun {t_dpd_sopn=sopn} -> fst (data_of_communication sopn))
	    preds_mem in
	inits@(List.map
		 (fun opn -> gen_macro_name graph opn operator indent InitSeq)
		 preds_mem))
      [] opns_comp in
  let mem_inits = (string_of_string_list (remove_copies mem_inits) "\n") in
  let sems =
    List.fold_left
      (fun sems operation ->
	let preds =
	  List.filter
	    (fun {t_dpd_sopn=sopn;t_dpd_class=(_,dpdclass)} ->
	      (sopn.t_opn_operator <> operation.t_opn_operator) &&
	      (not (is_memory (fst (data_of_communication sopn)))))
	    operation.t_opn_dependences_predecessors in
	let preds = List.map (fun {t_dpd_sopn=sopn} -> sopn) preds in
	sems@(List.map
		(fun sopn -> suc_pre operator operation sopn Pre Empty indent)
		preds))
      [] opns_comp in
  let init_pres = (string_of_string_list (remove_copies sems) "")^"\n" in
  
  init_thread^mem_inits^init_pres
			   
(** Returns the end part of the main (compute) sequence of an operator *)
let gen_seq_compute_end graph operator chrono =
  gen_seq_compute_initend graph operator chrono EndSeq

(** Returns the loop part of the main (compute) sequence of an operator *)
let gen_seq_compute_loop graph operator opns_comp chrono chrono_counter =
  (*ps "gen seq compute ................";*)
  let opns_comp = 
    List.filter
      (fun {t_opn_class=opnclass} ->
	match opnclass with
	| Calcul (Constant,_,_) -> false
	| _ -> true)
      opns_comp in
  let compute_seq,chrono_counter =
    gen_loop_seq graph operator opns_comp chrono chrono_counter in
  (indent^"loop_\n"^compute_seq^indent^"endloop_\n"),chrono_counter

(** Returns the main (compute) sequence for an operator. *)
let gen_compute_seq graph opr opns_comp chrono chrono_counter =
  let iniseq = 
    gen_seq_compute_init graph opr opns_comp chrono in
  let seq,chrono_counter = 
    gen_seq_compute_loop graph opr opns_comp chrono chrono_counter in
  let seqend = gen_seq_compute_end graph opr chrono in
  (compute_begin_macro^iniseq^seq^seqend^compute_end_macro),chrono_counter

(** Returns the macro code for an operator. *)
let genexec_opr application_name graph operator chrono chrono_counter_init =
  debug_ps 1 debug_level ("generating code for "^(name_of_operator operator)^
			  "..................");
  let nghbrs =
    Architecture.links !archilib !archiname operator in
  let opns_alloc_data_comm =
    List.map
      (fun (_,(media,_)) -> operations_of_operator_media graph operator media)
      nghbrs in
  let opns_alloc_data_comm = List.concat opns_alloc_data_comm in
  let opns_alloc_data_comp = operations_of_operator graph operator in
  let code_header = gen_header application_name operator in
  debug_ps 1 debug_level ("header generated");
  let alloc_sem =
    alloc_semaphores nghbrs opns_alloc_data_comm in
  debug_ps 1 debug_level "semaphores allocated";
  let alloc_data =
    alloc_databuffer operator
      (opns_alloc_data_comm @ opns_alloc_data_comp) in
  debug_ps 1 debug_level "data buffers allocated";
  let alloc_shareddata =
    List.fold_left
      (fun s (_,(mda,_)) ->
	let m = media_of_operator_class mda in
	match Architecture.bustype m with
	| Ram,_ ->
	    let opns = operations_of_operator graph mda in
	    let alloc_mda = alloc_shareddata operator opns in
	    let alloc_shared_sem = alloc_shared_semaphores opns in
	    s^"shared_("^(name_of_operator mda)^")\n"^
	    alloc_shared_sem^alloc_mda^"endshared_\n"
	| _ -> "")
      "" nghbrs in
  debug_ps 1 debug_level "shared data allocated"; 
  let communication_seq,chrono_counter = 
    List.fold_left
      (fun (s,counter) ((_,gate),(media,_)) ->
	let communication_media_seq,counter =
	  gen_communication_seq graph operator
	    (gate_of_gateoption gate) media chrono counter in
	(s^communication_media_seq^"\n"),counter)
      ("",chrono_counter_init) nghbrs in
  debug_ps 1 debug_level "communication seq done";
  let opns_alloc_data_comp_sorted =
    List.sort
      (fun {t_opn_rank = rank1} {t_opn_rank = rank2} -> compare rank1 rank2)
      opns_alloc_data_comp in
    (*ps (List.fold_left (fun s opn -> (name_of_operation opn)^"  "^s) "opns = " opns_alloc_data_comp_sorted);*)
  let compute_seq,chrono_counter = 
    gen_compute_seq graph operator opns_alloc_data_comp_sorted chrono chrono_counter in
  debug_ps 1 debug_level "computation seq done";
  let alloc_chrono =
    match chrono with
    | true ->
	chrono_macro^"("^
	(string_of_int (chrono_counter - chrono_counter_init))^")\n"
    | false -> "" in
  
  let code_end = gen_end () in
  (code_header^"\n"^alloc_sem^alloc_shareddata^"\n"^alloc_data^"\n"^alloc_chrono^
   media_failures_macro^"("^(string_of_int !nmf)^")\n"^
   processors_failures_macro^"("^(string_of_int !npf)^")\n\n"^
   communication_seq^compute_seq^code_end^"\n"),chrono_counter 
    
(** Mainly, builds the load_tree *)
let init graph =
  let alib,aname = Application.archi_main_get () in
  archilib := alib;
  archiname := aname;
  (*  let implantation_opns = hashtbl_filter is_implantation graph in*)
  context := Algorithm.context_global ();
  load_tree ()

(** [shift_rank graph operator threshold shift] shifts the ranks of all
   operations on operator by [shift] if their rank is superior to
   [threshold] *)
let shift_rank graph operator threshold shift =
  let opns = operations_of_operator graph operator in
  List.iter
    (fun opn ->
      match opn.t_opn_rank >= threshold with
      | true -> opn.t_opn_rank <- opn.t_opn_rank + shift
      | false -> ())
    opns

(** Returns the receives corresponding to send *)
let recvs_of_send send =
  let succs = successors send in
  List.filter
    (fun com ->
      match com.t_opn_class with
      | Communication (Receive _) -> true
      | Communication (Reliable_Receive _) -> true
      | _ -> false)
    succs
    
(** Returns the send corresponding to recv *)
let send_of_recv recv =
  let preds = predecessors recv in
  try
    List.find
      (fun com ->
	match com.t_opn_class with
	| Communication (Send _) -> true
	| Communication (Reliable_Send _) -> true
	| _ -> false)
      preds
  with _ -> failwith "send_recv"

(** Adds precedences between opns to make them in a full order in terms
   of precedence *)
let rec synchronize_precedence opns = 
  match opns with
    (* Remember that operations are sorted *)
  | opn1::opn2::tl ->
      let opn1_out,opn2_in =
	(find_precedence_port opn1 Port.Out), (find_precedence_port opn2 Port.In) in
      Adequationtypes.dependence_add opn1 opn1_out opn2 opn2_in
	(Data,Precedence) opn1.t_opn_condition false;
      synchronize_precedence (opn2::tl)
  | _ -> ()

(** Adds a Send_synchro to signal that o2's wait for o1 is over *)
let send_synchro s1 o1 o2 graph =
  let portout =
    {t_prt_name="synchro_out";
     t_prt_dir=Port.Out;
     t_prt_typename="int";
     t_prt_dim=1;
     t_prt_class=Port.Data_Port;
     t_prt_order=0} in
  let rank = s1.t_opn_rank + 1 in
  shift_rank graph (operator_of_operation s1) rank 1;
  let comclass = Send_Synchro (o1,o2) in
  let send_name = name_of_comclass comclass in
  let path = [send_name] in
  let send = new_opn path (Communication comclass) []
      ([portout]@(precedence_ports ())) [] [] s1.t_opn_condition
      s1.t_opn_operator rank (eefs s1) false (Ihm path) None ""
      "" "" [InitSeq;LoopSeq;EndSeq] in
  Hashtbl.add graph (identifier_of_operation send) send;
  let send_in = find_precedence_port send Port.In in
  send,portout

(** Adds a Recv_synchro for o2 to wait for o1 *)
let recv_synchro pred portout succ o1 o2 graph =
  let rank = match succ with
  | Some opn -> opn.t_opn_rank
	(* This is the last communication but we need a synchronization
	   between the infinite repetitions of the graph. +2, don't forget
	   the last receive which follows the send_synchro *)
  | None -> pred.t_opn_rank+2 in
  let comclass = Receive_Synchro (o1,o2) in
  let recv_name = name_of_comclass comclass in
  let portin = {portout with t_prt_name="synchro_in";t_prt_dir=Port.In} in
  shift_rank graph (operator_of_operation pred) rank 1;
  let path = [recv_name] in
  let recv = new_opn path (Communication comclass) []
      ([portin]@(precedence_ports ())) [] [] pred.t_opn_condition
      pred.t_opn_operator rank pred.t_opn_esfs false (Ihm path)
      None "" "" "" [InitSeq;LoopSeq;EndSeq] in
  Hashtbl.add graph (identifier_of_operation recv) recv;
  Adequationtypes.dependence_add pred portout recv portin
    (Data,Strong_Precedence_Data) pred.t_opn_condition false

(** Adds a Reliable Sync operation for operation opn in graph *)
let sync opn portout senders receivers executor graph =
  (*ps (name_of_operation opn);
  ps (List.fold_left (fun s opr -> s^""^(name_of_operator (Operator opr))) "senders = " senders);
  ps (List.fold_left (fun s opr -> s^""^(name_of_operator (Operator opr))) "receviers = " receivers);ps"";*)
  let rank = opn.t_opn_rank+1 in 
  let data = data_of_communication opn in
  let comclass = Reliable_Sync (data,senders,receivers,executor) in
  let recv_name = name_of_comclass comclass in
  let portin = {portout with t_prt_name="synchro_in";t_prt_dir=Port.In} in
  shift_rank graph (operator_of_operation opn) rank 1;
  let path = [recv_name] in
  let recv = new_opn path (Communication comclass) []
      ([portin]@(precedence_ports ())) [] [] opn.t_opn_condition
      opn.t_opn_operator rank opn.t_opn_esfs false (Ihm path)
      None "" "" "" [InitSeq;LoopSeq;EndSeq] in
  Hashtbl.add graph (identifier_of_operation recv) recv;
  Adequationtypes.dependence_add opn portout recv portin
    (Data,Strong_Precedence_Data) opn.t_opn_condition false

(** Adds synchronizations to graph for a sam multipoint without
   broadcast. This consists in Send_synchro and Recv_synchro operations
   and also in additionnal precedences between communications*)
let synchronize_sammp_nobroadcast sends graph =
  debug_ps 1 debug_level "NoBroadcast synchronization";
  let rec f sends_oprs = match sends_oprs with
  | (s1,o1)::(s2,o2)::tl ->
      (match o1 <> o2 with
      | true ->
	  let send_synchro,portout = send_synchro s1 o1 o2 graph in
	  recv_synchro send_synchro portout (Some s2) o1 o2 graph
      | false -> ());
      f ((s2,o2)::tl)
  | _ -> () in
  let sends_oprs =
    List.map
      (fun opn -> opn,(execution_operator_of_communication opn))
      sends in
  f sends_oprs;
  (* Check if we add a synchronization between the graph infinite
     repetitions *)
  let opr_first,(send_last,opr_last) =
    (snd (List.hd sends_oprs)),(List.hd (List.rev sends_oprs)) in
  match opr_first <> opr_last with
  | true ->
      let send_synchro,portout =
	send_synchro send_last opr_last opr_first graph in
      recv_synchro send_synchro portout None opr_last opr_first graph
  | false -> ()

(** Adds synchronization to graph for a sam multipoint with broadcast. This
   consists in Sync operations and also in additionnal precedences
   between communications*)
let synchronize_sammp_broadcast sends graph archilib archiname =
  debug_ps 1 debug_level "Broadcast synchronization";
  let synchronize_sammp_broadcast_send send =
    match send.t_opn_class with
    | Communication (Reliable_Send (data,senders,receivers)) ->
	(*let operator = execution_operator_of_communication send in*)
	let operators_connected_to_medium =
	  Architecture.links archilib archiname (operator_of_operation send) in
	let operators_media =
	  List.map
	    (fun (_,(opr,_)) -> operator_of_operator_class opr)
	    operators_connected_to_medium in
	(*let operator_neighbours = List.filter ((<>) operator) operators_media in*)
        let operator_neighbours = exclusion operators_media senders in
	Hashtbl.remove graph (identifier_of_operation send); 
	let comclass = Reliable_Send (data,senders,receivers) in
	send.t_opn_class <- Communication comclass;
	let sendname =
	  unique_identifier graph [] (name_of_comclass comclass) in
	send.t_opn_path <- [sendname];
	Hashtbl.add graph (identifier_of_operation send) send;
	let portsent = snd data in
	let is_port_out p =
	  p.t_prt_dir=Port.Out && (match (is_precedence_port portsent) with
	  | true -> (is_precedence_port p)
	  | false -> not (is_precedence_port p)) in
	let portout = List.hd (List.filter is_port_out send.t_opn_ports) in
	let recvs_operators =
	  List.map
	    execution_operator_of_communication
	    (recvs_of_send send) in
	let operators_to_synchronize =
	  exclusion operator_neighbours recvs_operators in
	List.iter
	  (fun opr -> sync send portout senders recvs_operators opr graph)
	  operators_to_synchronize
    | _ ->
	raise (Failure "Transformation.synchronize_sammp_broadcast_send error") in
  List.iter synchronize_sammp_broadcast_send sends

(** Adds synchronizations on medium. This consists in Send_cynchro,
   Recv_synchro and Sync but also in additionnal precedences between
   communications *)
let synchronize_transfer_media archilib archiname graph medium =    
  debug_ps 1 debug_level ("Synchronizing medium "^(name_of_operator medium)^
			  "..........................");
  let operators_archi =
    List.map
      operator_of_operator_class
      (Application.operators_list archilib archiname) in
  let opns = operations_of_operator graph medium in
  let sends =
    List.filter
      (fun com -> match com.t_opn_class with
      | Communication (Send _) -> true
      | Communication (Reliable_Send _) -> true
      | _ -> false)
      opns in
  match sends with
  | [] -> ()
  | _ ->
      let sends =
	List.sort
	  (fun {t_opn_rank=r1} {t_opn_rank=r2} -> compare r1 r2)
	  sends in
      (match Architecture.bustype (media_of_operator_class medium) with
      | SamMP,true ->
	  synchronize_sammp_broadcast sends graph archilib archiname
      | SamMP,false -> synchronize_sammp_nobroadcast sends graph
      | _ -> ());
      List.iter
	(fun operator_to_synchronize ->
	  let coms =
	    operations_of_operator_media graph
	      (Operator operator_to_synchronize) medium in
	  let coms_sorted =
	    List.sort
	      (fun {t_opn_rank= rank1} {t_opn_rank = rank2} ->
		compare rank1 rank2)
	      coms in
	  synchronize_precedence coms_sorted)
	operators_archi
	
(** For communications on the media of graph, add corresponding send
   and receive operations on operator source and destination *)
let synchronize graph =
  let archilib,archiname = Application.archi_main_get () in
  let media = Application.media_list archilib archiname in
  (*  List.iter (sendrecv_of_transfer_media graph) media;*)
  List.iter (synchronize_transfer_media archilib archiname graph) media;
  debug_ps 1 debug_level "Graph synchronized";
  graph

(** Returns the macros header for an architecture file (.m4m). *)
let gen_header_architecture application_name =
  let inclu = "include(syndex.m4m)dnl\n"
  and architecture =
    architecture_macro^"("^application_name^",\nSynDEx-"^Version.version^
    " (c)INRIA 2002, "^(date ())^"\n)\n\n" in
  inclu^architecture

(** [gen_operators_architecture oprs opr_main] returns the macros
   describing the set operators [oprs] preceded by the macro for the
   main operator [opr_main] *)
 let gen_operators_architecture oprs opr_main = 
 let gen_operator (name,typelib,typename,_) = 
 let  _,_,gates,_,_n,_ = 
    Architecture.operator_definitionname_content typelib typename in 
  let gates = string_of_string_list (List.fold_left (fun l (gtype,gname) -> l@[gtype;gname]) [] gates) "," in
   processor_macro^"("^typename^","^name^","^gates^")\n" in 
 let  opr_main,opr_others = 
   List.partition (fun (opr,_,_,_) -> opr=opr_main) oprs in 
  let oprs = opr_main@(List.sort compare opr_others) in 
  List.fold_left (fun s opr -> s^(gen_operator opr)) ""  oprs

(** Returns the macros describing a list of connections. *)
let gen_connection_architecture cncs =
  let rec process toprocess processed =
    match toprocess with
    | [] -> processed
    | (_,_,mdaname)::_ ->
	let cncs_mda,others =
	  List.partition (fun (_,_,m) -> m=mdaname) toprocess in
	let _,typemdalib,typemdaname,_,_ =
	  Architecture.media_referencename_content !archilib !archiname mdaname in
	let oprmain,gatemain = (ref ""),(ref "") in
	(* List of opr,gate sorted by opr, with oprmain first *)
	let mda =
	  List.fold_left
	    (fun l (opr,gate,_) ->
	      match opr = (Architecture.operator_main_get !archilib !archiname) with
	      | true -> oprmain:= opr;
		  gatemain:=gate; l
	      | false ->l@[(opr,gate)])
	    [] cncs_mda in
	let mda = List.sort (fun (opr1,_) (opr2,_) -> compare opr1 opr2) mda in
	let init_val =
	  match !oprmain with
	  | "" -> [typemdaname;mdaname]
	  | _ -> [typemdaname;mdaname;!oprmain;!gatemain] in
	let mda =
	  List.fold_left
	    (fun l (opr,gate) -> l@[opr;gate])
	    init_val mda in
	process others (mda::processed) in
  let mdas = process cncs [] in
  List.fold_left
    (fun s mda -> s^connect_macro^"("^(string_of_string_list mda ",")^")\n")
    "" mdas

(** [architecture_file application_name] generates the architecture file
   (.m4m) for application [application_name]. *)
let architecture_file application_name =
  let _,_,oprlist,opr_main,_,cnclist,_,_ =
    Architecture.architecturename_content !archilib !archiname in
  let header_architecture = gen_header_architecture application_name
  and processors_architecture = gen_operators_architecture oprlist opr_main
  and connections_architecture = gen_connection_architecture cnclist in
  header_architecture^processors_architecture^"\n"^connections_architecture^
  "\n"^end_architecture_macro

(** [generate_code application_name graph chrono] generates code files
   for application [application_name], which adequation is [graph] with
   chronometrics if required by [chrono]. *)
let generate_code application_name graph chrono =
  debug_ps 1 debug_level "Generating code";
  remove_false_status graph;
  init graph;
  let graph = synchronize graph in
  let oprs = Application.operators_list !archilib !archiname in
  let dirname = (Filename.dirname application_name)^"/" in
  let basename = Filename.basename (Filename.chop_extension application_name) in
  let code_of_opr counter opr =
    let code,counter = genexec_opr basename graph opr chrono counter in
     	  (*ps ("generated code for "^(name_of_operator opr)); *)
    file_write (filename_of_opr dirname opr) code;
    counter in
  ignore (List.fold_left (fun counter -> code_of_opr counter) 0 oprs);
  let architecture_file = architecture_file basename in
  file_write (filename_of_name dirname basename) architecture_file

(** [generated_files application_name] returns the list of names of code
files generated for application [application_name]. *)
let generated_files application_name =
  let archilib,archiname = Application.archi_main_get () in
  let oprs = Application.operators_list archilib archiname in
  let directory = (Filename.dirname application_name)^"/" in
  List.map (filename_of_opr directory) oprs
