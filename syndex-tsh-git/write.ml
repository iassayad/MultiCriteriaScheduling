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

open Types

let indent = " "

let string_of_coord2d c =
  match c with
  | Coord.Coord2d (a,b) -> (string_of_int a)^","^(string_of_int b)
  | Coord.No_coord2d -> ""

let string_of_position c =
  match c with
  | Coord.Coord2d (a,b) -> "@"^(string_of_int a)^","^(string_of_int b)
  | Coord.No_coord2d -> ""  

let token_of_code_phase code_phase =
  match code_phase with
  | InitSeq -> "initseq"
  | LoopSeq -> "loopseq"
  | EndSeq -> "endseq"

let string_of_code_phases code_phases =
  match code_phases with
  | [] -> ""
  | _ -> (List.fold_left
	    (fun s cphase -> s^" "^(token_of_code_phase cphase))
	    "code_phases: " code_phases)^";\n"
      
let string_of_repetition rep =
  match rep with
  | (Specified n) when n > 1 -> "["^(string_of_int n)^"]"
  | _ -> ""

let string_of_ports ports =
  let string_of_port (prt_name,_,dt,dt_dim,prt_class,prt_rank,prt_position) dir =
    match prt_class with
    | Port.Precedence_Port -> ""
    | _ ->
	let init = match prt_class with
	  |	Port.Init_Memory_Port -> "init_memory_port "
	  |	_ -> "" in
	  indent^dir^" "^dt^(string_of_dimension dt_dim)^" "^prt_name^" "^init^
	  (string_of_int prt_rank)^" "^
	  (string_of_position prt_position)^";\n" in
  let portsin,portsout =
    List.partition (function _,d,_,_,_,_,_ -> d=Port.In) ports in
  let string_of_ports p d =
    List.fold_left
      (function s -> function p -> s^(string_of_port p d))
      "" p in
    (string_of_ports portsin "?")^(string_of_ports portsout "!")

let string_of_range (a,b) =
  "["^(Symbolic.string_of_expression a)^".."^
  (Symbolic.string_of_expression b)^"] "

let string_of_port_ref (r,p) =
  match r with
  | "" -> p
  | _ -> r^"."^p

let string_of_condition (var, vle) =
  match var,vle with
  | "",0 -> "false;\n"
  | "",1 -> "true;\n"
  | _ -> var^" = "^(string_of_int vle)^";\n"

let string_of_def_description d =
  match d with
  | "" -> ""
  | _ -> "description : \""^d^"\"\n"

let string_of_ref_description d =
  match d with
  | "" -> ""
  | _ -> " \""^d^"\"\n"

let string_of_reference (ref_name, algo_lib, algo_name, args, condition, pos,rep,description) =
  indent^(string_of_ref_type algo_lib algo_name)^(string_of_argsvalues args)^" "^(string_of_repetition rep)^" "^ref_name^" "^(string_of_position pos)^(string_of_ref_description description)^";\n"

let string_of_dpd_class dpd_class =
  match dpd_class with
  | Strong_Precedence_Data -> "strong_precedence_data"
  | Weak_Precedence_Data -> "weak_precedence_data"
  | Precedence -> "precedence"
  | Data -> "data"

let string_of_dependence (source, destination, dpd_class, condition) =
  let f = match dpd_class with
    | Precedence -> fst
    | _ -> string_of_port_ref in
    indent^(string_of_dpd_class dpd_class)^" "^(f source)^" -> "^(f destination)^";\n"
      
let string_of_alg_class alg_class =
  match alg_class with
  | Constant -> "constant"
  | Sensor -> "sensor"
  | Actuator -> "actuator"
  | Memory _ -> "memory"
  | Operation -> "algorithm"
  | Internal -> "internal"

let string_of_internal (name, args, ports) =
  "def internal "^name^" "^(string_of_argsnames args ";")^":\n"^(string_of_ports ports)^"\n"

let string_of_constant (name, args, ports, dim, desc) =
  "def constant "^name^" "^(string_of_argsnames args ";")^" "^(string_of_coord2d dim)^":\n"^(string_of_ports ports)^(string_of_def_description desc)^"\n"

let string_of_sensor (name, args, ports, dim, desc) =
  "def sensor "^name^" "^(string_of_argsnames args ";")^" "^(string_of_coord2d dim)^":\n"^(string_of_ports ports)^(string_of_def_description desc)^"\n"

let string_of_actuator (name, args, ports, dim, desc) =
  "def actuator "^name^" "^(string_of_argsnames args ";")^" "^(string_of_coord2d dim)^":\n"^(string_of_ports ports)^(string_of_def_description desc)^"\n"

let string_of_memory (name, args, ports, dim, desc, ab) =
  "def memory "^name^" "^(string_of_range ab)^(string_of_argsnames args ";")^" "^(string_of_coord2d dim)^":\n"^(string_of_ports ports)^(string_of_def_description desc)^"\n"

let string_of_dpds_refs_by_cond refs dpds =
  let g cond lr ld s =
    let lr1,lr2 = List.partition (function (_,_,_,_,c,_,_,_) -> c=cond) lr
    and ld1,ld2 = List.partition (function (_,_,_,c) -> c=cond) ld in
    let cs = "conditions: "^(string_of_condition cond) in
    let rs = List.fold_left (function s -> function r -> s^(string_of_reference r)) "references:\n" lr1 in
    let ds = List.fold_left (function s -> function d -> s^(string_of_dependence d)) "dependences:\n" ld1 in
      lr2,ld2,s^cs^rs^ds in
  let rec f (lr,ld,s) = match lr,ld with
    | [],[] -> s
    | (_,_,_,_,cond,_,_,_)::_,_ -> f (g cond lr ld s)
    | [],(_,_,_,cond)::_ -> f (g cond lr ld s) in
    f (refs,dpds,"")

let string_of_algorithm (lib, name, algotype, args, ports, refs, dpds, dim, desc, code_phases) =
  match algotype with
  | Internal -> string_of_internal (name, args, ports)
  | Constant -> string_of_constant (name, args, ports, dim, desc)
  | Sensor -> string_of_sensor (name, args, ports, dim, desc)
  | Actuator -> string_of_actuator (name, args, ports, dim, desc)
  | Memory ab -> string_of_memory (name, args, ports, dim, desc, ab)
  | Operation ->
      let conds_refs_dpds = string_of_dpds_refs_by_cond refs dpds in
      let code_phases_s = string_of_code_phases code_phases in
	"def algorithm "^name^" "^(string_of_argsnames args ";")^" "^(string_of_coord2d dim)^":\n"^
	(string_of_ports ports)^conds_refs_dpds^code_phases_s^(string_of_def_description desc)^"\n"
	  
let algo_string () =
  let algo_list = (List.filter (function (l,_) -> l = "")) (Algorithm.algo_list ()) in
  let algo_refs = List.map (function (l,n) ->
    let (_,_,_,_,_,refs,_,_,_,_) = Algorithm.algorithmname_content l n in
    let rs = List.map (function (_,rl,rn,_,_,_,_,_) -> rl,rn) refs in (l,n),rs) algo_list in
  let rec sort_algos algos_not_sorted algos_sorted = match algos_not_sorted with
  | [] -> algos_sorted
  | _ -> let refdefineds,others = List.partition 
				    (function a -> (intersection (List.assoc a algo_refs) algos_not_sorted)=[])
				    algos_not_sorted in
    sort_algos others (algos_sorted @ refdefineds) in
  let algo_list_sorted = sort_algos algo_list [] in
  List.fold_left (function s -> function (_,aname) ->
		    s^(string_of_algorithm (Algorithm.algorithmname_content "" aname))) "" algo_list_sorted

let string_of_gates gates =
  List.fold_left (function s -> function (media,name) -> s^"gate "^media^" "^name^";") "" gates

(** return a string from a list of durations, 
  drs is ((libname,operation_name), float) *)
let string_of_durations drs =
  let string_of_duration ((lib,name),duration) = 
    (string_of_ref_type lib name)^" = "^(string_of_float duration)^";\n" in
    List.fold_left (function s -> function e -> s^(string_of_duration e)) "" drs

let string_of_operator (lib, name, gates, durations, desc, code_phases) =
  let gts = string_of_gates gates
  and drs = string_of_durations durations
  and code_phases_s = string_of_code_phases code_phases in
    "def operator "^name^" :\n"^gts^"\n"^drs^code_phases_s^(string_of_def_description desc)^"\n"
      
let string_of_media (lib, name, bustype, durations, desc) =
  let bt = match bustype with
  | SamPP -> "sampp"
  | SamMP -> "sammp"
  | Ram -> "ram" in
  let drs = string_of_durations durations in
  "def media "^name^" :\n"^bt^";\n"^drs^(string_of_def_description desc)^"\n"

let string_of_operator_reference (name, deflib, defname, pos) =
  indent^(string_of_ref_type deflib defname)^" "^name^" "^(string_of_position pos)^";\n"

let string_of_media_reference (name, deflib, defname, broadcast, pos) =
  let bt = match broadcast with
  | true -> "broadcast"
  | false -> "no_broadcast" in
  indent^(string_of_ref_type deflib defname)^" "^name^" "^bt^" "^(string_of_position pos)^";\n"

let string_of_connection (oprname, gatename, mdaname) =
  indent^oprname^"."^gatename^" "^mdaname^";\n"

let string_of_operator_main o =
  match o with
  | "" -> ""
  | _ -> " main operator "^o^";\n"

let string_of_architecture (lib, name, oprs, oprmain, mdas, cncs, dim, desc) =
  let oprs = List.fold_left (function s -> function o -> s^(string_of_operator_reference o)) "operators:\n" oprs
  and mdas = List.fold_left (function s -> function m -> s^(string_of_media_reference m)) "medias:\n" mdas
  and cncs = List.fold_left (function s -> function c -> s^(string_of_connection c)) "connections:\n" cncs in
  "def architecture "^name^" "^(string_of_coord2d dim)^":\n"^oprs^(string_of_operator_main oprmain)^mdas^cncs^(string_of_def_description desc)^"\n"

let archi_string () =
  let oprdef_list = List.filter (function (l,_) -> l = "") (Architecture.operatortypes_list ())
  and mdadef_list = List.filter (function (l,_) -> l = "") (Architecture.mediatypes_list ())
  and archi_list = List.filter (function (l,_) -> l = "") (Architecture.architectures_list ()) in
  let oprs = List.fold_left (function s -> function (_,oname) -> s^(string_of_operator (Architecture.operator_definitionname_content "" oname))) "" oprdef_list
  and mdas = List.fold_left (function s -> function (_,mname) -> s^(string_of_media (Architecture.media_definitionname_content "" mname))) "" mdadef_list
  and arcs = List.fold_left (function s -> function (_,aname) -> s^(string_of_architecture (Architecture.architecturename_content "" aname))) "" archi_list in
  oprs^mdas^arcs

let main_algo_string () =
  match Application.algo_main_get () with
  | "", "", [] -> ""
  | lib, name, args -> "main algorithm "^(string_of_ref_type lib name)^" "^(string_of_argsvalues args)^";\n"

let main_archi_string () =
  match Application.archi_main_get () with
  | "", "" -> ""
  | lib, name -> "main architecture "^(string_of_ref_type lib name)^";\n"

(** returns a string with the durations of local operations, for every local operator/media *)
let string_of_extradurations () =
   let opr_list = List.filter (function (l,_) -> l <> "") (Architecture.operatortypes_list ()) in
   let oprdef_list = List.map (function (olib,oname) -> Architecture.operator_definitionname_content olib oname) opr_list in
   let extradrs = List.map (function (olib,oname,_,drs,_,_) -> (olib,oname,(List.filter (function ((lib,_),_) -> lib="") drs))) oprdef_list in
   let drsoprs = List.fold_left (function s -> function (olib,oname,drs) -> match drs with
   | [] -> s
   | _ -> s^"extra_durations_operator "^(string_of_ref_type olib oname)^" :\n"^(string_of_durations drs)) "" extradrs in
   let mda_list = List.filter (function (l,_) -> l <> "") (Architecture.mediatypes_list ()) in
   let mdadef_list = List.map (function (mlib,mname) -> Architecture.media_definitionname_content mlib mname) mda_list in
   let extradrs = List.map (function (mlib,mname,_,drs,_) -> (mlib,mname,(List.filter (function ((lib,_),_) -> lib="") drs))) mdadef_list in
   let drsmdas = List.fold_left (function s -> function (mlib,mname,drs) -> match drs with
   | [] -> s
   | _ -> s^"extra_durations_media "^(string_of_ref_type mlib mname)^" :\n"^(string_of_durations drs)) "" extradrs in
   drsoprs^drsmdas


let string_of_reference_path refpath =
  string_of_string_list (""::refpath) "\\"
(*  List.fold_left (function s -> function e -> s^"\\"^e) "" refpath*)

let string_of_attachtype attachtype =
  match attachtype with
  | AttachAll -> "attach_all"
  | AttachRef -> "attach_ref"
  | AttachCondI -> "attach_condi"
  | AttachCondO -> "attach_condo"
  | AttachExplode -> "attach_explode"
  | AttachImplode -> "attach_implode"

let string_of_ref_attachement (ref_path, attachement_type) =
  "["^(string_of_reference_path ref_path)^","^(string_of_attachtype attachement_type)^"]\n"

let string_of_software_component xscname =
  List.fold_left
    (fun s (rp,atype) -> s^string_of_ref_attachement (rp,atype))
    ("software_component "^xscname^" :\n")
    (Application.refs_of_xsc xscname)

let xsc_string () =
  (* This will produce an awkward return before a semi-colon on a new line. *)
  (* Yet, this is required for ascending compability because in former versions, there were no returns between each ref path *)
  List.fold_left (fun s xscname -> s^"\n"^(string_of_software_component xscname)^";\n") "" (Application.xsc_namelist ())

let string_of_constraint refpath oprlist =
  "constraint : "^(string_of_reference_path refpath)^" on "^(List.fold_left (function s -> function ((archilib,archiname),oprname) -> s^(string_of_ref_type archilib archiname)^"."^oprname^" ") "" oprlist)^";\n"

let string_of_absolute_constraint xscname oprlist =
  "absolute constraint : "^xscname^" on "^(List.fold_left (function s -> function ((archilib,archiname),oprname) -> s^(string_of_ref_type archilib archiname)^"."^oprname^" ") "" oprlist)^";\n"

let string_of_relative_constraint relativetype xscnamelist =
  let rtype = match relativetype with
  | Union -> "union"
  | Disjunction -> "disjunction" in
  "relative constraint : "^rtype^" "^(string_of_string_list xscnamelist " ")^";\n"
(*  "relative constraint : "^rtype^" "^(List.fold_left (function s -> function xscname -> s^xscname^" ") "" xscnamelist)^";\n"*)

let constraints_string () =
  let ctrnt_list = Application.constraints_list () in
  let ctrnt = List.fold_left (function s -> function (rp,oprlist) -> s^(string_of_constraint rp oprlist)) "" ctrnt_list in
  let abs_list = Application.xsc_absolute_constraints_list () in
  let abs = List.fold_left (function s -> function (xscname,oprlist) -> s^(string_of_absolute_constraint xscname oprlist)) "" abs_list in
  let rel_list = Application.xsc_relative_constraints_list () in
  let rel = List.fold_left (function s -> function (reltype,xsclist) -> s^(string_of_relative_constraint reltype xsclist)) "" rel_list in
  ctrnt^abs^rel

let included_libs_string () =
  List.fold_left (function s -> function l -> s^"include \""^l^".sdx\";\n") "" (Application.included_libs ())

let application_string () =
  let version = "syndex_version : \""^(Version.version)^"\"\n"
  and description = "application description : \""^(Application.get_application_description ())^"\"\n"
  and included_libs = "\n# Libraries\n"^(included_libs_string ())
  and algo = "\n# Algorithms\n"^(algo_string ())
  and archi = "\n# Architectures\n"^(archi_string ())
  and main = "\n# Main Algorithm / Main Architecture\n"^(main_algo_string ())^(main_archi_string ())
  and extradurations = "\n# Extra durations\n"^(string_of_extradurations ())
  and xsc = "\n# Software components\n"^(xsc_string ())
  and constraints = "\n# Constraints\n"^(constraints_string ()) in
    version^description^included_libs^algo^archi^main^extradurations^xsc^constraints

open Adequationtypes

let adeq_string_of_comclass com_class =
  let string_of_receivers receivers =
    (cut (List.fold_left (function s -> function opr -> s^(name_of_operator (Operator opr))^",") "(" receivers) 1)^")" in
  let string_of_senders senders =
    (cut (List.fold_left (function s -> function opr -> s^(name_of_operator (Operator opr))^",") "(" senders) 1)^")" in
  match com_class with
  | Transfer (data,sender,receivers) -> "" (* Not supported yet *)
  | Write (data,writer) -> "write "^(name_of_operator (Operator writer))^" "^(name_of_data data)
  | Read (data,reader) -> "read "^(name_of_operator (Operator reader))^" "^(name_of_data data)
  | Send (data,sender,receivers) -> "send "^(name_of_operator (Operator sender))^" "^(string_of_receivers receivers)^
      " "^(name_of_data data)
  | Receive (data,sender,receivers,executor) -> "receive "^(name_of_operator (Operator sender))^
      " "^(string_of_receivers receivers)^" "^(name_of_operator (Operator executor))^" "^(name_of_data data)^" "

  | Reliable_Send (data,senders,receivers) -> "Reliable_send "^(string_of_senders senders)^" "^(string_of_receivers receivers)^
      " "^(name_of_data data)
  | Reliable_Receive (data,senders,receivers,executor) -> "Reliable_receive "^(string_of_senders senders)^
      " "^(string_of_receivers receivers)^" "^(name_of_operator (Operator executor))^" "^(name_of_data data)^" "
 | Reliable_Sync (data,senders,receivers,executor) -> "sync "^(string_of_senders senders)^
      " "^(string_of_receivers receivers)^" "^(name_of_operator (Operator executor))^" "^(name_of_data data)^" "
  | Send_Synchro (sender,receiver) -> "send_synchro "^(name_of_operator (Operator sender))^
      " "^(name_of_operator (Operator receiver))
  | Receive_Synchro (sender,receiver) -> "receive_synchro "^(name_of_operator (Operator sender))^
      " "^(name_of_operator (Operator receiver))
  | Sync (data,sender,receivers,executor) -> "sync "^(name_of_operator (Operator sender))^" "^(string_of_receivers receivers)^
      " "^(name_of_operator (Operator executor))^" "^(type_and_size_of_data data)

let adeq_string_of_operation_class opn_class =
  let class_str =
    match opn_class with
    | Calcul (alg_class,alg_lib,alg_name) -> "calcul "^(string_of_alg_class alg_class)^" "^(string_of_ref_type alg_lib alg_name)
    | Communication com_class -> "communication "^(adeq_string_of_comclass com_class) in
    class_str^" "

(*let string_of_pred_scheduled opn =
  match opn.t_opn_class with
  | Communication _ -> ""
  | Calcul _ -> match opn.t_opn_pred_scheduled with
    | Some o -> identifier_of_operation o
    | None -> ""*)

let adeq_string_of_port port dir =
  match (is_precedence_port port) with
  | true -> ""
  | false -> let dim_value = port.t_prt_dim in
    let dim_str = "["^(string_of_int dim_value)^"]" in
      indent^dir^" "^port.t_prt_typename^" "^dim_str^" "^port.t_prt_name^" "^(string_of_int port.t_prt_order)^";\n"

let adeq_string_of_ports ports =
  let ports_in,ports_out = List.partition (fun port -> port.t_prt_dir = Port.In) ports in
  let ports_in_str = List.fold_left (fun s port -> s^(adeq_string_of_port port "?")) "" ports_in in
  let ports_out_str = List.fold_left (fun s port -> s^(adeq_string_of_port port "!")) "" ports_out in
    "ports:\n"^ports_in_str^ports_out_str

let adeq_string_of_dpd_class dpd_class =
  match dpd_class with
  | Condition i, dclass -> (string_of_dpd_class dclass)^" "^"cond_level = "^(string_of_int i)
  | Data, dclass -> string_of_dpd_class dclass

let adeq_string_of_dependence dpd =
  indent^(adeq_string_of_dpd_class dpd.t_dpd_class)^" "^(string_of_dpd dpd)^
  " conditions: "^(string_of_condlist dpd.t_dpd_condition)^";\n"

let string_of_opn_origin opn_origin =
  let origin_str =
  match opn_origin with
  | Ihm p -> "ihm "^(identifier_of_path p)
  | Condition_In p -> "condI "^(identifier_of_path p)
  | Condition_Out p -> "condO "^(identifier_of_path p)
  | Explode p -> "explode "^(identifier_of_path p)
  | Implode p -> "implode "^(identifier_of_path p)
  | Synchro_Constant p -> "synchro_constant "^(identifier_of_path p)
  | Diffuse (_,_) | Iterate (_,_) | Fork (_,_) | Join (_,_) ->
      failwith "Error: adequation not supported with No Repetition Flatten mode" in
    "("^origin_str^")"

let adeq_string_of_operator operation =
  match operation.t_opn_operator with
  | Some ((Operator opr) as opclass) -> "operator "^(name_of_operator opclass)
  | Some ((Media opr) as opclass) -> "media "^(name_of_operator opclass)
  | None -> failwith "Error: an operation is not scheduled on any operator: "^(identifier_of_operation operation) 

let adeq_condition_string_of_operation opn =
  let opn_condition = "conditions: "^string_of_condlist opn.t_opn_condition in
    (identifier_of_operation opn)^" "^opn_condition

let adeq_dependence_string_of_operation opn =
  List.fold_left (fun s dpd -> s^(adeq_string_of_dependence dpd)) "" opn.t_opn_dependences_successors

let adeq_operation_string_of_operation opn =
  let opn_path = identifier_of_path opn.t_opn_path in
  let opn_class = adeq_string_of_operation_class opn.t_opn_class in
  let opn_args = string_of_argsvalues opn.t_opn_arguments_values in
  let opn_origin = string_of_opn_origin opn.t_opn_origin in
  let opn_operator = adeq_string_of_operator opn in
  let opn_rank = string_of_int opn.t_opn_rank in
  let opn_adequation_order = string_of_int opn.t_opn_adequation_order in
  let opn_esfs = string_of_float opn.t_opn_esfs in
  let opn_ports = adeq_string_of_ports opn.t_opn_ports in
    "operation_scheduled "^opn_path^":\n"^opn_class^opn_args^" "^opn_origin^
    ";\nscheduled: "^opn_operator^" "^opn_rank^" "^opn_esfs^" "^opn_adequation_order^" "^
    "\n"^opn_ports^"\n"
      
(** Returns a string corresponding to adequation adequation_graph.
  The string is rather precise, in order to be able to reload the adequation *)
let string_of_adequation adequation_graph =  
  let operations = list_of_hashtbl adequation_graph in
  let opns_calc = hashtbl_filter (fun opn -> is_calcul opn) adequation_graph in
  let opns_com =  hashtbl_filter (fun opn -> is_communication opn) adequation_graph in
  let operations_sorted = opns_calc@opns_com in
    (* When loading back, it will be easier to first create operations and then add the conditions and dependences *)
    (* Indeed, conditions and dependences contain fields which directly "point" on operations (t_dpd_sopn, etc.) *)
    (* Therefore, we seperate operation_string and dependence_string. *)
    (* For the same kind of reasons, we first process calcul operations and then communication operations *)
  let operations_string = List.fold_left (fun s opn -> s^(adeq_operation_string_of_operation opn))
			    "schedules:\n\n" operations_sorted in
  let conditions_string = List.fold_left (fun s opn -> s^(adeq_condition_string_of_operation opn))
			    "schedule_conditions:\n\n" operations in
  let dependences_string = List.fold_left (fun s opn -> s^(adeq_dependence_string_of_operation opn))
			     "schedule_dependences:\n\n" operations in
    operations_string^dependences_string

(** Returns a string corresponding to the schedule contained in graph.
  This is a simplified version of the adequation graph, which will be easier to read for a user,
  but which can't be used to reload the adequation. *)
let string_of_schedule schedules =
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
	"#"^indent^(identifier_of_operation operation)^time_string^cond_string^";\n" in
  let string_of_operator opr =
    match opr with
    | Operator o -> "#operator "^o.oprref_name
    | Media m -> "#media "^m.mdaref_name in
  let string_of_oprschedule oprschedule =
    let opns = Hashtbl.fold (fun _ {t_sch_operations=cond_opns} opns -> opns@cond_opns) oprschedule [] in
    let opns_sorted = List.stable_sort (function opn1,_,_ -> function opn2,_,_ -> compare opn1.t_opn_rank opn2.t_opn_rank) opns in
      List.fold_left (fun s (operation,esfs,eefs) ->
			s^(string_of_operation operation esfs eefs operation.t_opn_condition)) "" opns_sorted in
    Hashtbl.fold (fun _ (opr,oprschedule) total ->
		    total^(string_of_operator opr)^":\n"^(string_of_oprschedule oprschedule)) schedules ""

let application_save filename (save_adequation,adequation_graph) = 
  let full_adequation_string = match (save_adequation,adequation_graph) with
    | true,(Some graph, Some schedules) ->
	let schedule_string = "\n#Schedules:\n"^(string_of_schedule schedules) in
	let adequation_string = "\n# Adequation result\n\n"^(string_of_adequation graph) in
	  schedule_string^adequation_string
    | _ -> "" in
    file_write filename ((application_string ())^full_adequation_string)
