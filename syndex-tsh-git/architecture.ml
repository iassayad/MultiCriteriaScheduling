(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                          Christophe Macabiau                          *)
(*                            Julien Forget                              *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(** This modules contains basis function to access/modify the data structures concerning the architecture part *)
open Types


(** Returns the operator reference named oprname in architecture archilib/archiname *)
let operator_reference archilib archiname oprname =
  let arc = Application.architecture archilib archiname in
  try
    List.find (function {oprref_name = n} -> n = oprname) arc.archi_operators
  with Failure "Not_found" -> failwith ("Operator "^oprname^" doesn't exist.")

(** Returns the medium reference named mdaname in architecture archilib/archiname *)
let media_reference archilib archiname mdaname =
  let arc = Application.architecture archilib archiname in
  try
    List.find (function {mdaref_name = n} -> n = mdaname) arc.archi_medias
  with Failure "Not_found" -> failwith ("Media "^mdaname^" doesn't exist.")

(** Returns a list of gates corresponding to the list gates *)
let gates_of_string gates =
  List.map (function (gtype,gname) -> {gte_media_type = gtype ;gte_name = gname}) gates

(** Adds durations durations to operator definition lib/name *)
let operator_durations_add (lib,name) durations =
  let opr_durations = (Application.operatordef lib name).oprdef_operation_durations in
    List.iter (fun ((olib,oname),dn) -> Hashtbl.replace opr_durations (olib,oname) dn) durations

(** Sets durations of operator definition lib/name to durations. *)
let operator_durations_save lib name durations =
  let opr_durations = (Application.operatordef lib name).oprdef_operation_durations in
    Hashtbl.clear opr_durations;
    operator_durations_add (lib,name) durations

(** Adds durations durations to medium definition lib/name *)
let media_durations_add (lib,name) durations =
  let mda_durations = (Application.mediadef lib name).mdadef_operation_durations in
    List.iter (fun ((olib,oname),dn) -> Hashtbl.replace mda_durations oname (dn,olib)) durations

(** Sets durations of medium definition lib/name to durations. *)
let media_durations_save lib name durations =
  let mda_durations = (Application.mediadef lib name).mdadef_operation_durations in
    Hashtbl.clear mda_durations;
    media_durations_add (lib,name) durations

(** Sets gates of operator definition lib/name to gates. *)
let operator_gates_save lib name gates =
  let oprdef = Application.operatordef lib name
  and gates = gates_of_string gates in
  let gatestoadd = exclusion gates oprdef.oprdef_gates in
  oprdef.oprdef_gates <- (intersection oprdef.oprdef_gates gates) @ gatestoadd;
  List.iter (function arc ->
    arc.archi_connections <- List.filter (function {cnc_operator=opr;cnc_gate=gate} -> match opr.oprref_definition=oprdef with
    | true -> List.mem gate oprdef.oprdef_gates
    | false -> true) arc.archi_connections) Application.application.app_architectures

(** Sets operator oprlib/oprname code phases to code_phases *)
let operator_modify_code_phases oprlib oprname code_phases =
  let oprdef = Application.operatordef oprlib oprname in
    oprdef.oprdef_code_phases <- code_phases

(** Returns a new operator definition with given field values. Creates included gates.
  Raises failure if operator lib/name already exists *)
let operator_definition_create lib name gates_list durations_list desc code_phases =
  match List.exists (function o -> o.oprdef_library=lib && o.oprdef_name = name) Application.application.app_operator_definitions with
  | true -> failwith ("Operator "^name^" defined twice : ignoring second definition.")
  | false -> 
      let gates = gates_of_string gates_list in
      let oprt = {
	oprdef_library = lib;
	oprdef_name = name;
	oprdef_gates = gates;
	oprdef_operation_durations = Hashtbl.create 30;
	oprdef_description = desc;
	oprdef_code_phases = code_phases
      } in
	Application.application.app_operator_definitions <- oprt :: Application.application.app_operator_definitions;
	operator_durations_save lib name durations_list

(** Returns a new medium definition with given field values.
  Raises failure if medium lib/name already exists *)
let media_definition_create lib name bt durations_list desc =
  match List.exists (function m -> m.mdadef_library = lib && m.mdadef_name = name) Application.application.app_media_definitions with
  | true -> failwith ("Media "^name^" defined twice : ignoring second definition.")
  | false ->
      let mdat = {
	mdadef_library = lib;
	mdadef_name = name;
	mdadef_bus_type = bt;
	mdadef_operation_durations = Hashtbl.create 30;
	mdadef_description = desc;
      } in
	Application.application.app_media_definitions <- mdat :: Application.application.app_media_definitions;
	media_durations_save lib name durations_list

(** Returns a new operator reference with given field values. *)
let operator_reference_create arc_other_operators (name, (oprlib, oprname), pos) =
  let oprdef = Application.operatordef oprlib oprname in
  {
   oprref_name = name;
   oprref_definition = oprdef;
   oprref_position = pos;
   oprref_id = -1;
   oprref_links2 = Array.make 0 0;
   oprref_neighbours = []
 }

(** Returns a new medium reference with given field values. *)
let media_reference_create arc_other_medias (name, (mdalib,mdaname), bc, pos) =
  let mdadef = Application.mediadef mdalib mdaname in
  {
   mdaref_name = name;
   mdaref_definition = mdadef;
   mdaref_broadcast = bc;
   mdaref_position = pos;
   mdaref_links2 = Array.make 0 0;
   mdaref_neighbours = []
 }

(** Sets operator reference oprname as main operator of architecture archilib/archiname. *)
let operator_main_set (archilib,archiname) oprname =
  let arc = Application.architecture archilib archiname in
  let opr = operator_reference archilib archiname oprname in
  arc.archi_operator_main <- Some opr

(** Sets operator reference of architecture archilib/archiname to None. *)
let operator_main_clear archilib archiname =
  let arc = Application.architecture archilib archiname in
  arc.archi_operator_main <- None

(** Returns main operator reference name of archi archilib/archiname *)
let operator_main_get archilib archiname  =
  let arc = Application.architecture archilib archiname in
  match arc.archi_operator_main with
  | None -> ""
  | Some opr -> opr.oprref_name

(** Returns the value of each field of gate gte. *)
let gate_content gte =
  (gte.gte_media_type,gte.gte_name)

(** Returns the content of a duration. *)
let duration_content (opnt,d) =
  ((opnt.opn_library,opnt.opn_name),d)

(** Returns the value of each field of operator definition opr. *)
let operator_definition_content opr =
  let gates = List.map gate_content opr.oprdef_gates in
  let drs = Hashtbl.fold (fun (olib,oname) duration drs -> ((olib,oname),duration)::drs) opr.oprdef_operation_durations [] in
  (opr.oprdef_library, opr.oprdef_name, gates, drs, opr.oprdef_description, opr.oprdef_code_phases)

(** Returns the value of each field of operator reference opr. *)
let operator_reference_content opr =
  (opr.oprref_name, opr.oprref_definition.oprdef_library, opr.oprref_definition.oprdef_name, opr.oprref_position)

(** Returns the value of each field of medium definition mdat. *)
let media_definition_content mdat =
  let drs = Hashtbl.fold (fun oname duration drs -> (((snd duration),oname),(fst duration))::drs) mdat.mdadef_operation_durations [] in
  (mdat.mdadef_library, mdat.mdadef_name, mdat.mdadef_bus_type, drs, mdat.mdadef_description)

(** Returns the value of each field of medium reference mda. *)
let media_reference_content mda =
  (mda.mdaref_name, mda.mdaref_definition.mdadef_library, mda.mdaref_definition.mdadef_name, mda.mdaref_broadcast, mda.mdaref_position)

(** Returns the value of each field of medium reference named mdaname in archi archilib/archiname. *)
let media_referencename_content archilib archiname mdaname =
  let mda = media_reference archilib archiname mdaname in
  media_reference_content mda

(** Returns the value of each field of operator reference named oprname in archi archilib/archiname. *)
let operator_referencename_content archilib archiname oprname =
  let opr = operator_reference archilib archiname oprname in
  operator_reference_content opr

(** Returns the value of each field of operator definition oprlib/oprname. *)
let operator_definitionname_content oprlib oprname =
  let opr = Application.operatordef oprlib oprname in
  operator_definition_content opr

(** Returns the value of each field of medium definition mdalib/mdaname. *)
let media_definitionname_content mdalib mdaname =
  let mda = Application.mediadef mdalib mdaname in
  media_definition_content mda

(** Returns the value of each field of connection cnc. *)
let connection_content cnc =
  (cnc.cnc_operator.oprref_name, cnc.cnc_gate.gte_name, cnc.cnc_media.mdaref_name)

(** Returns the value of each field of architecture arc. Calls content functions on subcomponents (operators, media, etc). *)
let architecture_content arc =
  let oprlist = List.map operator_reference_content arc.archi_operators
  and opr_main = operator_main_get arc.archi_library arc.archi_name
  and mdalist = List.map media_reference_content arc.archi_medias
  and cnclist = List.map connection_content arc.archi_connections in
  (arc.archi_library, arc.archi_name, oprlist, opr_main, mdalist, cnclist, arc.archi_dimension_window, arc.archi_description)

(** Returns the value of each field of architecture lib/name. Calls content functions on subcomponents (operators, media, etc). *)
let architecturename_content lib name =
  let arc = Application.architecture lib name in
  architecture_content arc

(** Returns a boolean, true if creating this connection is allowed plus, if false, the message justifying unconnectability. *)
let connectable arclib arcname (orefname,ogate) mrefname =
  (* These include media to media or operator to operator connections attempts*)
  try
    ignore(operator_reference arclib arcname orefname);
    ignore(media_reference arclib arcname mrefname);
    let (_,_,_,_,_,cnclist,_,_) = architecturename_content arclib arcname in
      (* Forbid two connections on the same gate *)
    match (List.exists (function (n,p,_) -> n = orefname && p=ogate)) cnclist with
    | true -> (false,"Gate already connected")
    | false ->
          (* Forbid connections if media and operator gate have different types *)
	let (_,mdeflib,mdefname,_,_) = media_referencename_content arclib arcname mrefname in
	let (_,odeflib,odefname,_) = operator_referencename_content arclib arcname orefname in
	let (_,_,gates,_,_,_) = operator_definitionname_content odeflib odefname in
	let (ogate_type,_) = List.find (function (_,gtename) -> gtename = ogate) gates in
	match ogate_type = mdefname with
	| false -> (false,"Operator gate and media definition have different types (names)")
	| true ->
              (* Forbid mutiple connection on a SamPP bus *)
	    let (_,_,mbustype,_,_) = media_definitionname_content mdeflib mdefname in
	    match mbustype with 
	    | SamPP -> 
		(match List.length (List.filter (function (_,_,mdref) -> mdref = mrefname) cnclist) < 2 with
		| false -> (false,"Point to point media already connected to two operators")
		| true -> (true,""))
	    | _ -> (true,"")
  with Failure s -> (false,s)

(** Returns a new connection with given field values *)
let connection_create arclib arcname ((oprname,gatename),mdaname) operators medias =
  let opr = List.find (function {oprref_name = n} -> n = oprname) operators in
  let gate = List.find (function {gte_name = n} -> n = gatename) opr.oprref_definition.oprdef_gates in
  let mda = List.find (function {mdaref_name = n} -> n = mdaname) medias in  
  {
   cnc_operator = opr;
   cnc_gate = gate;
   cnc_media = mda
 }

(** Adds a new media reference to archilib/archiname.
 Raises Failure if media name already exists in this architecture *)
let media_reference_add archilib archiname (name,(mdalib,mdaname),broadcast,pos) =
  let arc = Application.architecture archilib archiname in
  match List.exists (function m -> m.mdaref_name = name) arc.archi_medias with
  | true -> failwith ("Media reference "^name^" defined twice : ignoring second definition.")
  | false ->
      let mda = media_reference_create arc.archi_medias (name, (mdalib,mdaname), broadcast, pos) in
      arc.archi_medias <- arc.archi_medias@[mda]

(** Adds a new operator reference to archilib/archiname.
  Raises Failure if operator name already exists in this architecture *)
let operator_reference_add archilib archiname (name,(oprlib,oprname),pos) =
  let arc = Application.architecture archilib archiname in
  match List.exists (function o -> o.oprref_name = name) arc.archi_operators with
  | true -> failwith ("Operator reference "^name^" defined twice : ignoring second definition.")
  | false ->
      let opr = operator_reference_create arc.archi_operators (name, (oprlib,oprname), pos) in
      arc.archi_operators <- arc.archi_operators@[opr]

(** Adds a new connection to archilib/archiname.
  Raises Failure if the predicate connectable is false for this connection *)
let connection_add archilib archiname ((oprname,gatename),mdaname) =
  let arc = Application.architecture archilib archiname in
  match connectable archilib archiname (oprname,gatename) mdaname with
  | false,msg -> failwith ("Can't create connection from "^oprname^"."^gatename^" to "^mdaname^" :\n"^msg)
  | _ ->
      let cnc = connection_create archilib archiname ((oprname,gatename),mdaname) arc.archi_operators arc.archi_medias in
      arc.archi_connections <- arc.archi_connections@[cnc]

(** Returns a new architecture with given fields values. Creates included operator and media references and connections.
  Raises Failure if some elements can't be created (however creating the others)*)
let architecture_create lib name operatorrefs mainoperator mediarefs connections dim desc =
  let errors = ref "" in
  match List.exists (function arc -> arc.archi_name = name && arc.archi_library = lib) Application.application.app_architectures with
  | true -> failwith ("Architecture "^name^" defined twice : ignoring second definition.")
  | false ->
  (* The architecture needs to be created before adding connections (connection_create accesses it via connectable) *)
      let arc = {
	archi_library = lib;
	archi_name = name;
	archi_operators = [];
	archi_operator_main = None;
	archi_medias = [];
	archi_connections = [];
	archi_dimension_window = dim;
	archi_description = desc;
      } in
      Application.application.app_architectures <- Application.application.app_architectures@[arc];
     (* Adding operator references *)
      List.iter (function o -> try
	operator_reference_add lib name o 
      with (Failure msg) -> errors:= !errors^"\n"^msg) operatorrefs;
      
      let mainopr = match mainoperator with
      | "" -> None
      | _ -> Some (List.find (function {oprref_name = n} -> n = mainoperator) arc.archi_operators) in
      arc.archi_operator_main <- mainopr;
      
  (* Adding media references *)
      List.iter (function m -> try
	media_reference_add lib name m
      with (Failure msg) -> errors:= !errors^"\n"^msg) mediarefs;
  (* Adding conections *)
      List.iter (function c -> try
	connection_add lib name c 
      with (Failure msg) -> errors:= !errors^"\n"^msg) connections;
      match !errors with
      | "" -> ()
      | some -> failwith some
	
(** Changes medium reference name from oldname to newname in archi arclib/arcname.*)
let media_reference_modify arclib arcname oldname newname =
  let arc = Application.architecture arclib arcname in
  let mda = List.find (function {mdaref_name=n} -> n=oldname) arc.archi_medias in
  mda.mdaref_name <- newname

(** Changes operator reference name from oldname to newname in archi arclib/arcname.*)
let operator_reference_modify arclib arcname oldname newname =
  let arc = Application.architecture arclib arcname in
  let ref = List.find (function {oprref_name=n} -> n=oldname) arc.archi_operators in
  ref.oprref_name <- newname

(** Moves operator named oprname in archi archilib/archiname taking (dx,dy) as offset *)
and operator_move archilib archiname oprname (dx,dy) =
  let opr = operator_reference archilib archiname oprname in
  let x,y = Coord.pos_of_coord2d opr.oprref_position in
  opr.oprref_position <- Coord.Coord2d (x+dx,y+dy)

(** Moves medium named oprname in archi archilib/archiname taking (dx,dy) as offset *)
and media_move archilib archiname mdaname (dx,dy) =
  let mda = media_reference archilib archiname mdaname in
  let x,y = Coord.pos_of_coord2d mda.mdaref_position in
  mda.mdaref_position <- Coord.Coord2d (x+dx,y+dy)

(** Returns the list of architectures (library,name) in current application. *)
let architectures_list () =
  List.map (function {archi_library = l;archi_name = n} -> (l,n)) Application.application.app_architectures

(** Returns the list of operator definitions (library,name) in current application. *)
and operatortypes_list () =
  List.map (function {oprdef_library = l;oprdef_name = n} -> (l,n)) Application.application.app_operator_definitions

(** Returns the list of operator references (name) in architecture archilib/archiname. *)
and operators_list archilib archiname =
  let arc = Application.architecture archilib archiname in
  List.map (function o -> o.oprref_name) arc.archi_operators

(** Returns the list of medium references (name) in architecture archilib/archiname. *)
and media_list archilib archiname =
  let arc = Application.architecture archilib archiname in
  List.map (function m -> m.mdaref_name) arc.archi_medias

(** Returns the list of medium definitions (library,name) in current application. *)
and mediatypes_list () =
  List.map (function {mdadef_library = l;mdadef_name = n} -> (l,n)) Application.application.app_media_definitions

(** Removes connection between oprname.gatename and mdaname in archi archilib/archiname. *)
and connection_delete archilib archiname oprname gatename mdaname =
  let arc = Application.architecture archilib archiname in
  arc.archi_connections <- List.filter (function {cnc_operator = opr; cnc_gate = gate; cnc_media = mda} -> opr.oprref_name <> oprname || mda.mdaref_name <> mdaname || gate.gte_name <> gatename) arc.archi_connections

(** Removes operator reference oprname in archi archilib/archiname. *)
and operator_reference_delete archilib archiname oprname =
  let arc = Application.architecture archilib archiname in
  (match operator_main_get archilib archiname with
  | n when n=oprname -> operator_main_clear archilib archiname;
  | _ -> ());
  arc.archi_connections <- List.filter (function {cnc_operator = opr} -> opr.oprref_name <> oprname) arc.archi_connections;
  arc.archi_operators <- List.filter (function opr -> opr.oprref_name <> oprname) arc.archi_operators

(** Removes medium reference mdaname in archi archilib/archiname. *)
and media_reference_delete archilib archiname mdaname =
  let arc = Application.architecture archilib archiname in
  arc.archi_connections <- List.filter (function {cnc_media = mda} -> mda.mdaref_name <> mdaname) arc.archi_connections;
  arc.archi_medias <- List.filter (function mda -> mda.mdaref_name <> mdaname) arc.archi_medias

(** Sets broadcast boolean field of medium reference mdaname in archi archilib/archiname to broadcast. *)
and media_broadcast_change archilib archiname mdaname broadcast =
  let arc = Application.architecture archilib archiname in
  let mda = List.find (function mda -> mda.mdaref_name = mdaname) arc.archi_medias in
  mda.mdaref_broadcast <- broadcast

(** Sets medium definition bus type to bustype. *)
and media_bustype_change mdalib mdaname bustype =
  let mdadef = Application.mediadef mdalib mdaname in
  mdadef.mdadef_bus_type <- bustype

(* Returns the name of operator o. *)
let name_of_operator o = match o with
| Operator opr -> opr.oprref_name
| Media mda -> mda.mdaref_name

let (=@@) opr1 opr2 = 
  ((name_of_operator (Operator opr1)) = (name_of_operator (Operator opr2)))

let (<>@@) opr1 opr2 = 
  ((name_of_operator (Operator opr1)) <> (name_of_operator (Operator opr2)))

let (=@@@) mda1 mda2 = 
  ((name_of_operator (Media mda1)) = (name_of_operator (Media mda2)))

let (<>@@@) mda1 mda2 = 
  ((name_of_operator (Media mda1)) <> (name_of_operator (Media mda2)))



(** Returns (bustype,broadcast) values of medium media. *)
let bustype mda =
  let archilib,archiname = Application.archi_main_get () in
  let _,mdadeflib,mdadefname,broadcast,_ = media_reference_content mda in
  let _,_,bustype,_,_ = media_definitionname_content mdadeflib mdadefname in
  bustype,broadcast

(** Sets arclib/arcname as main architecture. *)
let archi_main_set (arclib,arcname) =
  Application.archi_main_set (Application.architecture arclib arcname)

(** Returns true if architecture lib/name is the main architecture. *)
let archi_is_main lib name =
  match Application.archi_main_get () with
  | l,a when l=lib && a=name -> true
  | _ -> false

(** Removes operator definition oprlib/oprname. *)
let operator_definition_delete oprlib oprname =
  let oprdef = Application.operatordef oprlib oprname in
  let oprref_delete arc =
    List.iter (function oprref -> match oprref.oprref_definition=oprdef with
    | true -> operator_reference_delete arc.archi_library arc.archi_name oprref.oprref_name
    | false -> ()) arc.archi_operators in
  List.iter oprref_delete Application.application.app_architectures;
  Application.application.app_operator_definitions <- List.filter ((<>) oprdef) Application.application.app_operator_definitions

(** Removes medium definition mdalib/mdaname. *)
let media_definition_delete mdalib mdaname =
  let mdadef = Application.mediadef mdalib mdaname in
  let mdaref_delete arc =
    List.iter (function mdaref -> match mdaref.mdaref_definition=mdadef with
    | true -> media_reference_delete arc.archi_library arc.archi_name mdaref.mdaref_name
    | false -> ()) arc.archi_medias in
  List.iter mdaref_delete Application.application.app_architectures;
  Application.application.app_media_definitions <- List.filter ((<>) mdadef) Application.application.app_media_definitions

(** Removes architecture archilib/archiname. *)
let architecture_delete archilib archiname =
  (match Application.archi_main_get () with
  | l,n when l=archilib && n=archiname -> Application.archi_main_clear ()
  | _ -> ());
  Application.application.app_architectures <- List.filter (function a -> a.archi_library<>archilib || a.archi_name<>archiname) Application.application.app_architectures

(** Resizes architecture archilib/archiname window to size a b. *)
let archi_dimension_window_change archilib archiname a b =
  let arc = Application.architecture archilib archiname in
  arc.archi_dimension_window <- Coord.Coord2d(a,b)

(** Returns the list of media connected to operator in architecture arc with the gate they are connected to *)
let links_operator arc operator =
  let neighbourcnc = List.filter (function {cnc_operator=opr} -> opr=@@operator) arc.archi_connections in
  List.map (function {cnc_gate=gte;cnc_media=mda} -> ((Operator operator),(Some gte)),((Media mda),None)) neighbourcnc

(** Returns the list of operators connected to media in architecture arc with the gate they are connected to *)
let links_media arc operator =
  let neighbourcnc = List.filter (function {cnc_media=mda} -> mda=@@@operator) arc.archi_connections in
  List.map (function {cnc_operator=opr;cnc_gate=gte} -> ((Media operator),None),((Operator opr),(Some gte))) neighbourcnc

(** See links_media and links_operator. Warning, this function must only be used after routing calculation (in adequation), which computes neighbours. Otherwise, use links_media or links_operator. *)
let links archilib archiname operator =
  let arc = Application.architecture archilib archiname in
  match operator with
  | Operator operator -> operator.oprref_neighbours
  | Media media -> media.mdaref_neighbours

(** Returns true if operator reference oprref is able to execute operation definition opnlib/opnname, ie a duration is defined for this operation on that operator. *)
let able_to_execute opnlib opnname oprref =
  Hashtbl.mem oprref.oprref_definition.oprdef_operation_durations (opnlib,opnname)

(** Returns the list of operators able to execute operation, ie for which this operation has a duration defined. *)
let operators_able_to_execute archilib archiname opnlib opnname =
  let arc = Application.architecture archilib archiname in
  List.filter (able_to_execute opnlib opnname) arc.archi_operators

(** Returns the duration of opnlib/opnname when executed on operator definition operator. *)
let operator_duration operator opnlib opnname =
  let durations = operator.oprdef_operation_durations in
    try
      Hashtbl.find durations (opnlib,opnname)
    with Not_found -> raise (Failure ("No duration specified for operation "^opnlib^"/"^opnname^" on operator "^operator.oprdef_name))

(** Returns the duration for the transfer of unitary data of type typename on medium media. *)
let media_duration media typename =
  let durations = media.mdadef_operation_durations in
    try
      fst (Hashtbl.find durations typename)
    with Not_found -> raise (Failure ("No duration specified for type "^typename^" on medium "^media.mdadef_name))

(** Returns the duration of opnlib/opnname when executed on operator reference operator. *)
let operator_reference_duration operator opnlib opnname = operator_duration operator.oprref_definition opnlib opnname

(** Returns the duration for the transfer of unitary data of type typename. *)
let media_reference_duration media typename = media_duration media.mdaref_definition typename

(** Returns average duration of opnlib/opnname on all operator definitions. *)
let operator_duration_average opnlib opnname = 
  let oprs = Application.application.app_operator_definitions in
  let sum,nb = List.fold_left (fun (sum,nb) opr -> try
				 let duration = Hashtbl.find opr.oprdef_operation_durations (opnlib,opnname) in
				   (sum+.duration),(nb+.1.)
			       with Not_found -> sum,nb) (0.,0.) oprs in
    sum/.nb


