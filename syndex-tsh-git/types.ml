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

(** This module contains the core data structures for SynDEx and some basis function *)

(* Some constants *)
let syndex_msg = "SynDEx"
let syndex_version_msg = syndex_msg^"-"^Version.version
let inria_msg = "Inria Rocquencourt"
let web_msg = "www-rocq.inria.fr/syndex"
let team_msg = "Ostre team"
let author_msg = "Julien Forget, Christophe Macabiau"
let application_msg = "\n"^syndex_version_msg^"\n"^web_msg^"\n"^author_msg^","^inria_msg^","^team_msg^"\n"

let syndex_web = "http://www-rocq.inria.fr/syndex"
let team_web = "http://www-rocq.inria.fr/~sorel/work/ostre/welcomeGB.html"
let inria_web = "http://www.inria.fr/"
let inria_rocquencourt_web = "http://www-rocq.inria.fr/"
let code_generated = ref false

(** Class of algorithm definition *)
(* Internal for operations created at the transformation (Explode, Implode, etc.) *)
type algorithm_class = Constant | Sensor | Actuator | Memory of (Symbolic.expression*Symbolic.expression) | Operation | Internal
(** Dependence class*)
type dependence_class = Strong_Precedence_Data | Weak_Precedence_Data | Precedence | Data
(** Bus transmission type *)
type bus_type = SamPP | SamMP | Ram
(** Return type of sdx files parser *)
type parse_ret = Done | Include of string
(** Type of relative constraint *)
type relative_constraint_type = Union | Disjunction
(** Transformation type before the adequation *)
type adequation_type = Flatten | No_Repetition_Flatten | No_Flatten
(** Implicitely calculated repetition type *)
type calculated_repetition_type = Repeat of int | Error | Undefinied
(** Overall repetition type. Calculated type is more priotary than specified type. *)
type repetition_type = Calculated of calculated_repetition_type | Specified of int
(** Used to specify attachements on Internal operations (Explode, CondI, etc.) *)
type xsc_attachement_type = AttachAll | AttachRef | AttachCondI | AttachCondO | AttachExplode | AttachImplode
(** Phases of the code generation *)
type code_generation_phase = InitSeq | LoopSeq | EndSeq

(** Type of conditions in an algorithm definition. *)
and condition_type = 
  | Condition of string * int (* variable * value *)
  | Boolean of bool (* no condition : true *)

(** Type for the source or destination of a dependence. *)
and port_ref_type =
  | Port of Port.port (* Port of the current algorithm definition *)
  | Ref of reference_type * Port.port (* A reference and one of its ports *)

(** Type of a dependence. *)
and dependence_type = {
    dpd_source : port_ref_type;
    dpd_destination : port_ref_type;
    dpd_class : dependence_class;
    mutable dpd_condition : condition_type;
  } 

(** Type of a reference on an algorithm definition *)
and reference_type = {
    mutable ref_name : string;
    mutable ref_algorithm : algorithm_type; (* Algorithm definition referenced *)
    mutable ref_arguments_values : Symbolic.expression list;
    mutable ref_condition : condition_type;
    mutable ref_position : Coord.coord2d;
    mutable ref_repetition : repetition_type;
    mutable ref_description : string
  } 

(** Type of an algorithm definition *)
and algorithm_type = {
    algo_library : string;
    mutable algo_name : string;
    algo_class : algorithm_class;
    mutable algo_arguments_names : string list;
    mutable algo_ports : Port.port list;
    mutable algo_references : reference_type list;
    mutable algo_dependences : dependence_type list;
    mutable algo_dimension_window : Coord.coord2d;
    mutable algo_description : string;
    mutable algo_code_phases : code_generation_phase list (* phases in which code must be generated *)
  }

(** Type of an operator gate *)
type gate_type = {
    gte_name : string;
    gte_media_type : string;
  }

(** Simple operation definition identifier. Only used in adequationtypes. *)
and operation_type = {
    opn_library : string;
    opn_name : string;
  }

(** Type of an operator definition *)
and operator_definition_type = {
    oprdef_library : string;
    mutable oprdef_name : string;
    mutable oprdef_gates : gate_type list;
    mutable oprdef_operation_durations : ((string*string),float) Hashtbl.t;
    mutable oprdef_description : string;
    mutable oprdef_code_phases : code_generation_phase list (* to be able to add specific code in init or end phase *)
  }

(** Type of a medium definition *)
and media_definition_type = {
    mdadef_library : string;
    mutable mdadef_name : string;
    mutable mdadef_bus_type : bus_type;
    mutable mdadef_operation_durations : (string,(float*string)) Hashtbl.t; (* The second string is the library name.
									       This used only to know in which file the duration was defined.
									       Yet we don't want it to be part of the key for the hash function as the type is not really linked to this library. *)
    mutable mdadef_description : string;
  }

(** Type of a reference on an operator definition *)
and operator_reference_type = {
    mutable oprref_name : string;
    oprref_definition : operator_definition_type; (* Operator definition referenced *)
    mutable oprref_position : Coord.coord2d;
    mutable oprref_id : int; (* Id used in routes calculation *)
    mutable oprref_links2 : int array; (* Distance to other operators. Distance to opr of id i is oprref_links2[i-1] (in number of operators and media) *)
    mutable oprref_neighbours : link_type list (* Links to neighbour media *)
  } 

(** Type of a reference on a medium definition *)
and media_reference_type = {
    mutable mdaref_name : string;
    mdaref_definition : media_definition_type;
    mutable mdaref_broadcast : bool;
    mutable mdaref_position : Coord.coord2d;
    mutable mdaref_links2 : int array; (* Distance to operators. Distance to opr of id i is oprref_links2[i-1] (in number of operators and media) *)
    mutable mdaref_neighbours : link_type list (* Links to neighbour operators *)
  } 

(** Type of a connection between an operator reference and a medium reference *)
and connection_type = {
    cnc_operator : operator_reference_type;
    cnc_gate : gate_type;
    cnc_media : media_reference_type;
   } 

(** Class of operator ie a type which unifies calculation operators (operators) and communication operators (media) *)
and operator_class = Operator of operator_reference_type | Media of media_reference_type

(** Type used in the routing *)
and link_type = ((operator_class*(gate_type option))*(operator_class*(gate_type option)))

(** Architecture type *)
and architecture_type = {
    archi_library : string;
    mutable archi_name : string;
    mutable archi_operators : operator_reference_type list;
    mutable archi_operator_main : operator_reference_type option;
    mutable archi_medias : media_reference_type list;
    mutable archi_connections : connection_type list;
    mutable archi_dimension_window : Coord.coord2d;
    mutable archi_description : string;
  }

(** Software component type *)
type xsc_type = {
    mutable xsc_name : string;
    mutable xsc_references : ((string list)*(xsc_attachement_type)) list; (* a list of reference paths attached to this sc. xsc_attachement was added later to be able to specify sc for CondI, Explode, etc. *)
  } 

(** SynDEx application type (algo+archi+constraints) *)
and application_type = {
    mutable app_algorithms : algorithm_type list;
    mutable app_algorithm_main : algorithm_type option;
    mutable app_algorithm_main_arguments_values : Symbolic.expression list;
    mutable app_operator_definitions : operator_definition_type list;
    mutable app_media_definitions : media_definition_type list;
    mutable app_architectures : architecture_type list;
    mutable app_architecture_main : architecture_type option;
    mutable app_software_components : xsc_type list;
    mutable app_reverse_sc : (((string list)*xsc_attachement_type),xsc_type) Hashtbl.t; (* This field was added later for optimisation. It should replace app_software_components but we didn't for easier upward compatibility. It directly associates an operation path to it's software component. xsc_attachement was added later to be able to specify sc for CondI, Explode, etc. *)
    mutable app_constraints : ((string list) * (((string * string) * string) list)) list; (* ((ref_path) * (((arclib*arcname)*oprname) list) list) *)
    mutable app_xsc_absolute_constraints : (string * (((string * string) * string) list)) list; (* (xscname*(((arclib*arcname)*oprname) list) list) *)
    mutable app_xsc_relative_constraints : (relative_constraint_type * (string list)) list; (* string list is xscname list *)
    mutable app_libraries : string list; (* Be sure to keep this list in order of inclusion, there may be dependencies between libraries *)
    mutable app_description : string;
  }

(* ******************************************************************************* *)
(** Basic print one line string function *)
let ps s = print_string (s^"\n");flush stdout

(** Used to display debug messages.
  Uses a debug level which should be defined in each module and a message level given for each message.
  To enable different levels of debug, just change the module debug level.*)
let debug_ps msg_level debug_level s =
  match msg_level <= debug_level with
  | true -> ps s
  | false -> ()

(* ******************************************************************************* *)

(** Returns the substring of s starting at 0 and ending at (length s)-n without failure. *)
let cut s n = match (String.length s<=n) with | true -> "" | _ -> String.sub s 0 ((String.length s)-n)

(** Returns the intersection of lists l1 and l2 *)
let intersection l1 l2 = List.fold_left (function a -> function b -> if List.mem b l2 then a@[b] else a) [] l1
(** Returns the difference between l1 and l2 *)
let exclusion l1 l2 = List.fold_left (function a -> function b -> if List.mem b l2 then a else a@[b]) [] l1
(** Returns the list which is the intersection of all lists in l *)
let intersections l = match l with
| [] -> []
| _ -> List.fold_left (function a -> function b -> intersection a b) (List.hd l) (List.tl l)
(** Returns l without any duplicate *)
let remove_copies l = List.fold_left (function a -> function b -> if List.mem b a then a else a@[b]) [] l
(** Returns the union without any duplicate of all lists in l *)
let union l = remove_copies (List.concat l)

(** Generic function used for list elements comparison *)
let list_criterion cost criterion l default = match l with
| hd::tl -> List.fold_left (function sublist,c -> function e ->
    let fe = cost e in
    match criterion fe c with
    | x when x<0 -> [e],fe
    | x when x>0 -> sublist,c
    | _ -> e::sublist,c) ([hd],(cost hd)) tl
| _ -> [],default

(** Returns the list of minimum elements (according to cost function f) in l and their cost.
  When used on an empty list, returns [],default *) 
let list_min_elements_value f l default =
  list_criterion f (fun a b -> compare a b) l default

(** Returns the list of maximum elements (according to cost function f) in l and their cost.
  When used on an empty list, returns [],default *) 
let list_max_elements_value f l default =
  list_criterion f (fun a b -> - (compare a b)) l default

(** Returns the list of minimum elements (according to cost function f) in l.
  When used on an empty list, returns [] *)
let list_min_elements f l default =
  fst (list_min_elements_value f l default)

(** Returns the list of maximum elements (according to cost function f) in l.
  When used on an empty list, returns [] *)
let list_max_elements f l default =
  fst (list_max_elements_value f l default)

(** Returns the minimum of cost function f in l.
  When used on an empty list, returns default *)
let list_min_value f l default =
  snd (list_min_elements_value f l default)

(** Returns the maximum of cost function f in l.
  When used on an empty list, returns default *)
let list_max_value f l default =
  snd (list_max_elements_value f l default)

(** Returns the list of values from hashtbl ht *)
let list_of_hashtbl ht =
  Hashtbl.fold (fun _ value list -> value :: list) ht []

(** Returns the list of values for which f is true in hashtbl ht. *)
let hashtbl_filter f ht =
  let filtered = ref [] in
  Hashtbl.iter (function _ -> function e -> match f e with
  | true -> filtered := e :: !filtered
  | false -> ()) ht;
  !filtered

(** Returns the length (number of values) of hashtbl ht *)
let hashtbl_length ht =
  Hashtbl.fold (fun _ _ length -> 1 + length) ht 0

(** Flattens a string list l into a string where each string of the string list is separated by sep *)
let string_of_string_list l sep =
  match l with
  | [] -> ""
  | _ ->
      let s = List.fold_left (function s -> function e -> s^e^sep) "" l in
      cut s 1

(** Returns analyzed string plus an error message if there is a syntax error *)
let analyze strg rgxpstring =
  let rgxp = Str.regexp rgxpstring in
  let rec f i nb = try
    f ((Str.search_forward (Str.regexp "\\\\(") rgxpstring i)+1) (nb+1)
  with _ -> nb in
    (* Number of groups in rgxpstring (groups begin with \( ) *)
  let n = f 0 0 in
    (* Returns the expression containing n groups, some may be empty for optional groups, that was last matched.*)
  let rec expr i =
    let wd = try Str.matched_group i strg with Not_found -> "" in
    match i=n with
    | true -> [wd]
    | false -> wd::(expr (i+1)) in
    (* Returns the first expression matching rgxpstring, search starts after jth character of strg *)
  let rec ana j =
    try
      ignore(Str.search_forward rgxp strg j);
      let expr = expr 1 in
      match (Str.match_end ())=(String.length strg) with
      |	true -> [expr]
      |	false -> expr::(ana (Str.match_end ()));
    with Not_found -> [] in
  let result = ana 0 in
  let result_string = string_of_string_list (List.concat result) "" in
  let result_size = (String.length result_string) + 1 in (* I don't know why but one character is lost *)
(*     ps ("Result : "^result_string^" result size : "^(string_of_int result_size));  *)
    let input = (Str.global_replace (Str.regexp "\\( \\|\n\\|=\\|<\\|>\\|[\\|]\\|,\\|;\\|.\\)") "" strg) in
    let input_size = String.length input in
(*     ps ("Input : "^input^"input size : "^(string_of_int input_size)); *)
  let err_msg = match (input_size > result_size) with
    | true -> "Syntax error; some elements may have been ignored"
    | false -> "" in
    result,err_msg

(** Returns the string corresponding to generation code phase code_phase *)
let string_of_code_phase code_phase =
  match code_phase with
  | InitSeq -> "init"
  | LoopSeq -> "loop"
  | EndSeq -> "end"

(** Returns the string corresponding to an algorithm definition *)
let string_of_ref_type lib def = match lib with
| "" -> def
| _ -> lib^"/"^def

(** Returns the string corresponding to arg names an, seperated by sep *)
let string_of_argsnames an sep =
  match an with
  | [] -> ""
  | _ -> "<"^(string_of_string_list an sep)^">"

(** Returns the string corresponding to arg values an, seperated by sep *)
let string_of_argsvalues av =
  match av with
  | [] -> ""
  | _ -> "<"^(Symbolic.string_of_expression_list av)^">"

(** Returns the string corresponding to dimension d *)
let string_of_dimension d =
  "["^(Symbolic.string_of_expression d)^"]"

(** Returns the string corresponding to direction dir *)
let string_of_direction dir =
  match dir with
| Port.In -> "In"
| Port.Out -> "Out"

(** Returns the string corresponding to condition c *)
let string_of_condition c =
  match c with
  | Boolean b -> string_of_bool b
  | Condition (prt,value) -> prt^"="^(string_of_int value)

(** Returns the string corresponding to dependence dpd *)
let string_of_dpd dpd =
  let conv p = match p with
    | Port p -> Port.port_name p
    | Ref (r,p) -> r.ref_name^"."^ (Port.port_name p) in
    (conv dpd.dpd_source)^" to "^(conv dpd.dpd_destination)

(** Returns the string corresponding to condition list l *)
let string_of_condlist l =
  List.fold_left (fun rest cond -> rest^" & "^(string_of_condition cond)) "" l

let string_of_attachtype attachtype =
  match attachtype with
  | AttachAll -> "All"
  | AttachRef -> "Reference"
  | AttachCondI -> "CondI"
  | AttachCondO -> "CondO"
  | AttachExplode -> "Explode"
  | AttachImplode -> "Implode"

(** Writes string s to file named filename *)
let file_write filename s =
  let f = open_out filename in
  output_string f s;
  close_out f

(** Starts process prog with arguments args *)
let exec prog args =
  match Sys.file_exists prog with
  | true -> Unix.create_process prog (Array.of_list (prog::args)) (Unix.stdin) (Unix.stdout) (Unix.stderr)
  | false -> raise (Failure ("Application not found : "^prog))




