type data_type = operation_type * port_type
and communication_class =
    Write of data_type * Types.operator_reference_type
  | Read of data_type * Types.operator_reference_type
  | Transfer of data_type * Types.operator_reference_type *
      Types.operator_reference_type list
  | Send of data_type * Types.operator_reference_type *
      Types.operator_reference_type list
  | Receive of data_type * Types.operator_reference_type *
      Types.operator_reference_type list * Types.operator_reference_type
  (********************************************************)
  (****** RELIABLE COMMUNICATION PRIMITIVEs FOR SAMMP *****)
  | Reliable_Send of data_type * Types.operator_reference_type list *
      Types.operator_reference_type list
  | Reliable_Receive of data_type * Types.operator_reference_type list *
      Types.operator_reference_type list * Types.operator_reference_type
  | Reliable_Sync of data_type * Types.operator_reference_type list *
      Types.operator_reference_type list * Types.operator_reference_type
  (********************************************************)
  | Send_Synchro of Types.operator_reference_type *
      Types.operator_reference_type
  | Receive_Synchro of Types.operator_reference_type *
      Types.operator_reference_type
  | Sync of data_type * Types.operator_reference_type *
      Types.operator_reference_type list * Types.operator_reference_type
and operation_class =
    Calcul of Types.algorithm_class * string * string
  | Communication of communication_class
and origin_type =
    Ihm of string list
  | Condition_In of string list
  | Condition_Out of string list
  | Explode of string list
  | Implode of string list
  | Synchro_Constant of string list
  | Diffuse of (string list * int)
  | Iterate of (string list * int)
  | Fork of (string list * int)
  | Join of (string list * int)
and edge_type = Condition of int | Data
and dependence_class = edge_type * Types.dependence_class
and condition_type = data_type option * int option
and port_type = {
  mutable t_prt_name : string;
  t_prt_dir : Port.direction;
  t_prt_typename : string;
  t_prt_class : Port.port_class;
  mutable t_prt_dim : int;
  t_prt_order : int;
}
and dependence_type = {
  mutable t_dpd_sopn : operation_type;
  mutable t_dpd_sprt : port_type;
  t_dpd_dopn : operation_type;
  t_dpd_dprt : port_type;
  mutable t_dpd_backups : (dependence_type list) option;
  mutable t_dpd_routing : (operation_type list) option;
  mutable t_dpd_class : dependence_class;
  mutable t_dpd_condition : condition_type list;
  t_dpd_status : bool;
}
and operation_type = {
  mutable t_opn_path : string list;
  mutable t_opn_class : operation_class;
  t_opn_arguments_values : Symbolic.expression list;
  mutable t_opn_ports : port_type list;
  mutable t_opn_dependences_predecessors : dependence_type list;
  mutable t_opn_dependences_successors : dependence_type list;
  mutable t_opn_successors : operation_type list;
  mutable t_opn_predecessors : operation_type list;
  mutable t_opn_condition : condition_type list;
  mutable t_opn_operator : Types.operator_class option;
  mutable t_opn_rank : int;
  mutable t_opn_esfs : float;
  mutable t_opn_weefs : float;
  mutable t_opn_pred_scheduled : operation_type option;
  t_opn_status : bool;
  t_opn_origin : origin_type;
  mutable t_opn_xsc_name : string;
  t_opn_referencing_alglib : string;
  t_opn_referencing_algname : string;
  mutable t_opn_adequation_order : int;
  t_opn_code_phases : Types.code_generation_phase list;
}
and graph_type = (string, operation_type) Hashtbl.t
and schedule_type = {
  mutable t_sch_operations : (operation_type * float * float) list;
  mutable t_sch_last_opn : operation_type option;
  mutable t_sch_gaps : (float * float) list;
  mutable t_sch_gaps_backup : (float * float) list;
}
and operator_schedule_type = (condition_type list, schedule_type) Hashtbl.t
and full_schedule_type = (string, Types.operator_class * operator_schedule_type) Hashtbl.t
and available_data_type =
  (data_type,
   (Types.operator_class * operation_type * port_type * float) list)
  Hashtbl.t
val new_port :
  string ->
  Port.direction ->
  string -> int -> Port.port_class -> int -> port_type
(* val port_add :
  string ->
  Types.direction_type ->
  string -> int -> Types.port_class -> int -> operation_type -> unit *)
val dependence_add :
  operation_type ->
  port_type ->
  operation_type ->
  port_type -> dependence_class -> condition_type list -> bool -> unit
val dependence_remove : dependence_type -> unit
val new_opn :
  string list ->
  operation_class ->
  Symbolic.expression list ->
  port_type list ->
  dependence_type list ->
  dependence_type list ->
  condition_type list ->
  Types.operator_class option ->
  int ->
  float ->
  bool ->
  origin_type ->
  operation_type option ->
  string ->
  string -> string -> Types.code_generation_phase list -> operation_type
val name_of_operation : operation_type -> string
val name_of_operator : Types.operator_class -> string
val identifier_of_path : string list -> string
val identifier_of_operation : operation_type -> string
val identifier_of_path_name : string list -> string -> string
val parentpath_of_path : 'a list -> 'a list
val parentname_of_operation : operation_type -> string
val parentpath_of_operation : operation_type -> string list
val name_of_identifier : string -> string
(* val path_of_identifier : string -> string *)
val operation_add : graph_type -> operation_type -> unit
val operation_remove : graph_type -> operation_type -> unit
val operation_get_of_path : graph_type -> string list -> operation_type
val operation_cond_set : graph_type -> string list -> condition_type list -> unit
val new_adequation_graph : unit -> graph_type
val remove_false_status : graph_type -> unit
val data_get : graph_type -> string list -> string -> Port.direction -> data_type
val type_and_size_of_data : data_type -> string
val name_of_data : data_type -> string
val name_of_comclass : communication_class -> string
val unique_identifier : graph_type -> string list -> string -> string
val is_media : Types.operator_class -> bool
val type_of_operator : Types.operator_class -> string
val media_of_operator_class : Types.operator_class -> Types.media_reference_type
val operator_of_operator_class : Types.operator_class -> Types.operator_reference_type
val data_of_communication : operation_type -> data_type
val operations_of_operator : graph_type -> Types.operator_class -> operation_type list
val operations_of_operator_media : graph_type -> Types.operator_class -> Types.operator_class -> operation_type list
val path : operation_type -> string list
val predecessors : operation_type -> operation_type list
val successors : operation_type -> operation_type list
val is_calcul : operation_type -> bool
val is_communication : operation_type -> bool
val is_implantation : operation_type -> bool
val is_memory : operation_type -> bool
val is_constant : operation_type -> bool
(* val is_factorization_boundary : operation_type -> bool *)
val conditioned_operation : operation_type -> bool
(* val conditioned_dependence : dependence_type -> bool *)
val is_precedence_port : port_type -> bool
(* val is_precedence_com : operation_type -> bool *)
val is_dependence_with_data : dependence_type -> bool
val find_precedence_port : operation_type -> Port.direction -> port_type
val operator_of_operation : operation_type -> Types.operator_class
val execution_operator_of_communication : operation_type -> Types.operator_reference_type
val execution_operator_of_operation : operation_type -> Types.operator_reference_type
val deflibname : operation_type -> string * string
(* val operation_of_identifier : graph_type -> string -> operation_type *)
val delta : operation_class -> Types.operator_class -> float
val delta_average : operation_class -> float
val delta_median  : (operation_type,float) Hashtbl.t -> operation_type -> float


val eefs : operation_type -> float
val precedence_ports : unit -> port_type list
val simultaneous : float -> float -> float -> float -> bool
val exclusion_condition : condition_type list -> condition_type list -> bool
val string_of_condlist : condition_type list -> string
val string_of_dpd : dependence_type -> string
val size_of_expression : (string * Symbolic.expression) list -> string list -> Symbolic.expression -> int
(* val port_factor_of_boundary : operation_type -> int *)
val path_of_origin : operation_type -> string list
val get_xsc : string list -> string -> string list -> Types.xsc_attachement_type -> string
val input_operations : graph_type -> operation_type list

(* new operators to compare operators and operations *)
val (=@) : Types.operator_class -> Types.operator_class -> bool
val (<>@) : Types.operator_class -> Types.operator_class -> bool
val (=@@) : Types.operator_reference_type -> Types.operator_reference_type -> bool
val (<>@@) : Types.operator_reference_type -> Types.operator_reference_type -> bool

val (=$) : operation_type -> operation_type -> bool
val (<>$) : operation_type -> operation_type -> bool

val (=%)  : port_type  -> port_type -> bool
val (<>%) : port_type  -> port_type -> bool

(*val initialize_failure_rates_table : string ->  Types.operator_class list ->  Types.operator_class list -> unit*)

val failures_rate : (Types.operator_class, float) Hashtbl.t
val frequency : (Types.operator_class, (float * float * float * float)) Hashtbl.t
val replication_process_proc : (string, (int * int * int)) Hashtbl.t
val scheduled_opns :  (string * (string list)) list ref
val initialize_failure_rates_table : string ->  unit
val initialize_frequency_table : string ->  unit
val get_lambda : float -> float -> float  
val get_median : int list -> float 
val get_average : int list -> float
