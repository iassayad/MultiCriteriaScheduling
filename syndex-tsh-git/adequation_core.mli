val debug_level : int
val lefe_table : (string, float) Hashtbl.t
val lefe_table_median : (string, float) Hashtbl.t
val datas : Adequationtypes.available_data_type
val schedules : Adequationtypes.full_schedule_type
val archilib : string ref
val archiname : string ref
val adequation_order : int ref

val pi : Adequationtypes.operation_type -> Types.operator_class

val operators_constraint :
  Adequationtypes.operation_type -> Types.operator_class list
val operators_able_to_execute :
  Adequationtypes.operation_type -> Types.operator_class list
val create_gaps_backups : unit -> unit
val restore_gaps : unit -> unit
val update_datas :
  Types.operator_class ->
  Adequationtypes.data_type ->
  Adequationtypes.operation_type ->
  Adequationtypes.port_type -> float -> unit
val route :
  Types.operator_reference_type list ->
  Types.media_reference_type list -> unit
val architecture_init : 'a -> unit
val schedule :
  Adequationtypes.operation_type ->
  Types.operator_class -> bool -> float -> float -> unit
val deschedule :
  Adequationtypes.operation_type -> Types.operator_class -> bool -> unit

val delete_coms :
  Adequationtypes.operation_type -> Adequationtypes.graph_type -> unit

val esfs_cond :
  Adequationtypes.operation_type ->
  Types.operator_class ->
  float -> Adequationtypes.condition_type list -> float

val rel_esfs_cond :
  Adequationtypes.operation_type ->
  Types.operator_class ->
  float -> Adequationtypes.condition_type list -> float

val fault_esfs_cond :
  Adequationtypes.operation_type ->
  Types.operator_class ->
  float -> float -> Adequationtypes.condition_type list -> float*float


val shortest_links :
  Types.operator_class -> Types.operator_class -> Types.link_type list

val place_link :
  (Types.operator_class * 'a) list ->
  bool ->
  Adequationtypes.operation_type ->
  Adequationtypes.port_type ->
  float ->
  Adequationtypes.data_type ->
  Adequationtypes.graph_type ->
  Adequationtypes.condition_type list ->
  bool ->
  Adequationtypes.dependence_class -> bool -> 
  (Adequationtypes.operation_type * Adequationtypes.port_type * float) *
  Types.operator_class


val reliable_place_link :
  (Types.operator_class * 'a) list ->
  bool ->
  Adequationtypes.operation_type ->
  Adequationtypes.port_type ->
  float ->
  Adequationtypes.data_type ->
  Adequationtypes.graph_type ->
  Adequationtypes.condition_type list ->
  bool ->
  Adequationtypes.dependence_class ->  
  (Adequationtypes.operation_type * Adequationtypes.port_type * float) *
  Types.operator_class

val update_senders :
Types.operator_reference_type ->
Adequationtypes.operation_type ->
Adequationtypes.graph_type -> unit

val find_best_link :
  Types.link_type list ->
  (Types.operator_class * Types.gate_type option) list ->
  float ->
  Adequationtypes.operation_type ->
  Adequationtypes.port_type ->
  float ->
  Adequationtypes.data_type ->
  Adequationtypes.graph_type ->
  Adequationtypes.condition_type list ->
  bool ->
  Adequationtypes.dependence_class ->
  Types.operator_class -> bool ->
  (Types.operator_class * Types.gate_type option) list
val dpd_EEFS : Adequationtypes.dependence_type -> float
val make_cond_dpds :
  (float -> Adequationtypes.dependence_type -> 'a) ->
  Adequationtypes.dependence_type list -> float
val lefe : Adequationtypes.operation_type -> float
val lefe_median : (Adequationtypes.operation_type, float) Hashtbl.t -> Adequationtypes.operation_type -> float
val is_schedulable : Adequationtypes.operation_type -> bool
val update_schedulables :
  Adequationtypes.operation_type ->
  Adequationtypes.operation_type list ref -> unit
val print_adequation_result : unit -> unit

val pretty_conv_graph :
  Adequationtypes.graph_type ->
  (string *
   ((string * string * float * float * float * (string * int) list * int) list *
    (string * string * float * float * float * (string * int) list * int) list))
  list * (string * string list) list * (string * string list) list

(*val initial_gaps : 
  (Adequationtypes.condition_type list,Adequationtypes.schedule_type) Hashtbl.t 
  -> Adequationtypes.condition_type list -> (float * float ) list
*)
