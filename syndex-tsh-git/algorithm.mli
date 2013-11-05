val precedence_in_name : string
val precedence_out_name : string
val string_of_port_ref : string * string -> string

val portname_content :
  string ->
  string ->
  string ->
  Port.direction ->
  string * Port.direction * string * Symbolic.expression *
  Port.port_class * int * Coord.coord2d

val condition_create : string * int -> Types.condition_type
val referencename_content :
  string ->
  string ->
  string ->
  string * string * string * Symbolic.expression list * (string * int) *
  Coord.coord2d * Types.repetition_type * string
val dependence_content :
  Types.dependence_type ->
  (string * string) * (string * string) * Types.dependence_class *
  (string * int)
val algorithmname_content :
  string ->
  string ->
  string * string * Types.algorithm_class * string list *
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list *
  (string * string * string * Symbolic.expression list * (string * int) *
   Coord.coord2d * Types.repetition_type * string)
  list *
  ((string * string) * (string * string) * Types.dependence_class *
   (string * int))
  list * Coord.coord2d * string * Types.code_generation_phase list
val algorithm_cond_content :
  string ->
  string ->
  string * int ->
  string * string * Types.algorithm_class * string list *
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list *
  (string * string * string * Symbolic.expression list * (string * int) *
   Coord.coord2d * Types.repetition_type * string)
  list *
  ((string * string) * (string * string) * Types.dependence_class *
   (string * int))
  list * Coord.coord2d * string * Types.code_generation_phase list
(* val is_precedence_port : Types.port_type -> bool *)
val is_memory_reference : string -> string -> string -> bool
val port_move :
  string -> string -> string -> Port.direction -> int * int -> unit
val ref_move : string -> string -> string -> int * int -> unit
val atomic : string -> string -> bool
val globalexpression_of_localexpression :
  string list -> Symbolic.expression -> Symbolic.expression
val context_global : unit -> (string * Symbolic.expression) list
(* val connectable :
  string ->
  string ->
  int -> (string * string) * (string * string) -> bool -> bool * string *)
(* val dependence_create :
  (string * string) * (string * string) * Types.dependence_class *
  Types.condition_type ->
  Types.port_type list -> Types.reference_type list -> Types.dependence_type *)
val port_add :
  string ->
  string ->
  string * Port.direction * string * Symbolic.expression *
  Port.port_class * int * Coord.coord2d -> unit
val reference_add :
  string ->
  string ->
  string * string * string * Symbolic.expression list *
  Types.condition_type * Coord.coord2d * Types.repetition_type * string ->
  unit
val algo_list : unit -> (string * string) list
val port_types_list : unit -> string list
val dependence_add :
  string ->
  string ->
  (string * string) * (string * string) * Types.dependence_class *
  Types.condition_type -> bool -> unit
val algorithm_create :
  string ->
  string ->
  Types.algorithm_class ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list ->
  (string * string * string * Symbolic.expression list *
   Types.condition_type * Coord.coord2d * Types.repetition_type * string)
  list ->
  ((string * string) * (string * string) * Types.dependence_class *
   Types.condition_type)
  list -> Coord.coord2d -> string -> Types.code_generation_phase list -> unit
val constant_create :
  string ->
  string ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list -> Coord.coord2d -> string -> unit
val sensor_create :
  string ->
  string ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list -> Coord.coord2d -> string -> unit
val actuator_create :
  string ->
  string ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list -> Coord.coord2d -> string -> unit
val memory_create :
  string ->
  string ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list ->
  Coord.coord2d ->
  string -> Symbolic.expression * Symbolic.expression -> unit
val operation_create :
  string ->
  string ->
  string list ->
  (string * Port.direction * string * Symbolic.expression *
   Port.port_class * int * Coord.coord2d)
  list ->
  ((string * int) *
   (string * (string * string) * Symbolic.expression list * Coord.coord2d *
    int * string)
   list *
   ((string * string) * (string * string) * Types.dependence_class) list)
  list -> Coord.coord2d -> string -> Types.code_generation_phase list -> unit
val cond_list : string -> string -> string * int list
val ports_order_sort_libname : string -> string -> unit
val dependence_delete :
  string -> string -> string -> string -> 'a -> string * int -> unit
val port_delete : string -> string -> string -> unit
val ref_delete : string -> string -> string -> unit
val algo_delete : string -> string -> unit
val algo_main_set : string * string -> Symbolic.expression list -> unit
val algo_is_main : string -> string -> bool
val conditioned : string -> string -> bool
val algo_dimension_window_change : string -> string -> int -> int -> unit
val definition_modify_name : string -> string -> string -> unit
val definition_modify_description : string -> string -> string -> unit
val definition_modify_code_phases :
  string -> string -> Types.code_generation_phase list -> unit
val reference_modify :
  string -> string -> string -> string -> Symbolic.expression list -> unit
val reference_modify_description :
  string -> string -> string -> string -> unit
val repetition_factor_modify :
  string -> string -> string -> Types.repetition_type -> unit
val port_modify :
  string ->
  string ->
  string ->
  Port.direction ->
  string -> string -> Symbolic.expression -> int -> unit
val port_modify_class :
  string ->
  string -> string -> Port.direction -> Port.port_class -> unit
val algo_arguments_names_set : string -> string -> string list -> unit
val algo_main_arguments_values_set : Symbolic.expression list -> unit
val condition : string -> string -> string * int -> unit
val condition_delete : string -> string -> string * int -> unit
val string_of_repetition : Types.repetition_type -> string
