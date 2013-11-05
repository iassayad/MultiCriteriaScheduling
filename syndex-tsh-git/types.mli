val syndex_version_msg : string
val inria_msg : string
val web_msg : string
val team_msg : string
val author_msg : string
val application_msg : string
val syndex_web : string
val team_web : string
val inria_rocquencourt_web : string

type algorithm_class =
    Constant
  | Sensor
  | Actuator
  | Memory of (Symbolic.expression * Symbolic.expression)
  | Operation
  | Internal
and dependence_class =
    Strong_Precedence_Data
  | Weak_Precedence_Data
  | Precedence
  | Data
and bus_type = SamPP | SamMP | Ram
and parse_ret = Done | Include of string
and relative_constraint_type = Union | Disjunction
and adequation_type = Flatten | No_Repetition_Flatten | No_Flatten
and calculated_repetition_type = Repeat of int | Error | Undefinied
and repetition_type =
    Calculated of calculated_repetition_type
  | Specified of int
and xsc_attachement_type =
    AttachAll
  | AttachRef
  | AttachCondI
  | AttachCondO
  | AttachExplode
  | AttachImplode
and code_generation_phase = InitSeq | LoopSeq | EndSeq
and condition_type = Condition of string * int | Boolean of bool
and port_ref_type = Port of Port.port | Ref of reference_type * Port.port
and dependence_type = {
  dpd_source : port_ref_type;
  dpd_destination : port_ref_type;
  dpd_class : dependence_class;
  mutable dpd_condition : condition_type;
}
and reference_type = {
  mutable ref_name : string;
  mutable ref_algorithm : algorithm_type;
  mutable ref_arguments_values : Symbolic.expression list;
  mutable ref_condition : condition_type;
  mutable ref_position : Coord.coord2d;
  mutable ref_repetition : repetition_type;
  mutable ref_description : string;
}
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
  mutable algo_code_phases : code_generation_phase list;
}
and gate_type = { gte_name : string; gte_media_type : string; }
and operation_type = { opn_library : string; opn_name : string; }
and operator_definition_type = {
  oprdef_library : string;
  mutable oprdef_name : string;
  mutable oprdef_gates : gate_type list;
  mutable oprdef_operation_durations : (string * string, float) Hashtbl.t;
  mutable oprdef_description : string;
  mutable oprdef_code_phases : code_generation_phase list;
}
and media_definition_type = {
  mdadef_library : string;
  mutable mdadef_name : string;
  mutable mdadef_bus_type : bus_type;
  mutable mdadef_operation_durations : (string, float * string) Hashtbl.t;
  mutable mdadef_description : string;
}
and operator_reference_type = {
  mutable oprref_name : string;
  oprref_definition : operator_definition_type;
  mutable oprref_position : Coord.coord2d;
  mutable oprref_id : int;
  mutable oprref_links2 : int array;
  mutable oprref_neighbours : link_type list;
}
and media_reference_type = {
  mutable mdaref_name : string;
  mdaref_definition : media_definition_type;
  mutable mdaref_broadcast : bool;
  mutable mdaref_position : Coord.coord2d;
  mutable mdaref_links2 : int array;
  mutable mdaref_neighbours : link_type list;
}
and connection_type = {
  cnc_operator : operator_reference_type;
  cnc_gate : gate_type;
  cnc_media : media_reference_type;
}
and operator_class =
    Operator of operator_reference_type
  | Media of media_reference_type
and link_type =
  (operator_class * gate_type option) * (operator_class * gate_type option)
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
and xsc_type = {
  mutable xsc_name : string;
  mutable xsc_references : (string list * xsc_attachement_type) list;
}
and application_type = {
  mutable app_algorithms : algorithm_type list;
  mutable app_algorithm_main : algorithm_type option;
  mutable app_algorithm_main_arguments_values : Symbolic.expression list;
  mutable app_operator_definitions : operator_definition_type list;
  mutable app_media_definitions : media_definition_type list;
  mutable app_architectures : architecture_type list;
  mutable app_architecture_main : architecture_type option;
  mutable app_software_components : xsc_type list;
  mutable app_reverse_sc :
    (string list * xsc_attachement_type, xsc_type) Hashtbl.t;
  mutable app_constraints :
    (string list * ((string * string) * string) list) list;
  mutable app_xsc_absolute_constraints :
    (string * ((string * string) * string) list) list;
  mutable app_xsc_relative_constraints :
    (relative_constraint_type * string list) list;
  mutable app_libraries : string list;
  mutable app_description : string;
}

val ps : string -> unit
val debug_ps : 'a -> 'a -> string -> unit
val cut : string -> int -> string

val intersection : 'a list -> 'a list -> 'a list
val exclusion : 'a list -> 'a list -> 'a list
val intersections : 'a list list -> 'a list
val remove_copies : 'a list -> 'a list
val union : 'a list list -> 'a list

val list_min_elements_value : ('a -> 'b) -> 'a list -> 'b -> 'a list * 'b
val list_max_elements_value : ('a -> 'b) -> 'a list -> 'b -> 'a list * 'b
val list_min_elements : ('a -> 'b) -> 'a list -> 'b -> 'a list
val list_max_elements : ('a -> 'b) -> 'a list -> 'b -> 'a list
val list_min_value : ('a -> 'b) -> 'a list -> 'b -> 'b
val list_max_value : ('a -> 'b) -> 'a list -> 'b -> 'b

val list_of_hashtbl : ('a, 'b) Hashtbl.t -> 'b list
val hashtbl_filter : ('a -> bool) -> ('b, 'a) Hashtbl.t -> 'a list
val hashtbl_length : ('a, 'b) Hashtbl.t -> int

val string_of_string_list : string list -> string -> string
val analyze : string -> string -> string list list * string
val string_of_code_phase : code_generation_phase -> string
val string_of_ref_type : string -> string -> string
val string_of_argsnames : string list -> string -> string
val string_of_argsvalues : Symbolic.expression list -> string
val string_of_dimension : Symbolic.expression -> string
val string_of_direction : Port.direction -> string
val string_of_condition : condition_type -> string
val string_of_dpd : dependence_type -> string
val string_of_condlist : condition_type list -> string
val string_of_attachtype : xsc_attachement_type -> string

val file_write : string -> string -> unit
val exec : string -> string list -> int
