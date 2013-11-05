val application : Types.application_type
val get_application_description : unit -> string
val set_application_description : string -> unit
val library_referenced : string -> bool
val xscname_of_ref : string list -> Types.xsc_attachement_type -> string
val remove_ref_xsc : string list -> Types.xsc_attachement_type -> unit
val ref_modify_xsc :
  string list -> Types.xsc_attachement_type -> string -> unit
val xsc_namelist : unit -> string list
val refs_of_xsc : string -> (string list * Types.xsc_attachement_type) list
val xscname_define : string -> unit
val xscname_delete : string -> unit
val xsc_define :
  string -> (string list * Types.xsc_attachement_type) list -> unit
val constraints_create :
  string list * ((string * string) * string) list -> unit
val constraints_list :
  unit -> (string list * ((string * string) * string) list) list
val xsc_absolute_constraints_list :
  unit -> (string * ((string * string) * string) list) list
val xsc_absolute_constraints_save :
  (string * ((string * string) * string) list) list -> unit
val xsc_absolute_constraint_create :
  string * ((string * string) * string) list -> unit
val xsc_relative_constraints_list :
  unit -> (Types.relative_constraint_type * string list) list
val xsc_relative_constraints_save :
  (Types.relative_constraint_type * string list) list -> unit
val xsc_relative_constraint_create :
  Types.relative_constraint_type * string list -> unit
val algo_main_arguments_values_set : Symbolic.expression list -> unit
val algo_main_set : Types.algorithm_type -> Symbolic.expression list -> unit
val algo_main_clear : unit -> unit
val algo_main_get : unit -> string * string * Symbolic.expression list
val algo_main_argumentsvalues_get : unit -> Symbolic.expression list
val archi_main_set : Types.architecture_type -> unit
val archi_main_clear : unit -> unit
val archi_main_get : unit -> string * string
val close : unit -> unit
val is_included : string -> bool
val library_add : string -> unit
val library_remove : string -> unit
val included_libs : unit -> string list
val algorithmdef : string -> string -> Types.algorithm_type
val operatordef : string -> string -> Types.operator_definition_type
val mediadef : string -> string -> Types.media_definition_type
val architecture : string -> string -> Types.architecture_type
val operators_list : string -> string -> Types.operator_class list
val media_list : string -> string -> Types.operator_class list
