val operator_reference :
  string -> string -> string -> Types.operator_reference_type
val media_reference :
  string -> string -> string -> Types.media_reference_type
val operator_durations_add :
  string * string -> ((string * string) * float) list -> unit
val operator_durations_save :
  string -> string -> ((string * string) * float) list -> unit
val media_durations_add :
  string * string -> ((string * string) * float) list -> unit
val media_durations_save :
  string -> string -> ((string * string) * float) list -> unit
val operator_gates_save : string -> string -> (string * string) list -> unit
val operator_modify_code_phases :
  string -> string -> Types.code_generation_phase list -> unit
val operator_definition_create :
  string ->
  string ->
  (string * string) list ->
  ((string * string) * float) list ->
  string -> Types.code_generation_phase list -> unit
val media_definition_create :
  string ->
  string ->
  Types.bus_type -> ((string * string) * float) list -> string -> unit
val operator_main_set : string * string -> string -> unit
val operator_main_get : string -> string -> string
val media_referencename_content :
  string ->
  string -> string -> string * string * string * bool * Coord.coord2d
val operator_referencename_content :
  string -> string -> string -> string * string * string * Coord.coord2d
val operator_definitionname_content :
  string ->
  string ->
  string * string * (string * string) list *
  ((string * string) * float) list * string *
  Types.code_generation_phase list
val media_definitionname_content :
  string ->
  string ->
  string * string * Types.bus_type * ((string * string) * float) list *
  string
val architecturename_content :
  string ->
  string ->
  string * string * (string * string * string * Coord.coord2d) list *
  string * (string * string * string * bool * Coord.coord2d) list *
  (string * string * string) list * Coord.coord2d * string
val media_reference_add :
  string ->
  string -> string * (string * string) * bool * Coord.coord2d -> unit
val operator_reference_add :
  string -> string -> string * (string * string) * Coord.coord2d -> unit
val connection_add : string -> string -> (string * string) * string -> unit
val architecture_create :
  string ->
  string ->
  (string * (string * string) * Coord.coord2d) list ->
  string ->
  (string * (string * string) * bool * Coord.coord2d) list ->
  ((string * string) * string) list -> Coord.coord2d -> string -> unit
val media_reference_modify : string -> string -> string -> string -> unit
val operator_reference_modify : string -> string -> string -> string -> unit
val operator_move : string -> string -> string -> int * int -> unit
val media_move : string -> string -> string -> int * int -> unit
val architectures_list : unit -> (string * string) list
val operatortypes_list : unit -> (string * string) list
val operators_list : string -> string -> string list
val media_list : string -> string -> string list
val mediatypes_list : unit -> (string * string) list
val connection_delete :
  string -> string -> string -> string -> string -> unit
val operator_reference_delete : string -> string -> string -> unit
val media_reference_delete : string -> string -> string -> unit
val media_broadcast_change : string -> string -> string -> bool -> unit
val media_bustype_change : string -> string -> Types.bus_type -> unit
val bustype : Types.media_reference_type -> Types.bus_type * bool
val archi_main_set : string * string -> unit
val archi_is_main : string -> string -> bool
val operator_definition_delete : string -> string -> unit
val media_definition_delete : string -> string -> unit
val architecture_delete : string -> string -> unit
val archi_dimension_window_change : string -> string -> int -> int -> unit
val links_operator :
  Types.architecture_type ->
  Types.operator_reference_type ->
  Types.link_type list
val links_media :
  Types.architecture_type ->
  Types.media_reference_type ->
  Types.link_type list
val links : string -> string -> Types.operator_class -> Types.link_type list
val able_to_execute :
  string -> string -> Types.operator_reference_type -> bool
val operators_able_to_execute :
  string -> string -> string -> string -> Types.operator_reference_type list
val operator_reference_duration :
  Types.operator_reference_type -> string -> string -> float
val media_reference_duration : Types.media_reference_type -> string -> float
val operator_duration_average : string -> string -> float
