val adequation :
  unit -> Adequationtypes.graph_type * Adequationtypes.full_schedule_type
val generate_executive :
  string -> Adequationtypes.graph_type -> bool -> unit
val process : string -> string -> bool -> bool -> unit
