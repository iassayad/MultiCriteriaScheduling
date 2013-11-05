val synchronize :
  (string, Adequationtypes.operation_type) Hashtbl.t ->
  (string, Adequationtypes.operation_type) Hashtbl.t
val generate_code :
  string ->
  (string, Adequationtypes.operation_type) Hashtbl.t -> bool -> unit
val generated_files : string -> string list
