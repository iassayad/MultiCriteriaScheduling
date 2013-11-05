val consistancy_check :
  ('a, Adequationtypes.operation_type) Hashtbl.t -> string list
val transform :
  Types.adequation_type ->
  string -> string -> (string, Adequationtypes.operation_type) Hashtbl.t
