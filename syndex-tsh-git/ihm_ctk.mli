open Camltk

val adequation_graph :
  ((string, Adequationtypes.operation_type) Hashtbl.t option *
   (string,
    Types.operator_class *
    (Adequationtypes.condition_type list, Adequationtypes.schedule_type)
    Hashtbl.t)
   Hashtbl.t option)
  ref
val main_window_title : Widget.widget -> string option -> bool -> unit
val top_win_create : unit -> unit
