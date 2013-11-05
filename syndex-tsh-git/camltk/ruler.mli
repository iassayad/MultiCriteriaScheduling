(** Rulers are usually used to show the mouse pointer's location in a
  window. For the moment it only provides a graduation bar for the
  window. *)

type metric
(** The metric used to format the tick drawing. *)

type t
(** The type of a ruler. *)

val create : Camltk.Widget.widget -> width:int -> height:int -> Camltk.Tk.orientation -> t
(** Returns a new ruler of size [width] * [height]. *)

val create_metric : font:string -> ticks_looks:((int * bool) list) -> metric
(** Returns a new metric, which can be used to specify the look'n'feel
   of a ruler. The text of a tick will use [font] as its font and each
   couple [(length, show_value)] of [ticks_look] specifies the length of
   the tick and wether it shall display corresponding numeric
   value. [ticks_looks] is used as a circular list by draw_ticks to draw
   the ticks of a ruler. *)

val get_metric : t -> metric
(** Returns the metric of ruler. *)

val set_metric : t -> metric -> unit
(** Sets the metric of ruler. *)

val get_orientation : t -> Camltk.Tk.orientation
(** Returns the orientation of ruler. *)

val set_orientation : t -> Camltk.Tk.orientation -> unit
(** Sets the orientation of ruler. *)

val get_range_lower : t -> float
(** Returns the lower value of ruler. *)

val get_range_upper : t -> float
(** Returns the upper value of ruler. *)

val set_range : t -> lower:float -> upper:float -> max_size:int -> unit
(** Sets the range of ruler from lower to upper, [max_size] is the
  available size in pixels in which to draw the ruler *)

val get_range : t -> (float * float * int)
(** Returns the range of ruler. *)

val draw_ticks : t -> unit
(** Draws the ticks of ruler. *)
