val create_scrollbar :
  parent:Camltk.Widget.widget ->
  canvas:Camltk.Widget.widget -> Camltk.orientation -> Camltk.Widget.widget
(** Create a scrollbar using [parent] and [orientaton], and configure
   the bindings with [canvas]. *)

val update_region : Camltk.Widget.widget -> unit
(** [Scroll.update_region canvas] updates the [canvas] ScrollRegion to
   the bounding box containing all items. *)

val center_on_position : Camltk.Widget.widget -> int * int -> unit
(** [Scroll.center_on_position canvas (x, y)] sets the [canvas]
   xview/yview to center the viewport on [(x, y)]. *)

val transform_point : Camltk.Widget.widget -> int * int -> int * int
(** [Scroll.transform_point canvas (x, y)] converts the viewport
   coordinates in absolute coordinates relative to the [canvas]. *)
