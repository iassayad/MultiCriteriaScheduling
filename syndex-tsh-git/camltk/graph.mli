(** Graph operations.

   This is a higher-level interface over ...
*)

type graph
(** The type of a graph canvas. *)

type item =
    Box of string                       (** XXX *)
  | Port of Port.port                   (** XXX *)
  | Line of string                      (** XXX *)

(** The type of graph items. *)

val create :
  ?options:Camltk.options list ->
  Camltk.Widget.widget -> width:Camltk.units -> height:Camltk.units -> graph
(** [Graph.create parent width height] creates a ... *)

val get_items : graph -> item list
(** Returns the list of every item currently presented on [graph]. *)

val present : graph -> item -> x:int -> y:int -> unit
(** [Graph.present graph item x y] ... *)

(** {6 Item Selection} *)

(** The [graph] canvas maintains a list of currently selected items.
   This selection list can be automatically modified by clicking on an
   item to set the selection to that item, by shift-clicking to add or
   remove the item to the current selection, or by dragging out a
   selection rectangle to select several items at once.  The following
   functions are provided to manually alter the selection .  The graphic
   representation will visually reflect the current selection at any
   time. *)

val get_selection : graph -> item list
(** Returns the current selection of items in [graph]. *)

val clear_selection : graph -> unit
(** Clear the selection of items in [graph]. *)

val add_to_selection : graph -> item -> unit
(** Add [item] to the selection of [graph] if [item] is not already
   selected. *)

val remove_from_selection : graph -> item -> unit
(** Remove [item] to the selection of [graph].  Does nothing if [item]
   is not currently selected. *)

val toggle_selected : graph -> item -> unit
(** Add or remove [item] from the selection, depending on whether it is
   selected or not. *)

val set_selection : ?clear:bool -> graph -> item list -> unit
(** Add the items to the [graph] selection. If [clear] is true (the
   default), the current selection is cleared first. *)

val select_all : graph -> unit
(** Select all items in [graph]. *)

(** {6 Miscellaneous} *)

val center_on_item : graph -> item -> unit
(** Change the canvas viewport to center the view on [item]. *)
