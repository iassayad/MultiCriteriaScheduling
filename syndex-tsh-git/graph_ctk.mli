open Camltk

val gs_program : string ref

type graph_option =
    EnableSelection
  | MoveDirection
  | ReadOnly
  | ShowBubble
  | MoveEdges
and special_tags = Movable | Zoomable
val special_tags_list : (special_tags * Tk.tagOrId) list
type port_type = PIn | POut | PInOut
and ('a, 'b) graph_item = Edge of ('a, 'b) edge | Vertice of ('a, 'b) vertex
and 'a portid = string * 'a * string
and ('a, 'b) edgeid = 'a portid * 'a portid * 'b
and 'a vertexid = string * 'a
and ('a, 'b) e_draw_f =
  'b -> string * string * string -> ('a, 'b) edgeid -> unit
and ('a, 'b) e_enter_f = ('a, 'b) edgeid -> unit
and ('a, 'b) e_leave_f = ('a, 'b) edgeid -> unit
and ('a, 'b) e_select_f = ('a, 'b) edgeid -> unit
and ('a, 'b) e_delete_f = ('a, 'b) edgeid -> unit
and ('a, 'b) e_changetype_f = ('a, 'b) edgeid -> 'b -> unit
and ('a, 'b) e_infostr_f = ('a, 'b) edgeid -> string
and ('a, 'b) edge = {
  e_s : ('a, 'b) port;
  e_d : ('a, 'b) port;
  mutable e_type : 'b;
  e_tag : string;
  mutable e_draw : ('a, 'b) e_draw_f;
  e_enter : ('a, 'b) e_enter_f;
  e_leave : ('a, 'b) e_leave_f;
  e_select : ('a, 'b) e_select_f;
  e_delete : ('a, 'b) e_delete_f;
  e_changetype : ('a, 'b) e_changetype_f;
  e_infostr : ('a, 'b) e_infostr_f;
}
and ('a, 'b) p_connect_f = ('a, 'b) edgeid -> unit
and ('a, 'b) port = {
  mutable p_name : string;
  p_dir : port_type;
  p_vertex : ('a, 'b) vertex;
  p_connect : ('a, 'b) p_connect_f;
  p_authorized_edges : 'b list;
  p_tag : string;
}
and v_draw_f = string * string * (int * int) -> unit
and 'a v_enter_f = 'a vertexid -> unit
and 'a v_leave_f = 'a vertexid -> unit
and 'a v_select_f = 'a vertexid -> unit
and v_move_f = string -> int * int -> unit
and v_delete_f = string -> unit
and 'a v_infostr_f = 'a vertexid -> string
and ('a, 'b) v_refresh_ports_edges_f =
  'a vertexid ->
  (string * port_type * ('a, 'b) p_connect_f * 'b list) list *
  (string * string * string * string * 'b) list
and ('a, 'b) vertex = {
  v_type : 'a;
  mutable v_name : string;
  mutable v_ports : ('a, 'b) port list;
  v_draw : v_draw_f;
  v_enter : 'a v_enter_f;
  v_leave : 'a v_leave_f;
  v_select : 'a v_select_f;
  v_move : v_move_f;
  v_delete : v_delete_f;
  v_infostr : 'a v_infostr_f;
  v_refresh_ports_edges : ('a, 'b) v_refresh_ports_edges_f;
  v_tag : string;
  v_special_tags : special_tags list;
  mutable v_pos : int * int;
}
and ('a, 'b) edge_info_f =
  ('a, 'b) graph ->
  'b ->
  (special_tags list * ('a, 'b) e_draw_f * ('a, 'b) e_enter_f *
   ('a, 'b) e_leave_f * ('a, 'b) e_select_f * ('a, 'b) e_changetype_f *
   ('a, 'b) e_delete_f * ('a, 'b) e_infostr_f)
  option
and ('a, 'b) clipboard = ('a vertexid list * ('a, 'b) edgeid list) ref
and ('a, 'b) copy_f = 'a vertexid list * ('a, 'b) edgeid list -> unit
and ('a, 'b) vertex_paste_f = ('a, 'b) graph -> 'a vertexid -> string -> unit
and ('a, 'b) edge_paste_f = ('a, 'b) graph -> ('a, 'b) edgeid -> unit
and ('a, 'b) canvas_draw_f = ('a, 'b) graph -> unit
and ('a, 'b) graph = {
  mutable g_vertices : ('a, 'b) vertex list;
  mutable g_edges : ('a, 'b) edge list;
  g_canvas : Widget.widget;
  mutable g_menu_canvas : Widget.widget;
  g_canvas_draw : ('a, 'b) canvas_draw_f option;
  g_enter : unit -> unit;
  g_leave : unit -> unit;
  g_newedge_info : ('a, 'b) edge_info_f;
  g_vtypelist : (string * 'a * Widget.widget) list;
  g_etypelist : (string * 'b * Widget.widget) list;
  g_etypedefault : Textvariable.textVariable;
  g_etypecurrent : Textvariable.textVariable;
  g_clipboard : ('a, 'b) clipboard;
  g_copy : ('a, 'b) copy_f;
  g_vertex_paste : ('a, 'b) vertex_paste_f;
  g_edge_paste : ('a, 'b) edge_paste_f;
  g_bindwidget : Widget.widget;
  g_view_scrollbar_x : Widget.widget;
  g_scale_x_scrollbar : Widget.widget;
  g_scale_y_scrollbar : Widget.widget;
  mutable g_scale_x : float;
  mutable g_scale_y : float;
  mutable g_scale_function : Widget.widget -> float -> string -> unit;
  mutable g_options :
    (graph_option * (string * Textvariable.textVariable option)) list;
  g_directory : unit -> string;
}
val canvas : ('a, 'b) graph -> Widget.widget

val conv_window_canvas : Widget.widget -> int -> int -> int * int
val set_scrollregion : Widget.widget -> unit
val origin_pan_x : Widget.widget -> unit
(* val origin_pan_y : Widget.widget -> unit *)
val origin_pan : Widget.widget -> unit

val vertexid_of_vertex : ('a, 'b) vertex -> string * 'a
(* val edgeid_of_edge :
  ('a, 'b) edge -> (string * 'a * string) * (string * 'a * string) * 'b*)
val postscript : ('a, 'b) graph -> unit
val jpeg : ('a, 'b) graph -> unit

val find_vertex : ('a, 'b) graph -> string -> ('a, 'b) vertex

val option_get : ('a, 'b) graph -> graph_option -> string
val option_set : ('a, 'b) graph -> graph_option -> string -> unit

val rename_vertex : ('a, 'b) graph -> string -> string -> unit
val unselect_all : Widget.widget -> unit
val select_vertex : ('a, 'b) graph -> string * 'c -> unit
val conv_zoom : Widget.widget -> float -> string -> int * int * float * float

val refresh : ('a, 'b) graph -> unit

val update_menu_edit : ('a, 'b) graph -> Widget.widget -> unit
val update_menu_canvas : ('a, 'b) graph -> Widget.widget -> unit
val update_menu_vertices : ('a, 'b) graph -> unit
(* val update_menu_edges : ('a, 'b) graph -> unit*)

val vertex_create :
  ('a, 'b) graph ->
  string ->
  'a ->
  int * int ->
  bool ->
  special_tags list ->
  (string * port_type * ('a, 'b) p_connect_f * 'b list) list ->
  v_draw_f ->
  'a v_enter_f ->
  'a v_leave_f ->
  'a v_select_f ->
  v_move_f ->
  v_delete_f -> 'a v_infostr_f -> ('a, 'b) v_refresh_ports_edges_f -> unit
(* val edge_create :
  ('a, 'b) graph ->
  string * string * string * string * 'b ->
  special_tags list * ('a, 'b) e_draw_f * ('a, 'b) e_enter_f *
  ('a, 'b) e_leave_f * ('a, 'b) e_select_f * ('a, 'b) e_changetype_f *
  ('a, 'b) e_delete_f * ('a, 'b) e_infostr_f -> unit *)
val edge_create_from_etype :
  ('a, 'b) graph -> string * string * string * string * 'b -> unit
(* val edge_create_from_stype :
  ('a, 'b) graph -> string * string * string * string * string -> unit *)

val create_clipboard : unit -> ('a list * 'b list) ref
val selected : ('a, 'b) graph -> 'a vertexid list * ('a, 'b) edgeid list

val reset : ('a, 'b) graph -> unit
val create :
  Widget.widget ->
  Widget.widget ->
  ('a, 'b) canvas_draw_f option ->
  (unit -> unit) ->
  (unit -> unit) ->
  ('a, 'b) edge_info_f ->
  ('a, 'b) clipboard ->
  ('a, 'b) copy_f ->
  ('a, 'b) vertex_paste_f ->
  ('a, 'b) edge_paste_f ->
  (string * 'a) list ->
  (string * 'b) list ->
  'b ->
  (Tk.scrollValue -> unit) ->
  (Tk.scrollValue -> unit) ->
  (Widget.widget -> float -> string -> unit) ->
  bool -> (unit -> string) -> Widget.widget list option -> ('a, 'b) graph

val canvas_size : ('a, 'b) graph -> int * int -> unit
val canvas_scale : ('a, 'b) graph -> float -> unit

val configure_resize : ('a, 'b) graph -> (int * int -> unit) -> unit
val configure_scale : ('a, 'b) graph -> float -> float -> float -> unit
val tag_of_vertice : ('a, 'b) graph -> string * 'c -> string
val tag_of_port :
  ('a, 'b) graph -> string * 'c -> string * port_type -> string
val add_specialtag : ('a, 'b) graph -> Tk.tagOrId -> special_tags -> unit

val vertex_refresh_graph : ('a, 'b) graph -> string * 'c -> unit
val vertex_remove_graph : ('a, 'b) graph -> string * 'c -> unit
val edge_remove_graph :
  ('a, 'b) graph -> string -> string -> string -> string -> unit
val unselect_all_graph : ('a, 'b) graph -> unit

val get_menu_canvas : ('a, 'b) graph -> Widget.widget
(* val get_menu_edge : ('a, 'b) graph -> 'b -> Widget.widget*)
val get_menu_vertex : ('a, 'b) graph -> 'a -> Widget.widget

val graph_fit : ('a, 'b) graph -> unit
val place_oriented :
  ('a, 'b) graph ->
  (('a, 'b) vertex -> ('a, 'b) vertex list) -> int * int -> int * int -> unit
val place_non_oriented : ('a, 'b) graph -> int * int -> int * int -> unit
