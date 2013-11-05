open Camltk

type algo_vertice_type = VReference | VPort
and archi_vertice_type = VOperator | VMedia
and archi_edge_type = EConnection
and schedule_vertice_type = VOperation | VOperator_schedule
and options_box_type = Radiobutton | Checkbutton | Field of bool
and window_type =
    WAlgorithm of string list
  | WArchitecture of (string * string)
  | WOperator of (string * string)
  | WMedia of (string * string)
  | WAbsoluteConstraints of (string * string)
  | WRelative
  | WAsk_edit_definition
  | WAsk_edit_operator
  | WAsk_edit_media
  | WAsk_edit_architecture
  | WAsk_constraints
  | WAsk_definition_reference_add of string list
  | WAsk_operator_reference_add of (string * string)
  | WAsk_media_reference_add of (string * string)
  | WAsk_port_add of string list
  | WAsk_definition_modify of string list
  | WAsk_definition_reference_name of string list
  | WAsk_definition_reference_modify of (string list * string)
  | WAsk_definition_description_modify of string list
  | WAsk_reference_description_modify of string list
  | WAsk_port_modify of (string list * (string * Port.direction))
  | WAsk_condition_add of string list
  | WAsk_create_xsc
  | WAsk_create_definition
  | WAsk_create_operator
  | WAsk_create_media
  | WAsk_create_architecture
  | WAsk_operator_reference_name of (string * string)
  | WAsk_media_reference_name of (string * string)
  | WAsk_operator_reference_modify of (string * string * string)
  | WAsk_media_reference_modify of (string * string * string)
  | WSelect_reference of string list
  | WSelect_port of string list
  | WSelect_operator_reference of string
  | WSelect_medium_reference of string
  | WSelect_sched_opn
  | WPort_types
and schedule_direction_type = VerticalSchedule | HorizontalSchedule
and bindcommand_f = Widget.widget -> unit
and ('a, 'b, 'c, 'd, 'e, 'f) win_algo = {
  alg_top : ('a, 'b, 'c, 'd, 'e, 'f) win_top;
  alg_widget : Widget.widget;
  alg_bar_cond : Widget.widget;
  alg_library : string;
  mutable alg_name : string;
  mutable alg_path : string list;
  alg_menus : (string * Widget.widget) list;
  mutable alg_dimension : int * int;
  mutable alg_variable : string;
  mutable alg_values : int list;
  mutable alg_current_value : int;
  mutable alg_buttons : (int * Widget.widget) list;
  alg_bindcommand : bindcommand_f option;
}
and ('a, 'b, 'c, 'd, 'e, 'f) win_archi = {
  arc_top : ('a, 'b, 'c, 'd, 'e, 'f) win_top;
  arc_widget : Widget.widget;
  arc_library : string;
  arc_name : string;
  arc_menus : (string * Widget.widget) list;
  mutable arc_dimension : int * int;
  arc_bindcommand : bindcommand_f option;
}
and ('a, 'b, 'c, 'd, 'e, 'f) win_schedule = {
  sch_top : ('a, 'b, 'c, 'd, 'e, 'f) win_top;
  sch_widget : Widget.widget;
  sch_menus : (string * Widget.widget) list;
  mutable sch_direction : schedule_direction_type;
  mutable sch_arrows : bool;
  mutable sch_labels : bool;
  mutable sch_fixed_colors : bool;
  mutable sch_operators_positions :
    ((string * schedule_direction_type) * int) list;
  mutable sch_scale : bool;
  mutable sch_show_constants : bool;
}
and algo_clip =
  (string * string * string * Symbolic.expression list * Coord.coord2d *
   Types.repetition_type)
  list *
  (string * Port.direction * string * Symbolic.expression *
   Coord.coord2d)
  list
and archi_clip =
  (string * string * string * bool * Coord.coord2d) list *
  (string * string * string * Coord.coord2d) list
and ('a, 'b, 'c, 'd, 'e, 'f) win_top = {
  tw_frame : Widget.widget;
  tw_info_bar : Widget.widget;
  tw_msg_bar : Widget.widget;
  mutable tw_current_directory : string;
  mutable tw_current_file : string option;
  mutable tw_chrono : bool;
  mutable tw_algos :
    (('a, 'b, 'c, 'd, 'e, 'f) win_algo * ('a, 'b) Graph_ctk.graph) list;
  mutable tw_archis :
    (('a, 'b, 'c, 'd, 'e, 'f) win_archi * ('c, 'd) Graph_ctk.graph) list;
  mutable tw_schedules :
    (('a, 'b, 'c, 'd, 'e, 'f) win_schedule * ('e, 'f) Graph_ctk.graph) list;
  mutable tw_codes : Widget.widget list;
  mutable tw_windows : (window_type * Widget.widget) list;
  mutable tw_file_changed : unit -> unit;
  mutable tw_alg_clip : algo_clip;
  mutable tw_arc_clip : archi_clip;
  tw_alg_graphclip : ('a, 'b) Graph_ctk.clipboard;
  tw_arc_graphclip : ('c, 'd) Graph_ctk.clipboard;
}
and info_type =
    Port of (string * Port.direction)
  | Reference of string
  | Dependence of
      ((string * algo_vertice_type * string) *
       (string * algo_vertice_type * string) * Types.dependence_class)
  | Operator of (string * string * string)
  | Media of (string * string * string)
  | MediaDefinition of (string * string)
  | Connection of (string * string * string)
  | Task of string
  | Ecu of string
  | Canvas
  | Clean
(* val window_from_type : window_type -> string *)
val clean_info_bar : bool ref
(* val default_autopos_step : int *)
val font : string
val underlined_font : string
val html_program : string ref
val background_color : Tk.color
val operator_color : Tk.color
val media_color : Tk.color
val constant_color : Tk.color
val memory_color : Tk.color
val operation_color : Tk.color
val actuator_color : Tk.color
val sensor_color : Tk.color
(* val other_color : Tk.color *)
val conditioningport_color : Tk.color
val initmemoryport_color : Tk.color
(* val highlight_color : Tk.color *)
(* val select_color : Tk.color *)
val canvas_size_default_x : int
val canvas_size_default_y : int
val border : int
val port_space : int
(* val menu_size_max : int *)
(* val sch_spacing : int * int *)
(* val sch_size : int * int *)
(* val sch_opspace : int * int *)
(* val sch_ratio : int * int *)
(* val tag_buffer : int ref *)
(* val tag_create : unit -> string *)
val is_initialised_tk : unit -> bool
val frame_create : Widget.widget -> Widget.widget
val button_create : Widget.widget -> string -> Tk.relief -> Widget.widget
val menubutton_create : Widget.widget -> string -> Widget.widget
val parse_expression : 'a -> string -> Symbolic.expression
val string_of_tag : Tk.tagOrId -> string
val string_of_path : string list -> string
val args_names_of_string : string -> string list * string
val args_values_of_string : 'a -> string -> Symbolic.expression list * string
val args_names_args_values_of_string :
  'a -> string -> string list * Symbolic.expression list * string
val string_of_durations : ((string * string) * float) list -> string
val durations_analyze : string -> ((string * string) * float) list * string
val get_text_width : string -> int
val text_clean : Widget.widget -> unit
val ok_message_box : CTk.Widget.widget -> string -> unit
val message_info : Widget.widget -> string -> unit
val error_message : ('a, 'b, 'c, 'd, 'e, 'f) win_top -> string -> unit
(* val evaluation_variable :
  (string * Symbolic.expression) list ->
  string -> Widget.widget -> string -> Symbolic.result *)
val evaluation_expression :
  (string * Symbolic.expression) list ->
  Symbolic.expression -> Widget.widget -> string -> Symbolic.result
val generic_window_open :
  ('a, 'b, 'c, 'd, 'e, 'f) win_top ->
  window_type -> (unit -> Widget.widget) -> unit
val generic_window_close :
  ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit
val ask_for :
  string ->
  string ->
  string ->
  (string -> unit) ->
  Widget.widget -> ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit
val ask_for_more :
  string ->
  string ->
  string ->
  string ->
  (string -> string -> unit) ->
  bool ref ->
  Widget.widget -> ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit
val ask_for_more_rel :
  string ->
  string ->
  string ->
  string ->
  string ->
  string ->
  string ->
  string ->
  (string -> string -> string -> string -> unit) ->
  bool ref ->
  Widget.widget -> ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit
val edit : Widget.widget -> string -> string -> string -> string option
val dialog_create : string -> string -> int -> string list -> int
val options_box :
  Widget.widget ->
  string ->
  string ->
  (options_box_type * string list * string * (string -> unit)) list ->
  (unit -> unit) -> unit
val autopos_step_box : Widget.widget -> int * int
val file_view : Widget.widget -> string -> Widget.widget
val html_view : 'a -> string -> int
(* val ask_for_libsdef_open :
  string ->
  (string * string -> unit) ->
  (string * string list) list ->
  Widget.widget ->
  ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> Widget.widget *)
(* val ask_for_libsdef :
  string ->
  (string * string -> unit) ->
  (string * string list) list ->
  Widget.widget -> ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit *)
val clear_menu : Widget.widget -> unit
(* val libs_defs_list : ('a * 'b) list -> ('a * 'b list) list *)
val choose_libs_defs :
  (string * string -> unit) ->
  (string * string) list ->
  string ->
  Widget.widget -> ('a, 'b, 'c, 'd, 'e, 'f) win_top -> window_type -> unit
val rec_cascade : Widget.widget -> Tk.options list list -> unit
(* val create_menu_libsdefs :
  Widget.widget ->
  (string * string list) list -> (string -> string -> unit) -> Widget.widget *)
val code_phases_edit :
  Widget.widget ->
  ('a, 'b, 'c, 'd, 'e, 'f) win_top ->
  'g ->
  'h ->
  Types.code_generation_phase list ->
  ('g -> 'h -> Types.code_generation_phase list) ->
  ('g -> 'h -> Types.code_generation_phase list -> unit) -> unit
