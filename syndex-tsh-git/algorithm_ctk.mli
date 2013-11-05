val xsc_define :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top -> unit
val xsc_delete :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top -> string -> unit
val algo_close :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top -> string list -> unit
val algo_menus_update :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_algo *
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class) Graph_ctk.graph ->
  unit
val algo_open :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top ->
  Ihmcommon_ctk.bindcommand_f option ->
  string -> string -> string list -> unit
val operation_definition_edit :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val operation_definition_delete :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top -> string -> unit
val operation_definition_create :
  (Ihmcommon_ctk.algo_vertice_type, Types.dependence_class, 'a, 'b, 'c, 'd)
  Ihmcommon_ctk.win_top ->
  Ihmcommon_ctk.bindcommand_f option -> Types.algorithm_class -> unit
val port_types_list_view :
  ('a, 'b, 'c, 'd, 'e, 'f) Ihmcommon_ctk.win_top -> unit
