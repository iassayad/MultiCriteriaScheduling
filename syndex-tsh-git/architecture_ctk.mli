val operator_definition_edit :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val operator_definition_new :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val operator_definition_delete :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> string -> unit
val media_definition_edit :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val media_definition_new :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val media_definition_delete :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> string -> unit
val archi_close :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> string * string -> unit
val archi_menus_update :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_archi *
  (Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type)
  Graph_ctk.graph -> unit
val archi_open :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top ->
  Ihmcommon_ctk.bindcommand_f option -> string -> string -> unit
val architecture_definition_edit :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
val architecture_definition_delete :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> string -> unit
val architecture_definition_create :
  ('a, 'b, Ihmcommon_ctk.archi_vertice_type, Ihmcommon_ctk.archi_edge_type,
   'c, 'd)
  Ihmcommon_ctk.win_top -> Ihmcommon_ctk.bindcommand_f option -> unit
