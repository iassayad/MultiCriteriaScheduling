(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            Matthieu Rouget                            *)
(*                          Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

open Camltk
open Types

(** This module contains the data structures and functions to handle graphs. *)

let border = 3
let highlight_color = Red
let bubble_color = NamedColor "khaki1"
let grid_size = 40

let gs_program = ref ""

(* -- Data structures ------------------------------------------------------ *)
(** Global graph option properties. *)
type graph_option =
  | EnableSelection (* 1 | 0 *)
  | MoveDirection   (* Both | Vertical | Horizontal | None *)
  | ReadOnly        (* 0 | 1 *)
  | ShowBubble      (* 0 | 1 *)
  | MoveEdges       (* 1 | 0 *)

(** Graph items (ie edges and vertices) special tags. *)
type special_tags = Movable | Zoomable

(** Special tags grouped in one list. *)
let special_tags_list = [(Movable,Tag "Movable");(Zoomable,Tag "Zoomable")]

(** Port direction. *)
type port_type = PIn | POut | PInOut

(** Graph items type. *)
and ('vtype,'etype) graph_item = Edge of ('vtype,'etype) edge | Vertice of ('vtype,'etype) vertex

(** portid/edgeid/vertexid : the type of identifiers, sent by graph functions *)
and 'vtype portid = string*'vtype*string 
and ('vtype,'etype) edgeid = ('vtype portid)*('vtype portid)*'etype
and 'vtype vertexid = string*'vtype

(* -- edges -- *)
(** Called when drawing an edge. *)
and ('vtype,'etype) e_draw_f = 'etype -> (string*string*string) -> ('vtype,'etype) edgeid -> unit
(** Called when pointer arrives on an edge. *)
and ('vtype,'etype) e_enter_f = ('vtype,'etype) edgeid -> unit
(** Called when pointer leaves from an edge. *)
and ('vtype,'etype) e_leave_f = ('vtype,'etype) edgeid -> unit
(** Called when an edge gets selected. *)
and ('vtype,'etype) e_select_f = ('vtype,'etype) edgeid -> unit
(** Called when an edge is being deleted. *)
and ('vtype,'etype) e_delete_f = ('vtype,'etype) edgeid -> unit
(** Called when changing the type (class) of an edge. *)
and ('vtype,'etype) e_changetype_f = ('vtype,'etype) edgeid -> 'etype -> unit
(** Returns an information string on an edge. *)
and ('vtype,'etype) e_infostr_f = ('vtype,'etype) edgeid -> string

(** Edge type.*)
and ('vtype,'etype) edge = {
    e_s : ('vtype,'etype) port;
    e_d : ('vtype,'etype) port;
    mutable e_type : 'etype;
    e_tag : string;
    (* Functions below are called when performing/to perform corresponding actions on this edge. *)
    mutable e_draw : ('vtype,'etype) e_draw_f; (* see : edge_draw *)
    e_enter : ('vtype,'etype) e_enter_f;
    e_leave : ('vtype,'etype) e_leave_f;
    e_select : ('vtype,'etype) e_select_f;
    e_delete : ('vtype,'etype) e_delete_f; (* see : edge_delete *)
    e_changetype : ('vtype,'etype) e_changetype_f; (* see : edge_changetype *)
    e_infostr : ('vtype,'etype) e_infostr_f
  }

(* -- ports -- *)
(** Called when connecting a port to another. *)
and ('vtype,'etype) p_connect_f = ('vtype,'etype) edgeid -> unit

(** Port type. *)
and ('vtype,'etype) port = {
    mutable p_name : string;
    p_dir : port_type;
    p_vertex : ('vtype,'etype) vertex;
    p_connect : ('vtype,'etype) p_connect_f; (* see : port_connect *)
    p_authorized_edges : 'etype list;
    p_tag : string;
  }

(* -- vertices -- *)
(** Called when drawing a vertex. *)
and v_draw_f = (string*string*(int*int)) -> unit
(** Called when pointer arrives on a vertex. *)
and 'vtype v_enter_f = 'vtype vertexid -> unit
(** Called when pointer leaves from a vertex. *)
and 'vtype v_leave_f = 'vtype vertexid -> unit
(** Called when a vertex gets selected. *)
and 'vtype v_select_f = 'vtype vertexid -> unit
(** Called when a vertex is moved. *)
and v_move_f = string -> int*int -> unit
(** Called when a vertex is being deleted. *)
and v_delete_f = string -> unit
(** Returns an information string on a vertex. *)
and 'vtype v_infostr_f = 'vtype vertexid -> string
(** Called to refresh a port and its connected edges. *)
and ('vtype,'etype) v_refresh_ports_edges_f = 'vtype vertexid -> (((string*port_type*('vtype,'etype) p_connect_f*('etype list)) list)*(string*string*string*string*'etype) list)

and ('vtype,'etype) vertex = {
    v_type : 'vtype;
    mutable v_name : string;
    mutable v_ports : ('vtype,'etype) port list;
    v_draw : v_draw_f; (* see : vertex_draw*)
    v_enter : 'vtype v_enter_f;
    v_leave : 'vtype v_leave_f;
    v_select : 'vtype v_select_f;
    v_move : v_move_f;
    v_delete : v_delete_f; (* see : vertex_delete *)
    v_infostr : 'vtype v_infostr_f;
    v_refresh_ports_edges : ('vtype,'etype) v_refresh_ports_edges_f;
    v_tag : string;
    v_special_tags : special_tags list;
    mutable v_pos : int*int;
  }

(* -- graph -- *)
(*and ('vtype,'etype) vertex_info_f = ('vtype,'etype) graph -> 'vtype -> (string*((string*port_type*(('vtype,'etype) p_connect_f)) list)*coord2d*v_draw_f*v_move_f*v_delete_f) list option*)
and ('vtype,'etype) edge_info_f = ('vtype,'etype) graph -> 'etype -> ((special_tags list)*('vtype,'etype) e_draw_f*('vtype,'etype) e_enter_f*('vtype,'etype) e_leave_f*('vtype,'etype) e_select_f*('vtype,'etype) e_changetype_f*('vtype,'etype) e_delete_f*('vtype,'etype) e_infostr_f) option

and ('vtype,'etype) clipboard = ('vtype vertexid list * ('vtype,'etype) edgeid list) ref
and ('vtype,'etype) copy_f = ('vtype vertexid list)*(('vtype,'etype) edgeid list) -> unit
and ('vtype,'etype) vertex_paste_f = ('vtype,'etype) graph -> 'vtype vertexid -> string -> unit
and ('vtype,'etype) edge_paste_f = ('vtype,'etype) graph -> ('vtype,'etype) edgeid  -> unit
and ('vtype,'etype) canvas_draw_f = ('vtype,'etype) graph -> unit

and ('vtype,'etype) graph = {
  mutable g_vertices : ('vtype,'etype) vertex list;
  mutable g_edges : ('vtype,'etype) edge list;
  g_canvas : Widget.widget;
  mutable g_menu_canvas : Widget.widget;
  g_canvas_draw : ('vtype,'etype) canvas_draw_f option;
  g_enter : unit -> unit;
  g_leave : unit -> unit;
  g_newedge_info : ('vtype,'etype) edge_info_f;
  g_vtypelist : (string*('vtype)*Widget.widget) list; (* typename, vtype, menu *)
  g_etypelist : (string*('etype)*Widget.widget) list; (* typename, etype, menu *)
  g_etypedefault : Textvariable.textVariable;
  g_etypecurrent : Textvariable.textVariable;
  (* Clipboard - related data *)
  g_clipboard : ('vtype,'etype) clipboard;
  g_copy : ('vtype,'etype) copy_f;
  g_vertex_paste : ('vtype,'etype) vertex_paste_f;
  g_edge_paste : ('vtype,'etype) edge_paste_f;
  (* Widget used to do key-bindings *)
  g_bindwidget : Widget.widget;
  g_view_scrollbar_x : Widget.widget;
  g_scale_x_scrollbar : Widget.widget;
  g_scale_y_scrollbar : Widget.widget;
  (* Zoom value *)
  mutable g_scale_x : float;
  mutable g_scale_y : float;
  mutable g_scale_function : Widget.widget -> float -> string -> unit;
  (* Graph  options *)
  mutable g_options : (graph_option*(string*(Textvariable.textVariable option))) list;
  g_directory : unit -> string
  }

(* -- Utilities ------------------------------------------------------------ *) 
let ps n = print_string n; print_newline ()

let tag_buffer = ref 0
let tag_create () = incr tag_buffer; "TAG" ^ (string_of_int !tag_buffer)

let tag_of_specialtags spctag = List.assoc spctag special_tags_list

let clear_menu menu = Menu.delete menu (Number 0) Last

let canvas g = g.g_canvas

let change_coords (x,y) dx dy = (x+dx,y+dy)

let change_menu cvs menu =
  bind cvs [[],ButtonPressDetail 3] (BindSet ([Ev_RootX;Ev_RootY], function ev -> Menu.popup menu ev.ev_RootX ev.ev_RootY))

let origin cvs =
  let result,_ = analyze (cget cvs CScrollRegion) "\\([-0-9]+\\)" in
  match result with
  | [a]::[b]::_ -> (int_of_string a),(int_of_string b)
  | _ -> 0,0

let conv_window_canvas cvs x y =
  let w,h = (float_of_int (Winfo.width cvs)), (float_of_int (Winfo.height cvs)) in
  let x0,y0 = origin cvs in
  let viewportx0,viewportx1 = Canvas.xview_get cvs
  and viewporty0,viewporty1 = Canvas.yview_get cvs in
  (x0+(int_of_float (w*.viewportx0/.(viewportx1-.viewportx0))+x),
   y0+(int_of_float (h*.viewporty0/.(viewporty1-.viewporty0)))+y)

let set_scrollregion cvs =
  let w,h = Winfo.width cvs, Winfo.height cvs in
  let x0,y0,x1,y1 = match Canvas.find cvs [All] with
  | [] -> 0,0,w,h
  | l -> Canvas.bbox cvs l in
  let vx0,vy0 = conv_window_canvas cvs 0 0 in
  let vx1,vy1 = (vx0+w-3),(vy0+h-3) in
  let a,b,c,d = (min vx0 x0),(min vy0 y0),(max vx1 x1),(max vy1 y1) in
  Canvas.configure cvs [ScrollRegion ((Pixels a),(Pixels b),(Pixels c),(Pixels d))]

(** orgigin_pan but only on the x axis *)
let origin_pan_x cvs =
  set_scrollregion cvs;
  let x1,x2 = Canvas.xview_get cvs in
    Canvas.xview cvs (MoveTo (1.-.x2));
    set_scrollregion cvs

(** orgigin_pan but only on the y axis *)
let origin_pan_y cvs =
  set_scrollregion cvs;
  let y1,y2 = Canvas.yview_get cvs in
    Canvas.yview cvs (MoveTo (1.-.y2));
    set_scrollregion cvs
    
(** Centers the view so that the whole canvas (and not more) is visible in the frame *)
(* I don't fully understand very well how this works, but it seems to work fine. Anyway, there's probably a better way *)
let origin_pan cvs =
  origin_pan_x cvs;
  origin_pan_y cvs

let string_of_tag tag = match tag with
| Tag s -> s
| _ -> ""

let conv_tags_port g tags =
  let f l = List.filter (function p -> List.mem p.p_tag tags) l in
  match List.concat (List.map (function v -> f v.v_ports) g.g_vertices) with
  | [] -> None
  | p::_ -> Some p

let portid_of_port p = (p.p_vertex.v_name, p.p_vertex.v_type,p.p_name)
let vertexid_of_vertex v = (v.v_name,v.v_type)
let edgeid_of_edge e = (portid_of_port e.e_s),(portid_of_port e.e_d),e.e_type

let exec prog args =
  Unix.create_process prog (Array.of_list (prog::args)) (Unix.stdin) (Unix.stdout) (Unix.stderr)

let postscript g =
  match Tk.getSaveFile [DefaultExtension ".ps";FileTypes [{typename="PostScript files";extensions=[".ps"];mactypes=[]}];InitialDir (g.g_directory ())] with
  | "" -> ()
  | filename -> file_write filename (Canvas.postscript g.g_canvas [])

let jpeg g =
  match Sys.file_exists !gs_program with
  | true ->
      (match Tk.getSaveFile [FileTypes [{typename="jpeg files";extensions=[".jpg";".jpeg"];mactypes=[]}];InitialDir (g.g_directory ())] with
      | "" -> ()
      | filename ->
	  let dirname,basename = (Filename.dirname filename),(Filename.basename filename) in
	  let filename = Filename.concat dirname (try Filename.chop_extension basename with _ -> basename) in
	  let tmpfile = filename^"tmp.ps" in
	  file_write tmpfile (Canvas.postscript g.g_canvas []);
	  let pid = exec !gs_program ["-q";"-dNOPROMPT";"-dNOPAUSE";"-dBATCH";"-sDEVICE=jpeg";"-sPAPERSIZE=a4";"-sOutputFile="^filename^"%d.jpg";tmpfile] in
	  ignore(Unix.waitpid [] pid);
	  Sys.remove tmpfile)
  | false -> raise (Failure "Cannot find gs program")

(* Bubbles ----------------------------------------------------------------- *)
let bubble_draw canvas ev tag text =
  match text with
  | "" -> ()
  | _ ->
      let w,h = (float_of_int (Winfo.width canvas)), (float_of_int (Winfo.height canvas)) in
      let x,y = ev.ev_MouseX, ev.ev_MouseY in
      let yoffset, anchor = match y > (int_of_float h)/2, x> (int_of_float w)/2 with
	| true,true -> -8,SE
	| true,false -> -8,SW
	| false,true -> 8,NE
	| false,false -> 8,NW in
      let x,y = conv_window_canvas canvas x y in
      let bubble_txt = Canvas.create_text canvas (Pixels x) (Pixels (y+yoffset)) [Anchor anchor;Tags [Tag (tag^"Bubble")]; Text text] in
      let x1,y1,x2,y2 = Canvas.bbox canvas [Tag (tag^"Bubble")] in
      let bubble_rec = Canvas.create_rectangle canvas (Pixels x1) (Pixels y1) (Pixels x2) (Pixels y2) [FillColor bubble_color; Tags [Tag (tag^"Bubble")]] in
	Canvas.lower_below canvas bubble_rec bubble_txt

(* -- Item finding functions ---------------------------------------------- *)
let exists_vertex g (vname,vtype) =
  List.exists (function v -> v.v_name=vname && v.v_type=vtype) g.g_vertices 

let find_vertex g vname =
  List.find (function {v_name=name} -> name = vname) g.g_vertices 

let find_port g vname pname dir =
  let v = find_vertex g vname in
  List.find (function p -> p.p_name=pname && p.p_dir=dir) v.v_ports

let find_pin g vname pname =
  let v = find_vertex g vname in
  List.find (function p -> p.p_name=pname && (p.p_dir=PIn || p.p_dir=PInOut)) v.v_ports

let find_pout g vname pname =
  let v = find_vertex g vname in
  List.find (function p -> p.p_name=pname && (p.p_dir=POut || p.p_dir=PInOut)) v.v_ports

let find_edge g v1_name p1_name v2_name p2_name =
  List.find (function e -> e.e_s.p_vertex.v_name=v1_name && e.e_s.p_name=p1_name && e.e_d.p_vertex.v_name=v2_name && e.e_d.p_name=p2_name) g.g_edges

(* -- Options functions ---------------------------------------------------- *)
let option_get g o =
  fst (List.assoc o g.g_options)

let option_gettv g o =
  snd (List.assoc o g.g_options)

let option_set g o oval =
  let tv = snd (List.assoc o g.g_options) in
  (match tv with
  | Some txtvar ->  Textvariable.set txtvar oval
  | None -> ());
  g.g_options <- (o,(oval,tv))::(List.remove_assoc o g.g_options)

let option_textvariable_handle g tv o =
  let rec f () = 
    option_set g o (Textvariable.get tv);
    Textvariable.handle tv f in
    f ()

let option_create g o oval tv =
  let tv = match tv with
  | true ->
      let tv = Textvariable.create () in
      Textvariable.set tv oval;
      Textvariable.handle tv (function () -> option_textvariable_handle g tv o);
      Some tv
  | false -> None in
  g.g_options <- (o,(oval,tv))::g.g_options

(* -- Items renaming functions --------------------------------------------- *)
let rename_vertex g oldname newname =
  (find_vertex g oldname).v_name <- newname

let rename_port g vname oldname newname dir =
  (find_port g vname oldname dir).p_name <- newname

(* -- Selection-related functions ------------------------------------------ *)
let select g item =
  match (option_get g EnableSelection) with
  | "1" ->
      let cvs = canvas g in
      let w = 3 in
      let p tag x y = ignore(Canvas.create_rectangle cvs (Pixels (x-w)) (Pixels (y-w)) (Pixels (x+w)) (Pixels (y+w)) [FillColor Red; Tags [Tag tag; Tag (tag^"Handle")]]) in
      let selected tag = List.mem (Tag "Selected") (Canvas.gettags cvs (Tag tag)) in
      (match item with
      | Vertice v when not (selected v.v_tag) ->
	  let tag = v.v_tag in
	  let x1,y1,x2,y2 = Canvas.bbox cvs [Tag tag] in
	  p tag x1 y1;p tag x1 y2;p tag x2 y1;p tag x2 y2;
	  Canvas.addtag cvs (Tag "Selected") [Withtag (Tag tag)];
	  Canvas.raise_top cvs (Tag tag);
	  v.v_select (vertexid_of_vertex v)
      | Edge e when not (selected e.e_tag) ->
	  let tag = e.e_tag in
	  let coords = List.map int_of_float (Canvas.coords_get cvs (Tag tag)) in
	  let x1 = List.nth coords 0 in
	  let y1 = List.nth coords 1 in
	  let x2 = List.nth coords 2 in
	  let y2 = List.nth coords 3 in
	  p tag x1 y1 ; p tag x2 y2; p tag ((x1+x2)/2) ((y1+y2)/2);
	  Canvas.addtag cvs (Tag "Selected") [Withtag (Tag tag)];
	  e.e_select (edgeid_of_edge e)
      |	_ -> ())
  | _ -> ()

let unselect cvs tag =
  Canvas.delete cvs [Tag (tag^"Handle")];
  Canvas.dtag cvs (Tag tag) (Tag "Selected")

let unselect_all cvs =
  let selectedids = Canvas.find cvs [Withtag (Tag "Selected")] in
  let selectedtags = List.concat (List.map (function id -> Canvas.gettags cvs id) selectedids) in
  List.iter (function t -> unselect cvs (string_of_tag t)) selectedtags

let select_init g item = (* Configure item to be selectable *)
  let tag = match item with
  | Vertice v -> v.v_tag
  | Edge e -> e.e_tag in
  let cvs = canvas g in
  let clickselect _ = match List.mem (Tag "Selected") (Canvas.gettags cvs (Tag tag)) with
  | true -> ()
  | false -> unselect_all cvs; select g item in
  let shiftselect _ = match List.mem (Tag "Selected") (Canvas.gettags cvs (Tag tag)) with
  | true -> unselect cvs tag
  | false -> select g item in
  Canvas.bind cvs (Tag tag) [[],ButtonPressDetail 1] (BindExtend ([],clickselect));
  Canvas.bind cvs (Tag tag) [[],ButtonPressDetail 3] (BindExtend ([],clickselect));
  Canvas.bind cvs (Tag tag) [[Shift],ButtonPressDetail 1] (BindSet ([],shiftselect))

let selection_get g =
  let ids = Canvas.find g.g_canvas [Withtag (Tag "Selected")] in
  let tags = List.concat (List.map (function id -> Canvas.gettags g.g_canvas id) ids) in
  let stags = List.map string_of_tag tags in
  let vertices = List.filter (function {v_tag=tag} -> List.mem tag stags) g.g_vertices in
  let edges = List.filter (function {e_tag=tag} -> List.mem tag stags) g.g_edges in
  (vertices,edges)

let select_vertex graph vertex =
  select graph (Vertice (find_vertex graph (fst vertex)))

(* -- Zoom functions ------------------------------------------------------- *)
let conv_zoom canvas zfactor zoomdir=
  let zx,zy = conv_window_canvas canvas ((Winfo.width canvas)/2) ((Winfo.height canvas)/2) in
  match zoomdir with
  | "Both" -> zx, zy, zfactor, zfactor
  | "Horizontal" -> zx, 0, zfactor, 1.
  | "Vertical" -> 0, zy, 1., zfactor
  | _ -> 0, 0, 1., 1.

let zoom g s zoomdir (v_selection,e_selection) =
  let zfactor = match zoomdir with
    | "Both" -> s /. g.g_scale_x
    | "Horizontal" -> s /. g.g_scale_x
    | "Vertical" -> s /. g.g_scale_y
    | _ -> s /. g.g_scale_x in
    (match zoomdir with
    | "Both" -> g.g_scale_x <- s
    | "Horizontal" -> g.g_scale_x <- s
    | "Vertical" -> g.g_scale_y <- s
    | _ -> ());
  let zx,zy,factorx,factory = conv_zoom g.g_canvas zfactor zoomdir in
  List.iter (function t -> Canvas.scale g.g_canvas t (Pixels zx) (Pixels zy) factorx factory) (Canvas.find g.g_canvas [Withtag (tag_of_specialtags Zoomable)]);
    List.iter (fun v -> select g (Vertice v)) v_selection;
    List.iter (fun e -> select g (Edge e)) e_selection;
  g.g_scale_function g.g_canvas s zoomdir

(* -- Deletion-related functions ------------------------------------------ *)
let edge_remove g e =
  (* Remove the edge from the canvas *)
  Canvas.delete g.g_canvas [Tag e.e_tag];
  (* Remove the edge from the graph vertices list *)
  g.g_edges <- List.filter ((<>) e) g.g_edges

let port_remove g p =
  (* Remove the port from the canvas *)
  Canvas.delete g.g_canvas [Tag p.p_tag];
  (* Remove edges *)
  List.iter (function {e_s=s;e_d=d} as e -> match s=p || d=p with
  | true -> edge_remove g e
  | false -> ()) g.g_edges;
  (* Remove the port from the vertex *)
  p.p_vertex.v_ports <- List.filter ((<>) p) p.p_vertex.v_ports

let edge_delete g e =
  (* Cast the external delete function *)
  e.e_delete (edgeid_of_edge e);
  edge_remove g e

let vertex_remove g v =
  (* Delete the edge linked to this vertex *)
  let edges = List.filter (function e -> e.e_s.p_vertex=v || e.e_d.p_vertex=v) g.g_edges in
  List.iter (edge_remove g) edges;
  (* Remove the vertex from the canvas *)
  Canvas.delete g.g_canvas [Tag v.v_tag];
  (* Remove the vertive from the graph vertices list *)
  g.g_vertices <- List.filter ((<>) v) g.g_vertices

let vertex_delete g v =
  (* Delete the edge linked to this vertex *)
  let edges = List.filter (function e -> e.e_s.p_vertex=v || e.e_d.p_vertex=v) g.g_edges in
  List.iter (edge_delete g) edges;
  (* Cast the external delete function *)
  v.v_delete (v.v_name);
  vertex_remove g v

let selection_delete g =
  match (option_get g ReadOnly)="0" with
  | true ->
      let (vertices,edges) = selection_get g in
      List.iter (edge_delete g) edges;
      List.iter (vertex_delete g) vertices
  | false -> ()

(* -- Conversion functions ------------------------------------------------- *)
let etype_of_texttype g a = 
  let (_,c,_) = (List.find (function (b,_,_) -> a=b) g.g_etypelist) in c

let texttype_of_etype g a =
  let (c,_,_) = (List.find (function (_,b,_) -> a=b) g.g_etypelist) in c

let menu_of_etype g a =
  let (_,_,c) = (List.find (function (_,b,_) -> a=b) g.g_etypelist) in c

let vtype_of_texttype g a =
  let (_,c,_) = (List.find (function (b,_,_) -> a=b) g.g_vtypelist) in c

let texttype_of_vtype g a =
  let (c,_,_) = (List.find (function (_,b,_) -> a=b) g.g_vtypelist) in c

let menu_of_vtype g a =
  let (_,_,c) = (List.find (function (_,b,_) -> a=b) g.g_vtypelist) in c

let edge_draw g e =
  e.e_draw e.e_type (e.e_tag,e.e_s.p_tag,e.e_d.p_tag) (edgeid_of_edge e)

let edge_redraw g e =
  let tags = Canvas.gettags g.g_canvas (Tag e.e_tag) in
  let selected = List.mem (Tag "Selected") tags in
  Canvas.delete g.g_canvas [Tag e.e_tag];
  edge_draw g e;
  List.iter (function t -> Canvas.addtag (canvas g) t [Withtag (Tag e.e_tag)]) tags;
  match selected with
  | true -> select g (Edge e)
  | false -> ()

let edge_changetype g e etype =
  match (List.mem etype e.e_s.p_authorized_edges) && (List.mem etype e.e_d.p_authorized_edges) with
  | true ->
      let s = portid_of_port e.e_s in
      let d = portid_of_port e.e_d in
      e.e_changetype (s,d,e.e_type) etype;
      e.e_type <- etype;
      edge_redraw g e
  | false -> ()

let selection_changetype g etype =
  let (_,edges) = selection_get g in
  List.iter (function e -> edge_changetype g e etype) edges

let vertex_draw g v =
  v.v_draw (v.v_name,v.v_tag,v.v_pos)

(* -- Generic 3-step action function (drag, move and drop), 
      used for rectangle selection, edge creation  and moving -------------- *)
let action_init cvs tag scroll extend initevent moveevent releaseevent init_action move_action release_action =
  let lastx, lasty, firstx, firsty = ref 0, ref 0, ref 0, ref 0 in
  let move ev init =
    let mx,my = ev.ev_MouseX, ev.ev_MouseY in
    let x,y = match scroll with
    | true ->
	let wc,hc = Winfo.width cvs, Winfo.height cvs in
	let dx,xl = if mx<0 then (float_of_int mx),0 else if mx>wc then float_of_int (mx-wc),wc else 0.0,mx
	and dy,yl = if my<0 then (float_of_int my),0 else if my>hc then float_of_int (my-hc),hc else 0.0,my in
	Canvas.xview cvs (MoveTo (fst (Canvas.xview_get cvs) +. dx*.0.005));
	Canvas.yview cvs (MoveTo (fst (Canvas.yview_get cvs) +. dy*.0.005));
	xl,yl
    | false -> mx,my in
    (match move_action with
    | Some move -> move init (mx - !lastx) (my - !lasty) x y
    | None -> ());
    lastx := mx; lasty := my in
  let release init = 
    (match tag with
    | None ->
      	bind cvs moveevent BindRemove;
      	bind cvs releaseevent BindRemove
    | Some tag ->
	Canvas.bind cvs (Tag tag) moveevent BindRemove;
	Canvas.bind cvs (Tag tag) releaseevent BindRemove);
    match release_action with
    | Some release -> release init !firstx !firsty !lastx !lasty
    | None -> () in
  let move_begin ev =
    let mx,my = ev.ev_MouseX, ev.ev_MouseY in
    lastx := mx; lasty := my; firstx := mx; firsty := my;
    let init = match init_action with
    | Some init -> init mx my
    | None -> None in
    (match tag with
    | None ->
	bind cvs moveevent (BindSet ([Ev_MouseX;Ev_MouseY],(function ev -> move ev init)));
	bind cvs releaseevent (BindSet ([],(function _ -> release init)))
    | Some tag ->
	Canvas.bind cvs (Tag tag) moveevent (BindSet ([Ev_MouseX;Ev_MouseY],(function ev -> move ev init)));
	Canvas.bind cvs (Tag tag) releaseevent (BindSet ([],(function _ -> release init)))) in
  let bindaction = match extend with
  | false -> BindSet ([Ev_MouseX;Ev_MouseY],move_begin)
  | true -> BindExtend ([Ev_MouseX;Ev_MouseY],move_begin) in
  match tag with
  | None ->  bind cvs initevent bindaction
  | Some tag -> Canvas.bind cvs (Tag tag) initevent bindaction
	
(* -- Item selection by a rectangle on the canvas -------------------------- *)
let select_canvas_init g =
  let cvs = g.g_canvas in
  let init shift x0 y0 =
    let tagdrag = Tag (tag_create ()) in
    let x0,y0 = conv_window_canvas cvs x0 y0 in
    (match shift with
    | true -> ()
    | false -> unselect_all cvs);
    ignore(Canvas.create_rectangle cvs (Pixels x0) (Pixels y0) (Pixels x0) (Pixels y0) [Tags [tagdrag]]);
    Some (tagdrag,x0,y0) in
  let move init _ _ x y =
    match init with
    | Some (tagdrag,x0,y0) ->
	Canvas.delete cvs [tagdrag];
	let x1,y1 = conv_window_canvas cvs x y in
	ignore(Canvas.create_rectangle cvs (Pixels x0) (Pixels y0) (Pixels x1) (Pixels y1) [Tags [tagdrag]]);
    | None -> () in
  let release init x0 y0 x1 y1 =
    let x0,y0 = match init with
    | Some (tagdrag,x0,y0) -> Canvas.delete cvs [tagdrag];x0,y0
    | None -> 0,0 in
    let x1,y1 = conv_window_canvas cvs x1 y1 in
    let selectedids = Canvas.find cvs [Overlapping (x0,y0,x1,y1)] in
    let selectedtags = List.concat (List.map (function id -> Canvas.gettags cvs id) selectedids) in
    let selected_vertices = List.filter (function {v_tag=tag} -> List.mem (Tag tag) selectedtags) g.g_vertices in
    List.iter (function v -> select g (Vertice v)) selected_vertices;
    let selected_edges = List.filter (function {e_tag=tag} -> List.mem (Tag tag) selectedtags) g.g_edges in
    List.iter (function e -> select g (Edge e)) selected_edges in
  action_init cvs None true false [[],ButtonPressDetail 1] [[Button1],Motion] [[],ButtonReleaseDetail 1] (Some (init false)) (Some move) (Some release);
  action_init cvs None true false [[Shift],ButtonPressDetail 1] [[Button1],Motion] [[],ButtonReleaseDetail 1] (Some (init true)) (Some move) (Some release)

let select_canvas_remove g =
  bind g.g_canvas [[Shift],ButtonPressDetail 1] BindRemove;
  bind g.g_canvas [[],ButtonPressDetail 1] BindRemove

(* -- Refresh command ------------------------------------------------------ *)
let refresh g =
  (* Deletes all items from the canvas, and redraws vertices and edges *)
  let cvs = g.g_canvas in
  Canvas.delete cvs (Canvas.find cvs [All]);
  List.iter (vertex_draw g) g.g_vertices;
  List.iter (edge_draw g) g.g_edges

(* -- Vertice moving function ---------------------------------------------- *)
let move_init g tag =
  let cvs = g.g_canvas in
  match List.mem (tag_of_specialtags Movable) (Canvas.gettags cvs (Tag tag)) with
  | true ->
      let init _ _ =
	Canvas.raise_top cvs (Tag "Selected");
	let selectedids = Canvas.find cvs [Withtag (Tag "Selected")] in
	let selectedtags = union (List.map (function id -> Canvas.gettags cvs id) selectedids) in
	let selectedtags = List.filter (function t -> List.mem (tag_of_specialtags Movable) (Canvas.gettags cvs t)) selectedtags in
	let selected_vertices = List.filter (function {v_tag=tag} -> List.mem (Tag tag) selectedtags) g.g_vertices in
	List.iter (function {v_tag=tag} -> Canvas.addtag cvs (Tag "tomove") [Withtag (Tag tag)]) selected_vertices;
        (* edgesinin : edges that link two selected vertices *)
	let edgesininselect = List.filter (function {e_s=s; e_d=d} -> List.mem s.p_vertex selected_vertices && List.mem d.p_vertex selected_vertices) g.g_edges in
        (* edgesinoutselect : edges that link a selected vertex and an unselected vertex *)
	let edgesinoutselect = List.filter (function {e_s=s; e_d=d} -> (List.mem s.p_vertex selected_vertices)<>(List.mem d.p_vertex selected_vertices)) g.g_edges in
	List.iter (function {e_tag=tag} -> Canvas.addtag cvs (Tag "tomove") [Withtag (Tag tag)]) edgesininselect;
	let cx,cy = match (option_get g MoveDirection) with
	| "Both" -> 1,1
	| "Vertical" -> 0,1
	| "Horizontal" -> 1,0
	| _ -> 0,0 in
	Some (selected_vertices,edgesinoutselect,cx,cy) in
      let move init dx dy _ _ =
	let selected_vertices,edgesinoutselect,dx,dy = match init with
	| None -> [],[],0,0
	| Some (selected_vertices,edgesinoutselect,cx,cy) -> selected_vertices,edgesinoutselect,dx*cx,dy*cy in
	Canvas.move cvs (Tag "tomove") (Pixels dx) (Pixels dy);
	List.iter (function v ->
	  let c = change_coords v.v_pos dx dy in
	  v.v_pos <- c) selected_vertices;
          (* If the g.g_moveedges option is set, move edgesinoutselect among others items.
	     This can be quite slow if there are many edges to redraw *)
	(match option_get g MoveEdges with 
	| "1" -> List.iter (edge_redraw g) edgesinoutselect
	| _ -> ()) in
      let release init x0 y0 x1 y1 =
	let selected_vertices,edgesinoutselect,cx,cy = match init with
	| None -> [],[],0,0
	| Some (selected_vertices,edgesinoutselect,cx,cy) -> selected_vertices,edgesinoutselect,cx,cy in
	List.iter (function v -> v.v_move v.v_name (cx*(x1-x0),cy*(y1-y0))) selected_vertices;
	(match option_get g MoveEdges with
	| "0" -> List.iter (edge_redraw g) edgesinoutselect
	| _ -> ());
	Canvas.dtag cvs (Tag "tomove") (Tag "tomove");
	set_scrollregion cvs in
      action_init cvs (Some tag) false true [[],ButtonPressDetail 1] [[Button1],Motion] [[],ButtonReleaseDetail 1] (Some init) (Some move) (Some release)
  | false -> ()

(* -- Standard menu -------------------------------------------------------- *)
let rec update_menu_option g menu =
  let menu_o = Menu.create menu [TearOff false] in
  let f o label =
    match option_gettv g o with
    | Some tv -> Menu.add_checkbutton menu_o [Label label; Variable tv] ;
    | None -> () in
  f ShowBubble "Show Info Bubbles";
  f MoveEdges "Move Edges while Moving Vertices";
  Menu.add_cascade menu [Label "Options"; Menu menu_o]

and etype_menu_create g menu tv label f =
  let menu_e = Menu.create menu [TearOff false] in
  List.iter (function (lbl,t,_) ->
    Menu.add_radiobutton menu_e [Label lbl; Value lbl; Variable tv; Command (function _ -> f(lbl,t))]) g.g_etypelist;
  Menu.add_cascade menu [Label label; Menu menu_e]

and update_menu_edit g menu =
  Menu.add_command menu [Label "Copy"; Accelerator "Ctrl-C"; Command (function () -> selection_copy g)];
  bind g.g_bindwidget [[Control],KeyPressDetail "c"] (BindSet ([], function _ -> selection_copy g));
  Menu.add_command menu [Label "Cut"; Accelerator "Ctrl-X"; Command (function () -> selection_cut g)];
  bind g.g_bindwidget [[Control],KeyPressDetail "x"] (BindSet ([], function _ -> selection_cut g));
  Menu.add_command menu [Label "Paste"; Accelerator "Ctrl-V"; Command (function () -> paste g)];
  bind g.g_bindwidget [[Control],KeyPressDetail "v"] (BindSet ([], function _ -> paste g));
  Menu.add_command menu [Label "Delete"; Accelerator "Delete"; Command (function () -> selection_delete g)];
  bind g.g_bindwidget [[],KeyPressDetail "Delete"] (BindSet ([], function _ -> selection_delete g));
  Menu.add_separator menu;
  Menu.add_command menu [Label "Postscript File"; Command (function () -> postscript g)];  
  Menu.add_command menu [Label "Jpeg File"; Command (function () -> jpeg g)];  
  (match g.g_etypelist with 
  | a::b::_ ->
      Menu.add_separator menu;
      etype_menu_create g menu g.g_etypedefault "Default Edge Type" (function _ -> ())
  | _ -> ());

and update_menu_canvas g menu =
  clear_menu menu;
  update_menu_edit g menu;
  update_menu_option g menu;

and update_menu_vertex g menu =
  clear_menu menu;
  update_menu_edit g menu

and update_menu_edge g menu etype =
  clear_menu menu;
  (match g.g_etypelist with 
  | a::b::_ ->
      etype_menu_create g menu g.g_etypecurrent "Change Edge Type" (function (_,t) -> selection_changetype g t);
      Menu.add_separator menu;
  | _ -> ());
  update_menu_edit g menu

and update_menu_vertices g =
  List.iter (function (_,_,m) -> update_menu_vertex g m) g.g_vtypelist

and update_menu_edges g =
  List.iter (function (_,t,m) -> update_menu_edge g m t) g.g_etypelist

(* -- Vertice creation function -------------------------------------------- *)	 
(* Create the ports *)
and port_create v (pname,pdir,pconnect,authorized_edges) =
    {p_name = pname; 
     p_dir = pdir; 
     p_vertex = v;
     p_connect = pconnect;
     p_authorized_edges = authorized_edges;
     p_tag = tag_create ()}

and vertex_create g name vtype pos selected special_tags ports draw_f enter_f leave_f select_f move_f delete_f infostr_f refresh_ports_edges_f =
  (* Check whether the vertex already exists *)
  match exists_vertex g (name,vtype) with
  | false -> 	
      let cvs = g.g_canvas in
      let tag = tag_create () in
      (* Create the vertex *)
      let v = {
    	v_type = vtype;
    	v_name = name;
    	v_ports = [];
    	v_draw = draw_f;
	v_enter = enter_f;
	v_leave = leave_f;
	v_select = select_f;
    	v_move = move_f;
    	v_delete = delete_f;
	v_infostr = infostr_f;
	v_refresh_ports_edges = refresh_ports_edges_f;
    	v_tag = tag;
	v_special_tags = special_tags;
	v_pos = pos
      } in
      (* Add the ports to the vertex *)
      v.v_ports <- List.map (port_create v) ports;
      (* Add the vertex to the graph's vertices list *)
      g.g_vertices <- v::g.g_vertices;
      (* Draw the vertex  *)
      vertex_draw g v;
      (* Add the special tags *)
      List.iter (function t -> Canvas.addtag cvs (tag_of_specialtags t) [Withtag (Tag tag)]) special_tags;
      (* Do some bindings *)
      select_init g (Vertice v);
      (match selected with
      |	true -> select g (Vertice v)
      |	false -> ());
      move_init g tag;
      Canvas.bind cvs (Tag tag) [[Control],ButtonPressDetail 1] (BindSet ([],function _ -> ()));
      List.iter (drag_init g) v.v_ports;
      (* Zoom the vertex *)
      let zx,zy,factorx,factory = conv_zoom g.g_canvas g.g_scale_x "Both" in
      Canvas.scale g.g_canvas (Tag tag) (Pixels zx) (Pixels zy) factorx factory;
      Canvas.bind cvs (Tag tag) [[],Enter] (BindExtend ([Ev_MouseX;Ev_MouseY], function ev ->
    	select_canvas_remove g;
	v.v_enter (v.v_name,v.v_type);
    	Canvas.configure_rectangle cvs (Tag (tag^"Highlight")) [Outline highlight_color];
	(match option_get g ShowBubble with
    	| "1" -> bubble_draw cvs ev tag (infostr_f (vertexid_of_vertex v))
    	| _ -> ());
	change_menu cvs (menu_of_vtype g vtype)));
      Canvas.bind cvs (Tag tag) [[],Leave] (BindExtend ([],function _ ->
    	select_canvas_init g;
	v.v_leave (v.v_name,v.v_type);
    	Canvas.configure_rectangle cvs (Tag (tag^"Highlight")) [Outline Black];
	Canvas.delete cvs [Tag (tag^"Bubble")];
    	change_menu cvs g.g_menu_canvas))
  | true -> ()

(* -- Edge creation function ----------------------------------------------- *)
and edge_create g (v1_name,p1_name,v2_name,p2_name,etype) (special_tags,draw_f,enter_f,leave_f,select_f,changetype_f,delete_f,infostr_f) =
  (* Check wether the edge already exists *)
  match List.exists (function e -> e.e_s.p_vertex.v_name=v1_name && e.e_s.p_name=p1_name && e.e_d.p_vertex.v_name=v2_name && e.e_d.p_name=p2_name) g.g_edges with 
  | false ->
      let cvs = g.g_canvas in
      let p1 = find_pout g v1_name p1_name in
      let p2 = find_pin g v2_name p2_name in
      let tag = tag_create () in
      let e = 
    	{e_s = p1;
	 e_d = p2;
	 e_type = etype;
	 e_tag = tag;
	 e_draw = draw_f;
	 e_enter = enter_f;
	 e_leave = leave_f;
	 e_select = select_f;
	 e_changetype = changetype_f;
	 e_delete = delete_f;
	 e_infostr = infostr_f
       } in
      (* Draw the edge *)
      edge_draw g e;
      (* Add the special tags *)
      List.iter (function t -> Canvas.addtag cvs (tag_of_specialtags t) [Withtag (Tag tag)]) special_tags;
      (* Do some bindings *)
      Canvas.bind cvs (Tag tag) [[],Enter] (BindExtend ([Ev_MouseX;Ev_MouseY], function ev ->
    	select_canvas_remove g;
	e.e_enter (edgeid_of_edge e);
    	Canvas.configure_line cvs (Tag (tag^"Highlight")) [FillColor highlight_color];
	(match option_get g ShowBubble with
    	| "1" -> bubble_draw cvs ev tag (infostr_f (edgeid_of_edge e))
    	| _ -> ());
	Textvariable.set g.g_etypecurrent (texttype_of_etype g e.e_type);
    	change_menu cvs (menu_of_etype g e.e_type)));
      Canvas.bind cvs (Tag tag) [[],Leave] (BindExtend ([],function _ ->
    	Canvas.configure_line cvs (Tag (tag^"Highlight")) [FillColor Black];
    	select_canvas_init g;
	e.e_leave (edgeid_of_edge e);
	Canvas.delete cvs [Tag (tag^"Bubble")];
    	change_menu cvs g.g_menu_canvas));
      select_init g (Edge e);
      g.g_edges <- e::g.g_edges
  | true -> ()

and edge_create_from_etype g ((_,_,_,_,etype) as edge) =
  match g.g_newedge_info g etype with
  | Some parameters -> edge_create g edge parameters
  | None -> ()

and edge_create_from_stype g (v1_name,p1_name,v2_name,p2_name,stype) =
  let etype = etype_of_texttype g stype in
  edge_create_from_etype g (v1_name,p1_name,v2_name,p2_name,etype)

(* -- Edge creation by drag'n'drop ----------------------------------------- *)
and drag_init g p =
  let tag = p.p_tag in
  let cvs = canvas g in
  let init x0 y0 =
    let tagdrag = Tag (tag_create ()) in
    let sx1,sy1,sx2,sy2 = Canvas.bbox cvs [Tag (tag^"Cnx")] in
    let x0,y0 = (sx1+sx2)/2,(sy1+sy2)/2 in
    ignore(Canvas.create_line cvs [Pixels x0;Pixels y0;Pixels x0;Pixels y0] [Tags [tagdrag]; Width (Pixels 2)]);
    Some (tagdrag,x0,y0) in
  let move init _ _ x y =
    match init with
    | Some (tagdrag,x0,y0) ->
	Canvas.delete cvs [tagdrag];
	let x1,y1 = conv_window_canvas cvs x y in
	ignore(Canvas.create_line cvs [Pixels x0;Pixels y0;Pixels x1;Pixels y1] [Tags [tagdrag]; Width (Pixels 2)])
    | None -> () in
  let release init _ _ x1 y1 =
    (match init with
    | Some (tagdrag,_,_) -> Canvas.delete cvs [tagdrag]
    | None -> ());
    let x1,y1 = conv_window_canvas cvs x1 y1 in
    let tags2 = List.map string_of_tag (Canvas.gettags cvs (List.hd (Canvas.find cvs [Closest (Pixels x1, Pixels y1)]))) in 
    let s,d = match conv_tags_port g tags2 with
    | Some p2 ->
    	(match p.p_dir,p2.p_dir with
    	| POut, PIn -> Some p, Some p2
    	| PIn, POut -> Some p2, Some p
    	| PInOut, PInOut -> Some p, Some p2
    	| _ -> None,None)
    | None -> None,None in
    match s,d with
    | Some a, Some b ->
	let sid = portid_of_port a
	and did = portid_of_port b
	and etype = match etype_of_texttype g (Textvariable.get g.g_etypedefault) with
	| e when List.mem e p.p_authorized_edges -> e
	| _ -> List.hd p.p_authorized_edges in
	(try
	  a.p_connect (sid,did,etype);
	  edge_create_from_etype g (a.p_vertex.v_name,a.p_name,b.p_vertex.v_name,b.p_name,etype)
	with Failure _ -> ())
    | _ -> () in
  action_init cvs (Some tag) true false [[],ButtonPressDetail 2] [[Button2],Motion] [[],ButtonReleaseDetail 2] (Some init) (Some move) (Some release);
  action_init cvs (Some tag) true false [[Control],ButtonPressDetail 1] [[Button1],Motion] [[],ButtonReleaseDetail 1] (Some init) (Some move) (Some release)

(* -- Clipboard, copy, cut and paste stuff --------------------------------- *)
and create_clipboard () = ref ([],[])

and selected g =
  let (vertices,edges) = selection_get g in
  (List.map vertexid_of_vertex vertices),(List.map edgeid_of_edge edges)

and selection_copy g = 
  let verticesid,edgesid = selected g in
  (* First filter the selected edges that don't are between two selected vertices *)
  let edgesid = List.filter (function ((sname,_,_),(pname,_,_),_) -> 
    List.exists (function (name,_) -> name=sname) verticesid &&
    List.exists (function (name,_) -> name=pname) verticesid) edgesid in
  g.g_copy (verticesid,edgesid);
  g.g_clipboard := (verticesid,edgesid)

and selection_cut g = 
  match option_get g ReadOnly with
  | "0" -> selection_copy g; selection_delete g
  | _ -> ()
	
and paste g = 
  match option_get g ReadOnly with
  | "0" -> 
      unselect_all g.g_canvas;
      let (verticesid,edgesid) = !(g.g_clipboard) in
      let vertex_paste (vname,vtype) =
    	let newname = find_name vname (function n -> exists_vertex g (n,vtype)) in
	g.g_vertex_paste g (vname,vtype) newname;
	select g (Vertice (find_vertex g newname));
	(vname,newname) in
      let verticesmap = List.map vertex_paste verticesid in
      let edge_paste ((v1name,v1type,v1port),(v2name,v2type,v2port),etype) =
	let v1name = List.assoc v1name verticesmap in
	let v2name = List.assoc v2name verticesmap in
	g.g_edge_paste g ((v1name,v1type,v1port),(v2name,v2type,v2port),etype) in
      List.iter edge_paste edgesid
  | _ -> ()
	
and find_name name exists_function = 
  let rgxp = Str.regexp "\\(.*_\\)\\([0-9]+\\)$" in
  match (exists_function name) with
  | false -> name
  | true ->
      let try_name = try
	let _ = Str.search_forward rgxp name 0 in
	let number = 1+(int_of_string (Str.matched_group 2 name)) in
	(Str.matched_group 1 name)^(string_of_int number)
      with Not_found -> name^"_1" in
      find_name try_name exists_function

(* -- Vertice move --------------------------------------------------------- *)
let vertex_move g vname (dx,dy) =
  let v = find_vertex g vname in
  v.v_move v.v_name (dx,dy);
  v.v_pos <- change_coords v.v_pos dx dy;
  Canvas.move g.g_canvas (Tag v.v_tag) (Pixels dx) (Pixels dy);
  List.iter (function e -> match (List.mem e.e_s v.v_ports)||(List.mem e.e_d v.v_ports) with
  | true -> edge_redraw g e
  | false -> ()) g.g_edges 

(* -- Reset ---------------------------------------------------------------- *)
let reset g =
  g.g_vertices <- [];
  g.g_edges <- [];
  Canvas.delete g.g_canvas (Canvas.find g.g_canvas [All]);
  match g.g_canvas_draw with
  | Some f -> f g
  | None -> ()
	
(* -- Graph creation function ---------------------------------------------- *)
let create frm bindwidget canvas_draw_f enter_f leave_f edge_info_f clip copy_f vertex_paste_f edge_paste_f vtypelist etypelist etypedefault scroll_x_f scroll_y_f scale_f two_zooms directory additional_canvas =
  let cvs = Canvas.create frm [] in
  let xsb = Scrollbar.create frm [Orient Horizontal] in
  let ysb = Scrollbar.create frm [Orient Vertical] in
  let xscz = Scale.create frm [From 1.; To 100.; Orient Horizontal; Resolution 0.5; ShowValue false] in
  let yscz = Scale.create frm [From 1.; To 100.; Orient Vertical; Resolution 0.5; ShowValue false] in
    
  pack [xsb] [Side Side_Bottom; Fill Fill_X];
  pack [ysb] [Side Side_Right; Fill Fill_Y];
  pack [yscz] [Side Side_Right; Fill Fill_Y];
  (* This is not very clean: this is done for the particular case of the schedule view, in which the operator canvas must be packed that way *)
  (match two_zooms with
  | true ->   pack [xscz] [Side Side_Bottom; Fill Fill_X]
  | false -> ());
  (match additional_canvas with
  | None -> ()
  | Some add_cvs -> pack add_cvs [Side Side_Top]);
  pack [cvs] [Side Side_Right; Expand true; Fill Fill_Both];
  let g = {
    g_vertices = [];
    g_edges = [];
    g_canvas = cvs;
    g_canvas_draw = canvas_draw_f;
    g_enter = enter_f;
    g_leave = leave_f;
    g_menu_canvas = Menu.create cvs [TearOff false];
    g_newedge_info = edge_info_f;
    g_clipboard = clip;
    g_copy = copy_f;
    g_vertex_paste = vertex_paste_f;
    g_edge_paste = edge_paste_f;
    g_vtypelist = List.map (function (s,e) -> (s,e,(Menu.create cvs [TearOff false]))) vtypelist;
    g_etypelist = List.map (function (s,e) -> (s,e,(Menu.create cvs [TearOff false]))) etypelist;
    g_etypedefault = Textvariable.create ();
    g_etypecurrent = Textvariable.create ();
    g_bindwidget = bindwidget;
    g_view_scrollbar_x = xsb;
    g_scale_x_scrollbar = xscz;
    g_scale_y_scrollbar = yscz;
    g_scale_x = 1.0;
    g_scale_y = 1.0;
    g_scale_function = scale_f;
    (* Options *)
    g_options =  [];
    g_directory = directory
  } in

  Canvas.configure cvs [XScrollCommand (Scrollbar.set xsb)];
  Scrollbar.configure xsb [ScrollCommand (function sv -> Canvas.xview cvs sv;set_scrollregion cvs;scroll_x_f sv)];
  Canvas.configure cvs [YScrollCommand (Scrollbar.set ysb)];
  Scrollbar.configure ysb [ScrollCommand (function sv -> Canvas.yview cvs sv;set_scrollregion cvs;scroll_y_f sv)];
  Canvas.configure cvs [XScrollIncrement (Pixels 10); YScrollIncrement (Pixels 10)];
  Scale.configure yscz [ScaleCommand (function s -> let selection = selection_get g in
				      let zoomdir = match two_zooms with
					| true -> "Vertical"
					| false -> "Both" in
					unselect_all cvs; zoom g s zoomdir selection; set_scrollregion cvs)];
  Scale.set yscz g.g_scale_y;
  (match two_zooms with
  | true -> Scale.configure xscz [ScaleCommand (function s -> let selection = selection_get g in
						  unselect_all cvs; zoom g s "Horizontal" selection; set_scrollregion cvs)];
      Scale.set xscz g.g_scale_x;
  | false -> ());

  option_create g EnableSelection "1" false;
  option_create g MoveDirection "Both" false;
  option_create g ReadOnly "0" false;
  option_create g ShowBubble "0" true;
  option_create g MoveEdges "1" true;

  Textvariable.set g.g_etypedefault (texttype_of_etype g etypedefault);

  (* -- Fill the menus with some graph commands -- *)
  update_menu_canvas g g.g_menu_canvas;
  update_menu_vertices g;
  update_menu_edges g;
  (* -- Bindings -- *)
  change_menu cvs g.g_menu_canvas;
  select_canvas_init g; (* activate rectangle selection *)
  bind cvs [[],Enter] (BindExtend ([], function _ -> g.g_enter ()));
  bind cvs [[],Leave] (BindExtend ([], function _ -> g.g_leave ()));
  bind cvs [[], Configure] (BindSet ([],function _ -> set_scrollregion cvs)); (* Resize *)
  (* -- Reset -- *)
  reset g;
  (* Return the graph data structure *)
  g

(* -- Graph services ------------------------------------------------------ *)
let canvas_size graph (canvas_size_x,canvas_size_y) =
  Canvas.configure (canvas graph) [Width (Pixels canvas_size_x); Height (Pixels canvas_size_y)];
  update ()

let canvas_scale graph scale =
  graph.g_scale_x <- scale;
  graph.g_scale_y <- scale;
  Scale.set graph.g_scale_x_scrollbar scale;
  Scale.set graph.g_scale_y_scrollbar scale

let canvas_setscrollregion graph = set_scrollregion (canvas graph)

let configure_resize g resize = 
  bind g.g_canvas [[], Configure] (BindSet ([],function _ -> 
    set_scrollregion g.g_canvas;
    resize (Winfo.width g.g_canvas,Winfo.height g.g_canvas)))

let configure_scale graph min max res =
  Scale.configure graph.g_scale_x_scrollbar [From min; To max; Resolution res];
  Scale.configure graph.g_scale_y_scrollbar [From min; To max; Resolution res]

let tag_of_vertice graph (name,vtype) =
  (find_vertex graph name).v_tag

let tag_of_port graph (vname,vtype) (pname,dir) =
  (find_port graph vname pname dir).p_tag

let add_specialtag graph tagorid spctag =
  Canvas.addtag (canvas graph) (tag_of_specialtags spctag) [Withtag tagorid]

let vertex_refresh graph v =
  let cvs = canvas graph in
  Canvas.delete cvs [Tag v.v_tag];
  List.iter (port_remove graph) v.v_ports;
  let ports,edges = v.v_refresh_ports_edges (vertexid_of_vertex v) in
  v.v_ports <- List.map (port_create v) ports;
  vertex_draw graph v;
  List.iter (edge_create_from_etype graph) edges;
  List.iter (function t -> Canvas.addtag cvs (tag_of_specialtags t) [Withtag (Tag v.v_tag)]) v.v_special_tags

let vertex_refresh_graph graph (vname,vtype) =
  let v = find_vertex graph vname in
  vertex_refresh graph v

let vertex_remove_graph graph (name,vtype) =
  let v = find_vertex graph name in
  vertex_remove graph v

let port_remove_graph graph (vname,vtype) pname =
  let v = find_vertex graph vname in
  let ports = List.filter (function p -> p.p_name=pname) v.v_ports in
  let edges = List.filter (function {e_s=s;e_d=d} -> List.mem s ports || List.mem d ports) graph.g_edges in
  List.iter (edge_remove graph) edges;
  v.v_ports <- List.filter (function p -> p.p_name<>pname) v.v_ports;
  vertex_refresh graph v

let port_add_graph graph (vname,vtype) p =
  let v = find_vertex graph vname in
  v.v_ports <- (port_create v p)::v.v_ports;
  vertex_refresh graph v

let edge_remove_graph graph v1_name p1_name v2_name p2_name =
  let e = find_edge graph v1_name p1_name v2_name p2_name in
  edge_remove graph e

let unselect_all_graph graph =
  let cvs = canvas graph in
  unselect_all cvs

(* -- Functions used to get menus, from canvas,edges and vertices -- *) 
let get_menu_canvas g = g.g_menu_canvas
let get_menu_edge g etype = menu_of_etype g etype
let get_menu_vertex g vtype = menu_of_vtype g vtype

(* Returns the difference needed to fit a position on the grid *)
let diff_fit_pos (x,y) =
  let rint x = int_of_float (floor (x+.0.5)) in
  let quantize x q =  q * rint ((float_of_int x)/.(float_of_int q)) in
  ((quantize x grid_size)-x, (quantize y grid_size)-y)

(* Fits the positions of the elements of this algo on a grid *)
let graph_fit g =
  List.iter (function v -> vertex_move g v.v_name (diff_fit_pos v.v_pos)) g.g_vertices

let place_column g vertices xpos (ox,oy) step_size =
  let offset (oldx,oldy) (newx,newy) =
    (newx-oldx+ox,newy-oldy+oy) in
  ignore(List.fold_left (function ypos -> function v ->
    vertex_move g v.v_name (offset v.v_pos (xpos,ypos));
    ypos + step_size) 0 vertices)

(* Computes positions for oriented graph vertices depending on the edges between them *)
let place_oriented g pred origin (step_size_x,step_size_y) =
  let is_init v = (pred v = []) in
  let (no_predecessor,rest) = List.partition (function v -> is_init v) g.g_vertices in
  (* Is candidate if all predecessors have been placed *)
  let is_candidate v rest = (intersection (pred v) rest)=[] in
  let rec place_all (level,rest) xpos =
    match level with
    | [] -> ()
    | _ ->
	place_column g level xpos origin step_size_y;
	place_all (List.partition (function v -> is_candidate v rest) rest) (xpos + step_size_x) in
  place_all (no_predecessor,rest) 0

(* Computes positions for non-oriented graph vertices. *)
(* Does a basic partition between different vertices groups.*)
let place_non_oriented g origin (step_size_x,step_size_y) =
  let rec place_all toprocess xpos = match toprocess with
  | [] -> ()
  | {v_type=hdtype}::_ ->
      let level,others = List.partition (function {v_type=vtype} -> vtype=hdtype) toprocess in
      place_column g level xpos origin step_size_y;
      place_all others (xpos+step_size_x) in

  place_all g.g_vertices 0


