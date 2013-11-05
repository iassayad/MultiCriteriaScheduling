open Camltk

type item =
    Box of string (* XXX *)
  | Port of Port.port
  | Line of string

type graph = { canvas: Widget.widget;
               mutable ids: (item, string) Hashtbl.t;
               mutable items: (tagOrId, item) Hashtbl.t;
               mutable selection: item list;
               gensym_counter: int ref }

(** Enable vertical scrolling in [canvas] using mouse wheel. *)
let bind_mouse_wheel canvas =
  bind canvas [[], ButtonPressDetail 4]
    (BindSet ([], function _ -> Canvas.yview canvas (ScrollUnit (-1))));
  bind canvas [[], ButtonPressDetail 5]
    (BindSet ([], function _ -> Canvas.yview canvas (ScrollUnit 1)))

let create ?(options=[]) parent ~width ~height =
  let cvs = Canvas.create parent
      ([Width width; Height height] @ options) in
  let xsb = Scroll.create_scrollbar ~parent:parent ~canvas:cvs Horizontal in
  let ysb = Scroll.create_scrollbar ~parent:parent ~canvas:cvs Vertical in
  pack [xsb] [Side Side_Bottom; Fill Fill_X];
  pack [ysb] [Side Side_Right; Fill Fill_Y];
  pack [cvs] [Expand true; Fill Fill_Both];
  Canvas.configure cvs [XScrollIncrement (Pixels 10);
                        YScrollIncrement (Pixels 10)];
  bind_mouse_wheel cvs;
  { canvas = cvs;
    ids = Hashtbl.create 100;
    items = Hashtbl.create 100;
    selection = [];
    gensym_counter = ref 0 }

let get_items graph =
  let items = ref [] in
  Hashtbl.iter (fun _ item -> items := item :: !items) graph.items;
  !items

let add_item graph item id =
  Hashtbl.add graph.ids item id;
  Hashtbl.add graph.items (Tag id) item

let find_id graph item =
  Hashtbl.find graph.ids item

let find_item graph id =
  Hashtbl.find graph.items id

(** Item Selection *)

(** Highlight the ... *)
let highlight_selected graph item =
  let id = (find_id graph item) in
  let x0, y0, x1, y1 = Canvas.bbox graph.canvas [Tag id] in
  let x_list, y_list = match item with
      Port _ | Box _ -> [x0; x1; x1; x0], [y0; y0; y1; y1]
    | Line _ -> [x0; (x1 - x0) / 2; x1], [y0; (y1 - y0) / 2; y1] in
  List.iter2
    (fun x y ->
      ignore (Canvas.create_rectangle graph.canvas
                (Pixels (x - 3)) (Pixels (y - 3))
                (Pixels (x + 3)) (Pixels (y + 3))
                [FillColor Red; Tags [Tag id; Tag (id ^ "Handle")]]))
    x_list y_list

let unhighlight_selected graph item =
  let tag = (find_id graph item) in
  Canvas.delete graph.canvas [Tag (tag ^ "Handle")]

let get_selection graph =
  graph.selection

let selected graph item =
  List.mem item graph.selection

let add_to_selection graph item =
  if not (selected graph item)
  then begin
    graph.selection <- item :: graph.selection;
    highlight_selected graph item
  end

let remove_from_selection graph item =
  if (selected graph item)
  then begin
    graph.selection <- List.filter (fun i -> i <> item) graph.selection;
    unhighlight_selected graph item
  end

let clear_selection graph =
  List.iter
    (fun item -> remove_from_selection graph item)
    graph.selection

let set_selection ?(clear=true) graph items =
  if clear then clear_selection graph;
  List.iter (fun item -> add_to_selection graph item) items

let toggle_selected graph item =
  if (selected graph item)
  then remove_from_selection graph item
  else add_to_selection graph item

let select_all graph =
  set_selection graph (get_items graph)

(** Generate a unique string within [graph], prefixed by [prefix]. *)
let gensym ?(prefix="") graph =
  incr graph.gensym_counter;
  prefix ^ "_" ^ string_of_int !(graph.gensym_counter)

(** sjkls *)
let item_center_position graph item =
  let x0, y0, x1, y1 = Canvas.bbox graph.canvas [Tag (find_id graph item)] in
  (x0 + x1) / 2, (y0 + y1) / 2

let center_on_item graph item =
  Scroll.center_on_position graph.canvas
    (item_center_position graph item)

(**  *)
let bind_highlight ?(fill=false) canvas tag highlight_ids =
  let highlight id color =
    match Canvas.typeof canvas id with
      Line_item -> Canvas.configure_line canvas id [FillColor color]
    | _ -> Canvas.configure_rectangle canvas id [Outline color];
        if fill then Canvas.configure_rectangle canvas id [FillColor color]
  in
  Canvas.bind canvas tag [[], Enter]
    (BindExtend ([], function _ ->
      List.iter (fun id -> highlight id Red) highlight_ids));
  Canvas.bind canvas tag [[], Leave]
    (BindExtend ([], function _ ->
      List.iter (fun id -> highlight id Black) highlight_ids))

let bind_selection graph id item =
  let cvs = graph.canvas in
  Canvas.bind cvs id [[], ButtonPressDetail 1]
    (BindSet ([], function _ -> set_selection graph [item]));
  Canvas.bind cvs id [[Shift], ButtonPressDetail 1]
    (BindSet ([], function _ -> toggle_selected graph item));
  let selection_tag = Tag "selection" in
  let x0, y0 = ref 0, ref 0 in
  let init_selection_rectangle canvas x y =
    let x, y = Scroll.transform_point canvas (x, y) in
    x0 := x; y0:= y;
    ignore(Canvas.create_rectangle canvas
             (Pixels x) (Pixels y) (Pixels x) (Pixels y)
             [Outline Black; Dash "."; Tags [selection_tag]])
  and drag_selection_rectangle canvas mx my =
    let x, y = (* if scroll = true *)
      let wc, hc = Winfo.width canvas, Winfo.height canvas in
      let dx, xl = if mx < 0 then -2, 0 else if mx > wc then 2, wc else 0, mx
      and dy, yl = if my < 0 then -2, 0 else if my > hc then 2, hc else 0, my in
      Canvas.xview cvs (ScrollUnit dx); Canvas.yview cvs (ScrollUnit dy);
      xl, yl in
    let x, y = Scroll.transform_point canvas (x, y) in
    Canvas.coords_set canvas selection_tag
      [Pixels !x0; Pixels !y0; Pixels x; Pixels y]
   and end_selection_rectangle canvas ~clear =
    let x0, y0, x1, y1 = Canvas.bbox canvas [selection_tag] in
    let items = ref [] in
    List.iter
      (function tag ->
        try items := (find_item graph tag) :: !items with
          Not_found -> ())
      (List.concat (List.map
                      (function id -> Canvas.gettags canvas id)
                      (Canvas.find canvas [Overlapping (x0, y0, x1, y1)])));
    set_selection graph !items ~clear:clear;
    Canvas.delete canvas [selection_tag]
  in
  bind cvs [[], ButtonPressDetail 1]
    (BindSet ([Ev_MouseX; Ev_MouseY], function ev ->
      init_selection_rectangle cvs ev.ev_MouseX ev.ev_MouseY));
  bind cvs [[Button1], Motion]
    (BindSet ([Ev_MouseX; Ev_MouseY], function ev ->
      drag_selection_rectangle cvs ev.ev_MouseX ev.ev_MouseY));
  bind cvs [[], ButtonReleaseDetail 1]
    (BindSet ([], function _ -> end_selection_rectangle cvs ~clear:true));
  bind cvs [[Shift], ButtonReleaseDetail 1]
    (BindSet ([], function _ -> end_selection_rectangle cvs ~clear:false))

let text_width string ~font =
  List.fold_left max 0
    (List.map (fun s -> Font.measure font s)
       (Str.split (Str.regexp "\n") string))

let text_height string ~font =
  List.fold_left (+) 0
    (List.map (fun s -> Font.metrics font Linespace)
       (Str.split (Str.regexp "\n") string))

let draw_handler canvas x y =
  let id = Canvas.create_rectangle canvas
      (Pixels x) (Pixels (y - 3)) (Pixels (x + 2)) (Pixels (y + 3))
      [Outline Black; FillColor Black] in
  bind_highlight canvas id [id] ~fill:true

let default_font = "Helvetica -12" (* XXX *)

let present_port ?(length=0) ?(font=default_font) graph port x y =
  let id = gensym graph ~prefix:"port" in
  let cvs = graph.canvas in
  let name, direction = (Port.port_name port), (Port.port_direction port) in
  let height = (text_height name ~font:font) + 6 in
  let width = max length ((text_width name ~font:font) + 6) in
  let textx, anchor =
    match direction with
      Port.In -> x + 3, NW
    | Port.Out -> x + width - 3, NE
    (* | Port.InOut -> x + (width / 2), N *) in
  let rect_id = Canvas.create_rectangle cvs
      (Pixels x) (Pixels y) (Pixels (x + width)) (Pixels (y + height))
    [FillColor White; Tags [Tag id]] in
  ignore(Canvas.create_text cvs (Pixels textx) (Pixels (y + 3))
           [Text name; Anchor anchor; Font font; Tags [Tag id]]);
  bind_highlight cvs (Tag id) [rect_id] ~fill:false;
  let m = y + (height / 2) in
  if (direction = Port.In (* || direction = Port.InOut *))
  then draw_handler cvs (x - 2) m;
  if (direction = Port.Out (* || direction = Port.InOut *))
  then draw_handler cvs (x + width) m;
  let menu = Menu.create cvs [TearOff false] in
  Menu.add_command menu [Label ("Port " ^ name)];
  Canvas.bind cvs (Tag id) [[], ButtonPressDetail 3]
    (BindSet ([Ev_RootX; Ev_RootY],
              function ev -> Menu.popup menu ev.ev_RootX ev.ev_RootY));
  bind_selection graph (Tag id) (Port port);
  id

let present_box graph box x y =
  let id = gensym graph ~prefix:"box" in
  id

let present graph item ~x ~y =
  let id = match item with
    Box box -> present_box graph box x y
  | Port port -> present_port graph port x y
  | Line _ -> "" in
  Scroll.update_region graph.canvas;
  add_item graph item id
