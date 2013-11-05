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

let border = 2

type parameter = {
    values : Widget.widget;
    bar : Widget.widget;
    progress : Widget.widget;
    total_ticks : int;
    step : int;
    mutable current_ticks : int; 
  }

type progress = {
    window : Widget.widget;
    parameters : parameter list;
    time_start : float;
  }

let hms_of_s t =
  let norm s = if String.length s=1 then "0"^s else s in
  let s = string_of_int (t mod 60) in
  let m = t / 60 in
  let h = string_of_int (m / 60) in
  let m = string_of_int (m mod 60) in
  h^":"^(norm m)^":"^(norm s)

let close p = destroy p.window

let tick p number =
  let parameter = List.nth p.parameters (number-1) in
  parameter.current_ticks <- parameter.current_ticks + 1;
  (match (parameter.current_ticks mod parameter.step=0) || (parameter.current_ticks=parameter.total_ticks) with
  | true ->
      let percent = (float_of_int parameter.current_ticks)/.(float_of_int parameter.total_ticks) in
      let percent_txt = (string_of_int (int_of_float (100.*.percent)))^"% ("^(string_of_int parameter.current_ticks)^"/"^(string_of_int parameter.total_ticks)^")" in
      let time_txt = match number=1 with
      | true ->
	  let elapsed = Unix.gettimeofday() -. p.time_start in
	  let remaining = elapsed/.(float_of_int parameter.current_ticks)*.(float_of_int (parameter.total_ticks-parameter.current_ticks)) in
	  let elapsed_txt = hms_of_s (int_of_float elapsed)
	  and remaining_txt = hms_of_s (int_of_float (remaining +. 1.)) in
	  "\n"^elapsed_txt^"\n"^remaining_txt
      | false -> "" in
      Label.configure parameter.values [Text (percent_txt^time_txt)];
      let w = float_of_int (Winfo.width parameter.bar) in
      Frame.configure parameter.progress [Width (Pixels (int_of_float (w*.percent)))];
      update ()
  | false -> ());
  match (parameter.current_ticks=parameter.total_ticks) && (number=1) with
  | true -> close p
  | false -> ()

let create title parameters = 
  (* Create the widgets *)
  let win = Toplevel.create Widget.default_toplevel [] in
  Wm.title_set win title;

  let rec parameters_create tocreate created = match tocreate with
  | [] -> created
  | (text,total,step)::tl ->
      let msg = Label.create win [Text text] in
      let values_label_text = "Done :"^(match created=[] with
      |	true -> "\nElapsed Time :\nRemaining Time :"
      |	false -> "") in
      let values_text = "0% ("^"0/"^(string_of_int total)^")"^(match created=[] with
      |	true -> "\n0:00:00\n"
      |	false -> "") in
      let frame = Frame.create win [] in
      let values_label = Label.create frame [Justify Justify_Left;Text values_label_text] in
      let values = Label.create frame [Justify Justify_Right; Text values_text] in
      let bar = Frame.create win [Height (Pixels 20); Relief Sunken; BorderWidth (Pixels border)] in
      let progress = Frame.create bar [Height (Pixels 20); Background (NamedColor "RoyalBlue3")] in
      pack [msg] [Fill Fill_X;Side Side_Top];
      pack [values_label] [Side Side_Left];
      pack [values] [Side Side_Right];
      pack [frame] [Side Side_Top;Fill Fill_X];
      pack [progress] [Side Side_Left;Fill Fill_X];
      pack [bar] [Side Side_Top;Fill Fill_X];
      let param = {
	values = values;
	bar = bar;
	progress = progress;
	total_ticks = total;
	step = step;
	current_ticks = 0} in
      parameters_create tl (created@[param]) in
  
  (* Refresh the display *)
  let progress = {
   window = win;
   parameters = parameters_create parameters [];
   time_start = Unix.gettimeofday ();
  } in
  update();
  let geom = Wm.geometry_get win in
  let r = Str.regexp "\\([^x]*\\)x\\([^\\+]*\\)\\+" in
  let width,height = try
    ignore(Str.search_forward r geom 0);
    (int_of_string (Str.matched_group 1 geom))+50,int_of_string (Str.matched_group 2 geom)
  with _ -> 100,100 in
  Wm.geometry_set win (Format.sprintf "%ix%i" width height);
  Wm.resizable_set win false false;
  update();
  progress
