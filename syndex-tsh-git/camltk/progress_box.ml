(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                            Matthieu Rouget                            *)
(*                          Christophe Macabiau                          *)
(*                       Revisited by Julien Forget                      *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

open Camltk

(** A progress box is used to display the progress of a long process. *)
module type PROGRESS_BOX_SIG =
  sig
    type t
	  
    val create : int -> t
    (** [create total] returns a new progress box, which progress will be complete
       after [total] steps. *)
	
    val tick : t -> unit
    (** Signals the progress box that the process has gone one step further. *)
	
    val close : t -> unit
    (** Closes the progress box. *)
  end

(** An implementation of the progress box for Tk *)
module Tk : PROGRESS_BOX_SIG =
  struct
    type t = {
	total_ticks : int;
	ticks_per_step : int;
	mutable current_ticks : int;
	window : Widget.widget;
	values_label : Widget.widget;
	drawing_bar : Widget.widget;
	drawing_width : float;
	start_time : float
      }

    let create total = 
      let win = Toplevel.create Widget.default_toplevel [] in
      Wm.title_set win "SynDEx";
      let title_msg = Label.create win [Text "Adequation"] in
      let rows_text = "Done :\nElapsed Time :\nRemaining Time :" in
      let rows_values = "0% ("^"0/"^(string_of_int total)^")\n0:00:00\n" in
      let text_frame = Frame.create win [] in
      let rows_text_label =
	Label.create text_frame
	  [Justify Justify_Left;Text rows_text] in
      let rows_values_label =
	Label.create text_frame
	  [Justify Justify_Right; Text rows_values] in
      let drawing_frame =
	Frame.create win
	  [Height (Pixels 20); Relief Sunken; BorderWidth (Pixels 2)] in
      let drawing_bar =
	Frame.create drawing_frame
	  [Height (Pixels 20); Background (NamedColor "RoyalBlue3")] in
      pack [title_msg] [Fill Fill_X;Side Side_Top];
      pack [rows_text_label] [Side Side_Left];
      pack [rows_values_label] [Side Side_Right];
      pack [text_frame] [Side Side_Top;Fill Fill_X];
      pack [drawing_bar] [Side Side_Left;Fill Fill_X];
      pack [drawing_frame] [Side Side_Top;Fill Fill_X];
      update();

      let geom = Wm.geometry_get win in
      let r = Str.regexp "\\([^x]*\\)x\\([^\\+]*\\)\\+" in
      let width,height = try
	ignore(Str.search_forward r geom 0);
	(int_of_string (Str.matched_group 1 geom))+50,
	int_of_string (Str.matched_group 2 geom)
      with _ -> 100,100 in
      Wm.geometry_set win (Format.sprintf "%ix%i" width height);
      Wm.resizable_set win false false;
      update();
      let width = float_of_int (Winfo.width drawing_frame) in
      let current_time = Unix.gettimeofday () in

      let box =
	{
	 total_ticks = total;
	 ticks_per_step =
	 (match total<100 with
	 | true -> 1
	 | false -> total/100);
	 current_ticks = 0;
	 window = win;
	 values_label = rows_values_label;
	 drawing_bar = drawing_bar;
	 drawing_width = width;
	 start_time = current_time;
       } in
      box

    let close box = destroy box.window

    let hms_of_s t =
      let norm s = if String.length s=1 then "0"^s else s in
      let s = string_of_int (t mod 60) in
      let m = t / 60 in
      let h = string_of_int (m / 60) in
      let m = string_of_int (m mod 60) in
      h^":"^(norm m)^":"^(norm s)

    let tick box =
      box.current_ticks <- box.current_ticks + 1;
      (match (box.current_ticks mod box.ticks_per_step = 0) ||
      (box.current_ticks = box.total_ticks) with
      | true ->
	  let percent =
	    (float_of_int box.current_ticks) /. (float_of_int box.total_ticks) in
	  let percent_txt =
	    (string_of_int (int_of_float (100.*.percent)))^"% ("^
	    (string_of_int box.current_ticks)^"/"^
	    (string_of_int box.total_ticks)^")" in
	  let time_txt =
	    let elapsed = Unix.gettimeofday() -. box.start_time in
	    let remaining =
	      elapsed /. (float_of_int box.current_ticks) *. 
		(float_of_int (box.total_ticks - box.current_ticks)) in
	    let elapsed_txt = hms_of_s (int_of_float elapsed)
	    and remaining_txt = hms_of_s (int_of_float (remaining +. 1.)) in
	    "\n"^elapsed_txt^"\n"^remaining_txt in
	  Label.configure box.values_label [Text (percent_txt^time_txt)];
	  Frame.configure box.drawing_bar
	    [Width (Pixels (int_of_float (box.drawing_width *. percent)))];
	  update ()
      | false -> ());
      match (box.current_ticks=box.total_ticks) with
      | true -> close box
      | false -> ()
  end

(** A dummy implementation of the progress box. It actually does nothing. *)
module Dummy : PROGRESS_BOX_SIG =
  struct
    type t = unit
    let create total = ()
    let tick _ = ()
    let close _ = ()
  end
