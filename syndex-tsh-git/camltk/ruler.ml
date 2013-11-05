(** This module provides horizontal or vertical rulers. Rulers are
  usually used to show the mouse pointer's location in a window. For
  the moment it only provides a graduation bar for the window. *)

open Camltk;;

type tick_look = {
  length: int;
  show_number: bool
}

type metric = {
  ticks_font: string;
  ticks_looks: tick_look array
}
    
type t = {
  canvas: Widget.widget;
  mutable lower: float;
  mutable upper: float;
  mutable max_size: int;
  mutable metric: metric;
  mutable orientation: Tk.orientation
}
    
(* The default metric assigned when creating a ruler. *)
let default_metric canvas orientation = {
  ticks_font = "Helvetica -8";
  ticks_looks =
   let cvs_small_dim = match orientation with
     | Vertical -> Winfo.reqwidth canvas
     | Horizontal -> Winfo.reqheight canvas in
   let look1 = {length = cvs_small_dim; show_number = true} in
   let look2 = {length = cvs_small_dim / 2; show_number = false} in
   let look3 = {length = cvs_small_dim / 4; show_number = false} in
     [| look1;
	look3; look2;
	look3; look2;
	look3; look2;
	look3; look2;
	look3 |]
}

let create parent ~width ~height orientation =
  let cvs =
    Canvas.create parent [Width (Pixels width); Height (Pixels height)] in
  let ruler = {
    canvas = cvs;
    lower = 0.;
    upper = 1.;
    max_size =
   (match orientation with
      | Horizontal -> width
      | Vertical -> height);
    metric = default_metric cvs orientation;
    orientation = orientation
  } in
    pack [ruler.canvas] [];
    ruler
      
let create_metric ~font ~ticks_looks =
  { ticks_font = font;
    ticks_looks = Array.of_list
		    (List.map
		       (fun (length,show) -> {length=length;show_number=show})
		       ticks_looks)
  }
  
let get_metric ruler =
  ruler.metric
    
let set_metric ruler metric =
  ruler.metric <- metric
    
let get_orientation ruler =
  ruler.orientation
    
let set_orientation ruler orientation =
  ruler.orientation <- orientation

let get_range_lower ruler =
  ruler.lower
    
let get_range_upper ruler =
  ruler.upper
    
let set_range ruler ~lower ~upper ~max_size =
  ruler.lower <- lower;
  ruler.upper <- upper;
  ruler.max_size <- max_size

let get_range ruler =
  (ruler.lower, ruler.upper, ruler.max_size)
  
(* Returns the geometry to use to draw the ticks. This consists in
  the number of pixels between each tick, the number of ticks to
  draw, and the difference of value between each tick. *)
let ticks_geometry ruler =
  let range = ruler.upper -. ruler.lower in
  let base10 = (int_of_float (log10 range)) + 1 in
  let boundary10 = 10. ** (float_of_int base10) in
  let tick_range = match range < (boundary10 /. 4.) with
    | true -> boundary10 /. 400.
    | false -> match range < (boundary10 /. 2.) with
	| true -> boundary10 /. 200.
	| false -> boundary10 /. 100. in
  let ticks_number = int_of_float (range /.tick_range) in
  let tick_size =
    (float_of_int ruler.max_size) /. (float_of_int ticks_number) in
    tick_size, ticks_number, tick_range

let draw_ticks ruler =
  let cvs_small_dim = match ruler.orientation with
    | Horizontal -> Winfo.reqheight ruler.canvas
    | Vertical -> Winfo.reqwidth ruler.canvas in
  let tick_size, ticks_number, tick_range = ticks_geometry ruler in
  let rec aux tick_index =
    if tick_index <= ticks_number then
      let t_look =
	(Array.get ruler.metric.ticks_looks
	   (tick_index mod (Array.length ruler.metric.ticks_looks))) in
	(* Get coordinates *)
      let x0,y0,x1,y1 =
	match ruler.orientation with 
	  | Horizontal ->
	      ((int_of_float (tick_size *. (float_of_int tick_index))) + 1,
	       (cvs_small_dim - t_look.length),
	       ((int_of_float (tick_size *. (float_of_int tick_index))) + 1),
	       cvs_small_dim)
	  | Vertical ->
	      ((cvs_small_dim - t_look.length),
	       ((int_of_float (tick_size *. (float_of_int tick_index))) + 1),
	       cvs_small_dim,
	       ((int_of_float (tick_size *. (float_of_int tick_index))) + 1)) in
	(* Draw tick line *)
	ignore(Canvas.create_line ruler.canvas
		 [Pixels x0; Pixels y0; Pixels x1; Pixels y1] []);
	(* Draw tick text *)
	(if t_look.show_number then
	   let tick_value =
	     tick_range *. (float_of_int tick_index) +. ruler.lower in
	   let tick_text =
	     if (float_of_int (int_of_float tick_value)) = tick_value then
	       string_of_int (int_of_float tick_value)
	     else
	       string_of_float tick_value in
	   let font = ruler.metric.ticks_font in
	     (match ruler.orientation with
		| Horizontal ->
		    ignore(Canvas.create_text ruler.canvas
			     (Pixels (x0 + 2)) (Pixels 0)
			     [Anchor NW; Text tick_text; Font font])
		| Vertical ->
		    let car_max_width = Font.measure font "0" in
		      ignore(Canvas.create_text ruler.canvas
			       (Pixels 0) (Pixels (y0 + 2))
			       [Anchor NW; Text tick_text; Font font;
				(* No orient Vertical available, this Width
				   option will cut each line to a single caracter *)
				Width (Pixels car_max_width)])));
	aux (tick_index + 1) in
    aux 0
