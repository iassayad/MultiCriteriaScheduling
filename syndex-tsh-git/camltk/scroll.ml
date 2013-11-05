open Camltk

let create_scrollbar ~parent ~canvas orientation =
  let sb = Scrollbar.create parent [Orient orientation] in
  let opt, cmd = match orientation with
    Horizontal ->
      XScrollCommand (Scrollbar.set sb), fun sv -> Canvas.xview canvas sv
  | Vertical ->
      YScrollCommand (Scrollbar.set sb), fun sv -> Canvas.yview canvas sv
  in
  Canvas.configure canvas [opt];
  Scrollbar.configure sb [ScrollCommand cmd];
  sb

let get_scrollregion canvas =
  let _, _, x1, y1 =
    match Canvas.find canvas [All] with
      (* Returns a non-empty region to make ScrollRegion happy *)
      [] -> 0, 0, 1, 1
    | l -> Canvas.bbox canvas l in
  0, 0, x1, y1

let update_region canvas =
  let x0, y0, x1, y1 = get_scrollregion canvas in
  Canvas.configure canvas [ScrollRegion (Pixels x0, Pixels y0,
                                         Pixels x1, Pixels y1)]
let center_on_position canvas (x, y) =
  let x0, y0, x1, y1 = get_scrollregion canvas in
  let x_ratio = (float_of_int x) /. (float_of_int (x1 - x0))
  and y_ratio = (float_of_int y) /. (float_of_int (y1 - y0)) in
  let min, max = Canvas.xview_get canvas in
  Canvas.xview canvas (MoveTo (x_ratio -. ((max -. min) /. 2.)));
  let min, max = Canvas.yview_get canvas in
  Canvas.yview canvas (MoveTo (y_ratio -. ((max -. min) /. 2.)))

let transform_point canvas (x, y) =
  let width, height = Winfo.width canvas, Winfo.height canvas in
  let xa, xb = Canvas.xview_get canvas
  and ya, yb = Canvas.yview_get canvas in
  x + int_of_float ((float_of_int width) *. xa /. (xb -. xa)),
  y + int_of_float ((float_of_int height) *. ya /. (yb -. ya))
