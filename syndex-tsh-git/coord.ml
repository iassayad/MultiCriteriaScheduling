(** 2D coordinates of objects of graphs *)
type coord2d = Coord2d of int*int | No_coord2d

let pos_of_coord2d pos =
  match pos with
  | Coord2d (x,y) -> x,y
  | No_coord2d -> (0,0)

