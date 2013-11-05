type coord2d = Coord2d of int * int | No_coord2d

val pos_of_coord2d : coord2d -> int * int
