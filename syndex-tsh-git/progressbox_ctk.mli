open Camltk

type progress

val close : progress -> unit
val tick : progress -> int -> unit
val create : string -> (string * int * int) list -> progress
