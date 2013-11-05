(** Port operations. *)

type port
(** The type of a port. *)

type direction =
    In
  | Out
(*  | InOut *)

(** Port Directions. *)

type port_class =
    Data_Port
  | Precedence_Port
  | Init_Memory_Port
  | Delay_Buffer
  | Delay_Port

(** Port Classes. *)

val create : name:string -> dir:direction ->
  typename:string -> dim:Symbolic.expression ->
    port_class:port_class -> order:int -> pos:Coord.coord2d -> port
(** Creates a new port. *)

val port_name : port -> string
val set_port_name : port -> string -> unit

val port_direction : port -> direction
val set_port_direction : port -> direction -> unit

val port_type : port -> string * Symbolic.expression
val set_port_type : port -> typename:string -> dim:Symbolic.expression -> unit

val port_class : port -> port_class
val set_port_class : port -> port_class -> unit

val port_order : port -> int
val set_port_order : port -> int -> unit

val port_position : port -> Coord.coord2d
val set_port_position : port -> Coord.coord2d -> unit

val is_precedence_port : port -> bool
(** Returns true if port is a precedence port. *)

val find_port : name:string -> direction -> port list -> port
(** Returns the port of name name and direction dir in the list of port. *)

val partition_ports : port list -> port list * port list
(** Partition a list of ports in three sublists according to
   port direction.  *)

val ports_order_sort : port list -> port list
(** Returns ports partitionned in input ports and output ports, sorted
   according to prt_order. After sorting, ports are renumerated from 0, in
   case there are gaps in the numeration. *)
