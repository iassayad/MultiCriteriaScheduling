type direction =
    In
  | Out

type port_class =
    Data_Port
  | Precedence_Port
  | Init_Memory_Port
  | Delay_Buffer
  | Delay_Port

type port = { mutable prt_name: string;
              mutable prt_direction: direction;
              mutable prt_typename: string;
              mutable prt_dim: Symbolic.expression;
              mutable prt_class: port_class;
              mutable prt_order: int;               (* Drawing order *)
              mutable prt_position: Coord.coord2d
            }

let create ~name ~dir ~typename ~dim ~port_class ~order ~pos =
  { prt_name=name;
    prt_direction=dir;
    prt_typename=typename;
    prt_dim=dim;
    prt_class=port_class;
    prt_order=order;
    prt_position=pos }

let port_name port =
  port.prt_name
let set_port_name port name =
  port.prt_name <- name

let port_direction port =
  port.prt_direction
let set_port_direction port dir =
  port.prt_direction <- dir

let port_type port =
  port.prt_typename, port.prt_dim
let set_port_type port ~typename ~dim =
  port.prt_typename <- typename;
  port.prt_dim <- dim

let port_class port =
  port.prt_class
let set_port_class port port_class =
  port.prt_class <- port_class

let port_order port =
  port.prt_order
let set_port_order port order =
  port.prt_order <- order

let port_position port =
  port.prt_position
let set_port_position port pos =
  port.prt_position <- pos

let is_precedence_port port = (port.prt_class = Precedence_Port)

let find_port ~name dir ports =
  List.find (function p -> p.prt_name = name && p.prt_direction = dir) ports

let partition_ports ports =
  let ins = ref [] and inouts = ref [] and outs = ref [] in
  List.iter (function port ->
    let dest = match port.prt_direction with
      In -> ins
    | Out -> outs in
    dest := port :: !dest) ports;
  List.rev !ins, List.rev !outs

let ports_order_sort ports =
  let renumerate ports = fst (List.fold_left (function ports,order -> function p ->
					  p.prt_order <- order;
					  (ports@[p]),(order+1)) ([],0) ports) in
  let ports_in, ports_out =
    partition_ports (List.sort (fun p1 p2 -> compare p1.prt_order p2.prt_order) ports) in
  (renumerate ports_in)@(renumerate ports_out)
