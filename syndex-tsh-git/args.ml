(* Demonstration of the Getopt module *)


let theta = ref 0. (* In degrre, converted to radian see reliability.ml  *)
let obj_len = ref 100000.
let replication_level = ref 3
(*let olet obj_len = ref 49.bj_rel = ref 0.999989439647  *)
let obj_rel = ref 0.
and number_of_failures = ref 1
let archive = ref false
and update  = ref false
and verbose = ref 0
and includ  = ref [1]
and output  = ref ""

let bip ()  = Printf.printf "\007"; flush stdout
let wait () = Unix.sleep 1 

(*let specs = 
[
  ( 'x', "execute", None, Some (fun x -> Printf.printf "execute %s\n" x));
  ( 'I', "include", None, (append includ));
  ( 'o', "output",  None, (atmost_once output (Error "only one output")));
  ( 'a', "archive", (set archive true), None);
  ( 'u', "update",  (set update  true), None);
  ( 'v', "verbose", (incr verbose), None);
  ( 'X', "",        Some bip, None);
  ( 'w', "wait",    Some wait, None)

]*)

(*let _ = *)
(*let parse_cmdline  =
  (*Getopt.parse_cmdline specs print_endline;*)

  Printf.printf "archive = %b\n" !archive;
  Printf.printf "update  = %b\n" !update;
  Printf.printf "verbose = %i\n" !verbose;
  Printf.printf "output  = %s\n" !output;
  List.iter (fun x -> Printf.printf "include %s\n" x) !includ;;
*)
