(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                             Julien Forget                             *)
(*                          Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

(** This modules contains additional data structures and basis functions concerning a SynDEx application. *)
open Types

(** Type of a SynDEx application. *)
let application = {
  app_algorithms = [];
  app_algorithm_main = None;
  app_algorithm_main_arguments_values = [];
  app_operator_definitions = [];
  app_media_definitions = [];
  app_architectures = [];
  app_architecture_main = None;
  app_software_components = [];
  app_reverse_sc = Hashtbl.create 30;
  app_xsc_absolute_constraints = [];
  app_xsc_relative_constraints = [];
  app_constraints = []; (* this was used in older versions and is kept for ascending version compatibility *)
  app_libraries = [];
  app_description = "";
}

let get_application_description () =
  application.app_description

let set_application_description descr =
  application.app_description <- descr


(** Returns true if one of the definitions belonging to "lib" is referenced in one architecture, false if not *)
let library_referenced lib =
  (* comparison function for operator def *)
  let opr_def_in_lib ref =
    ref.oprref_definition.oprdef_library=lib
  (* comparison function for media def *)
  and mda_def_in_lib ref =
    ref.mdaref_definition.mdadef_library=lib in 
    (* testing for each archi of application (not belonging to lib) if exists a referenced op/media definition belonging to lib *)
    (List.exists (function archi -> 
		    archi.archi_library <> lib && (
		      List.exists (opr_def_in_lib) archi.archi_operators 
		      || List.exists (mda_def_in_lib) archi.archi_medias)) application.app_architectures 
     || 
     (* testing in application if exists a referenced algorithm belonging to lib *)
     let algo_belongs_to_lib algo_def =
       List.exists (function ref -> ref.ref_algorithm.algo_library=lib) algo_def.algo_references in 
       (* scanning definitions *)
       List.exists (function a -> if a.algo_library<>lib then (algo_belongs_to_lib a) else false) application.app_algorithms)
    
(** Returns the software component which reference defref belongs to. *)
let xsc_of_ref defref attachtype =
  try
    Some (Hashtbl.find application.app_reverse_sc (defref,AttachAll))
  with Not_found -> try
    Some (Hashtbl.find application.app_reverse_sc (defref,attachtype))
  with Not_found -> None

(** Returns the software component name which reference refpath belongs to. *)
let xscname_of_ref refpath attachtype =
  let xsc = xsc_of_ref refpath attachtype in
  match xsc with
  | None -> ""
  | Some xsc -> xsc.xsc_name

(** Removes reference defref from its software component. *)
let remove_ref_xsc defref attachtype =
  match xsc_of_ref defref attachtype with
  | None -> ()
  | Some xsc -> xsc.xsc_references <- List.filter (function (r,t) -> not (r = defref && t = attachtype)) xsc.xsc_references;
      Hashtbl.remove application.app_reverse_sc (defref,attachtype)

(** Returns the software component of name xscname. *)
let xsc xscname =
  List.find (function {xsc_name=n} -> n=xscname) application.app_software_components

(** Add reference defref to software component named xscname. *)
let add_ref_xsc defref attachtype xscname =
  let xsc = xsc xscname in
  xsc.xsc_references <- xsc.xsc_references@[(defref,attachtype)];
    Hashtbl.add application.app_reverse_sc (defref,attachtype) xsc

(** Moves reference defref from its current software component to software component named xscname. *)
let ref_modify_xsc defref attachtype xscname =
  remove_ref_xsc defref attachtype;
  add_ref_xsc defref attachtype xscname

(** Returns the list of software components (their name) in current application. *)
let xsc_namelist () =
  List.map (function {xsc_name=n} -> n) application.app_software_components

(** Returns the list of reference belonging to software component xscname. *)
let refs_of_xsc xscname =
  let xsc = xsc xscname in
  xsc.xsc_references

(** Adds a new software component named name to the current application software components list. *)
let xscname_define name =
  let defined = xsc_namelist () in
  match List.mem name defined with
  | true -> ()
  | false -> let xsc = {xsc_name = name;xsc_references = [];} in
    application.app_software_components <- application.app_software_components@[xsc]

(** Removes software component named name from the current application software components list. *)
let xscname_delete name =
  let defined = xsc_namelist () in
  try
    let xsc = List.find (function x -> x.xsc_name=name) application.app_software_components in
    application.app_software_components <- List.filter (function x -> x.xsc_name<>name) application.app_software_components;
      Hashtbl.iter (fun defref xsc -> match xsc.xsc_name=name with
		    | true -> Hashtbl.remove application.app_reverse_sc defref
		    | false -> ()) application.app_reverse_sc
  with
    Not_found -> () 
      
(** Adds a new software component named name to the current application software components list and makes references refs belong to this software component. *)
let xsc_define xsc refs =
  xscname_define xsc;
  List.iter (function (defref,attach_type) -> ref_modify_xsc defref attach_type xsc) refs

(** Adds constraint cstrt to the basic constraints of the application. *)
let constraints_create cstrt =
  application.app_constraints <- cstrt :: application.app_constraints

(** Returns the list of basic constraints of the application. *)
let constraints_list () =
  application.app_constraints

(** Returns the list of absolute constraints of the application. *)
let xsc_absolute_constraints_list () =
  application.app_xsc_absolute_constraints

(** Sets the absolute constraints of the application to absolute_constraints_list. *)
let xsc_absolute_constraints_save absolute_constraints_list =
  application.app_xsc_absolute_constraints <- absolute_constraints_list

(** Adds constraint absolute_constraint to the absolute constraints. *) 
let xsc_absolute_constraint_create absolute_constraint =
  application.app_xsc_absolute_constraints <- absolute_constraint :: application.app_xsc_absolute_constraints

(** Returns the list of relative constraints of the application. *)
let xsc_relative_constraints_list () =
  application.app_xsc_relative_constraints

(** Sets the relative constraints of the application to absolute_constraints_list. *)
let xsc_relative_constraints_save relative_constraints_list =
  application.app_xsc_relative_constraints <- relative_constraints_list

(** Adds constraint absolute_constraint to the relative constraints. *) 
let xsc_relative_constraint_create relative_constraint =
  application.app_xsc_relative_constraints <- relative_constraint :: application.app_xsc_relative_constraints

(** Sets main algorithm arguments values to args. *)
let algo_main_arguments_values_set args =
  application.app_algorithm_main_arguments_values <- args

(** Sets algorithm definition alg as main algorithm with arguments values args. *)
let algo_main_set alg args =
  application.app_algorithm_main <- Some alg;
  algo_main_arguments_values_set args

(** Sets main algorithm to None. *)
let algo_main_clear () =
  application.app_algorithm_main <- None;
  application.app_algorithm_main_arguments_values <- []

(** Returns main algorithm (lib,name,arguments values) *)
let algo_main_get () =
  match application.app_algorithm_main with
  | None -> "","",[]
  | Some algo -> algo.algo_library, algo.algo_name, application.app_algorithm_main_arguments_values

(** Returns main algorithm arguments values. *)
let algo_main_argumentsvalues_get () =
  match application.app_algorithm_main with
  | None -> []
  | Some algo -> application.app_algorithm_main_arguments_values

(** Sets architecture arc as main architecture. *)
let archi_main_set arc =
  application.app_architecture_main <- Some arc

(** Sets main architecture to None. *)
let archi_main_clear () =
  application.app_architecture_main <- None

(** Returns main architecture (lib,name). *)
let archi_main_get () =
  match application.app_architecture_main with
  | None -> "",""
  | Some archi -> archi.archi_library, archi.archi_name

(** Closes current application ie reinitialises its fields. *)
let close () =
  application.app_algorithms <- [];
  application.app_algorithm_main <- None;
  application.app_algorithm_main_arguments_values <- [];
  application.app_operator_definitions <- [];
  application.app_media_definitions <- [];
  application.app_architectures <- [];
  application.app_architecture_main <- None;
  application.app_software_components <- [];
  Hashtbl.clear application.app_reverse_sc;
  application.app_xsc_absolute_constraints <- [];
  application.app_xsc_relative_constraints <- [];
  application.app_constraints <- [];
  application.app_libraries <- [];
  application.app_description <- ""

(** Returns true if library lib_name is already included in the application *)
let is_included lib_name =
  List.mem lib_name application.app_libraries

(** Adds library libname to the included libraries. *)
let library_add lib_name =
  match is_included lib_name with
  | true -> ()
  | false -> application.app_libraries <- application.app_libraries@[lib_name]

(** Deletes library libname to the included libraries *)
let library_remove lib_name = 
  application.app_libraries <- List.filter (function l -> l<>lib_name) application.app_libraries

(** Returns the list of included libraries. *)
let included_libs () =
  application.app_libraries
    
(** Returns true if algorithm definition lib/name exists in the application. *)
let algorithm_exists lib name =
  List.exists (function {algo_name=n;algo_library=l} -> n=name && l=lib) application.app_algorithms

(** Returns the algorithm definition lib/name.
  Raises Failure if this definition doesn't exist. *)
let algorithmdef lib name =
  try
    List.find (function {algo_name=n;algo_library=l} -> n=name && l=lib) application.app_algorithms
  with Not_found -> failwith ("Algorithm definition "^(string_of_ref_type lib name)^" doesn't exist.")

(** Returns the operator definition lib/name.
  Raises Failure if this definition doesn't exist. *)
let operatordef lib name =
  try
    List.find (function {oprdef_name=n;oprdef_library=l} -> n=name && l=lib) application.app_operator_definitions
  with Not_found -> failwith ("Operator definition "^(string_of_ref_type lib name)^" doesn't exist.")

(** Returns the medium definition lib/name.
  Raises Failure if this definition doesn't exist. *)
let mediadef lib name =
  try
    List.find (function {mdadef_name=n;mdadef_library=l} -> n=name && l=lib) application.app_media_definitions
  with Not_found -> failwith ("Media definition "^(string_of_ref_type lib name)^" doesn't exist.")

(** Returns the architecture lib/name.
  Raises Failure if this architecture doesn't exist. *)
let architecture lib name =
  try
    List.find (function {archi_name=n;archi_library=l} -> n=name && l=lib) application.app_architectures
  with Not_found -> failwith ("Architecture "^(string_of_ref_type lib name)^" doesn't exist.")

(** Returns the list of operator references in architecture archilib/archiname. *)
let operators_list archilib archiname =
  let archi = architecture archilib archiname in
  List.map (function o -> Operator o) archi.archi_operators

(** Returns the list of medium references in architecture archilib/archiname. *)
let media_list archilib archiname =
  let archi = architecture archilib archiname in
  List.map (function m -> Media m) archi.archi_medias
