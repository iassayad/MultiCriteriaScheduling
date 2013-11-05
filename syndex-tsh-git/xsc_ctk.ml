(*************************************************************************)
(*                                                                       *)
(*                               SynDEx 6                                *)
(*                                                                       *)
(*                          Christophe Macabiau                          *)
(*                                                                       *)
(*                     Projet Ostre, INRIA Rocquencourt                  *)
(*                                                                       *)
(*   Copyright 2000 Institut National de Recherche en Informatique et    *)
(*   en Automatique.  All rights reserved.  This file is distributed     *)
(*   under the terms of the Q Public License version 1.0.                *)
(*                                                                       *)
(*************************************************************************)

open Types
open Camltk
open Ihmcommon_ctk

let absolute_constraints_close top absdef =
  generic_window_close top (WAbsoluteConstraints absdef)

let relative_constraints_close top =
  generic_window_close top WRelative

let absolute_constraints_open top (lib,name) =
  let window = Toplevel.create Widget.default_toplevel [] in
  Wm.title_set window ("Absolute Constraints : "^(string_of_ref_type lib name));
  let msg=Label.create window [Text "Absolute Constraints"] in
  let bottom=Frame.create window [BorderWidth (Pixels 5)] in
  let frml = frame_create window in
  let lb_xsc = Listbox.create frml [SelectMode Extended] in
  let lb_xsc_scr = Scrollbar.create frml [Orient Vertical] in
  Listbox.configure lb_xsc [YScrollCommand (Scrollbar.set lb_xsc_scr)];
  Scrollbar.configure lb_xsc_scr [ScrollCommand (Listbox.yview lb_xsc)];
  Listbox.insert lb_xsc End (List.sort compare (Application.xsc_namelist ()));
  let varlist = List.map (function p -> 
    let v = Textvariable.create () in
    Textvariable.set v "0";
    (((lib,name),p),v)) (List.sort compare (Architecture.operators_list lib name)) in
  let frmr = frame_create window in
  let lb_cstr = Listbox.create frmr [SelectMode Extended] in
  let lb_cstr_scr = Scrollbar.create frmr [Orient Vertical] in
  Listbox.configure lb_cstr [YScrollCommand (Scrollbar.set lb_cstr_scr)];
  Scrollbar.configure lb_cstr_scr [ScrollCommand (Listbox.yview lb_cstr)];
  let create_button = button_create frml "Create" Raised in
  let remove_button = button_create frml "Remove" Raised in
  let ok = button_create bottom "ok" Raised in
  let cancel = button_create bottom "cancel" Raised in
  pack [msg] [Side Side_Top; Fill Fill_X];
  pack [lb_xsc] [Side Side_Left; Fill Fill_Y];
  pack [lb_xsc_scr] [Side Side_Left; Fill Fill_Y];
  List.iter (function (_,p),v -> pack [Checkbutton.create frml [Text p; Variable v; Anchor W]] [Side Side_Top;Fill Fill_X]) varlist;
  pack [create_button] [Side Side_Top];
  pack [remove_button] [Side Side_Top];
  pack [lb_cstr] [Side Side_Left; Fill Fill_Y];
  pack [lb_cstr_scr] [Side Side_Right; Fill Fill_Y];
  pack [ok;cancel] [Side Side_Left; Expand true];
  pack [bottom] [Side Side_Bottom; Fill Fill_X];
  pack [frml] [Side Side_Left; Expand true; Fill Fill_Both];
  pack [frmr] [Side Side_Right; Expand true; Fill Fill_Both];
  let constraints_lib_name,constraints_others = ref [], ref [] in
  let constraints_update () =
    let xsclist = List.map (function i -> Listbox.get lb_xsc i) (Listbox.curselection lb_xsc) in
    let proclist,_ = List.split (List.filter (function p,v -> Textvariable.get v = "1") varlist) in
    let update xsc =
      constraints_lib_name := (try (List.remove_assoc xsc !constraints_lib_name) with Not_found -> !constraints_lib_name);
      match proclist with
      | [] -> ()
      | _ -> constraints_lib_name :=  !constraints_lib_name @ [(xsc,proclist)] in
    List.iter update xsclist in
  let constraints_view () =
    let lcstr = List.map (function xsc,proclist -> 
      let procs = List.fold_left (function s -> function (_,p) -> s^p^" ") " " proclist in (xsc^procs)) !constraints_lib_name in
    Listbox.delete lb_cstr (Number 0) End;
    Listbox.insert lb_cstr End lcstr;
    let xsclist = List.map (function i -> Listbox.get lb_xsc i) (Listbox.curselection lb_xsc) in
    let proclist = match xsclist,!constraints_lib_name with
    | [],_ -> []
    | _,[] -> []
    | _ -> intersections (List.map (function xsc -> try List.assoc xsc !constraints_lib_name with Not_found -> []) xsclist) in
    List.iter (function p,v -> Textvariable.set v (match List.mem p proclist with | true -> "1" | false -> "0")) varlist in
  let delete_constraints () =
    let dellist = List.map (function i -> List.nth !constraints_lib_name (match i with | Number n -> n | _ -> 0)) (Listbox.curselection lb_cstr) in
    constraints_lib_name := List.filter (function e -> not (List.mem e dellist)) !constraints_lib_name;
    constraints_view () in
  let load_constraints () =
    let cs = Application.xsc_absolute_constraints_list () in
    let libname,others = List.fold_left (function libname,others -> function xsc,oprslist ->
      let oprslibname,oprsothers = List.partition (function ((l,n),_) -> l=lib && n=name) oprslist in
      let f oprs l = match oprs with
      |	[] -> l
      |	_ -> (xsc,oprs)::l in
      (f oprslibname libname),(f oprsothers others)) ([],[]) (Application.xsc_absolute_constraints_list ()) in
    constraints_lib_name := libname;
    constraints_others := others;
    constraints_view () in
  let save_constraints () =
    let xscs = remove_copies (List.map fst ((!constraints_lib_name)@(!constraints_others))) in
    let constraints = List.map (function xscname ->
				  let constraints_xsc = List.filter (function (xsc,_) -> xsc = xscname) ((!constraints_lib_name)@(!constraints_others)) in
				  let oprs = List.concat (List.map snd constraints_xsc) in
				    xscname,oprs) xscs in
      Application.xsc_absolute_constraints_save constraints;
      top.tw_file_changed () in
    Button.configure create_button [Command (function () -> constraints_update (); constraints_view ())];
    Button.configure remove_button [Command (function () -> delete_constraints (); constraints_view ())];
    Button.configure ok [Command (function _ -> absolute_constraints_close top (lib,name);save_constraints ())];
    Button.configure cancel [Command (function _ -> absolute_constraints_close top (lib,name))];
    bind lb_xsc [[],ButtonReleaseDetail 1] (BindSet ([], (function _ -> constraints_view ())));
    bind lb_cstr [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> delete_constraints()));
    load_constraints ();
    bind window [[], Destroy] (BindSet ([],function _ -> absolute_constraints_close top (lib,name)));
    window

and relative_constraints_open top =
  let constraints = ref [] in
  let uniondisjunction = Textvariable.create () in
  let window = Toplevel.create Widget.default_toplevel [] in
  Wm.title_set window "Relative Constraints";
  let ttl=Label.create window [Text "Relative Constraints"] in
  let bottom=Frame.create window [BorderWidth (Pixels 5)] in
  let frml = frame_create window in
  let lb_xsc = Listbox.create frml [SelectMode Extended] in
  let lb_xsc_scr = Scrollbar.create frml [Orient Vertical] in
  Listbox.configure lb_xsc [YScrollCommand (Scrollbar.set lb_xsc_scr)];
  Scrollbar.configure lb_xsc_scr [ScrollCommand (Listbox.yview lb_xsc)];
  Listbox.insert lb_xsc End (List.sort compare (Application.xsc_namelist ()));
  Textvariable.set uniondisjunction "";
  let rbunion = Radiobutton.create frml [Text "Union"; Value "Union"; Variable uniondisjunction]
  and rbdisjunction = Radiobutton.create frml [Text "Disjunction"; Value "Disjunction"; Variable uniondisjunction] in
  let frmr = frame_create window in
  let lb_cstr = Listbox.create frmr [SelectMode Extended] in
  let lb_cstr_scr = Scrollbar.create frmr [Orient Vertical] in
  Listbox.configure lb_cstr [YScrollCommand (Scrollbar.set lb_cstr_scr)];
  Scrollbar.configure lb_cstr_scr [ScrollCommand (Listbox.yview lb_cstr)];
  let ok = button_create bottom "ok" Raised in
  let cancel = button_create bottom "cancel" Raised in
  pack [lb_xsc] [Side Side_Left; Fill Fill_Both; Expand true];
  pack [lb_xsc_scr] [Side Side_Left; Fill Fill_Y];
  pack [rbunion;rbdisjunction] [Side Side_Top; Anchor W];
  pack [lb_cstr] [Side Side_Left; Fill Fill_Both; Expand true];
  pack [lb_cstr_scr] [Side Side_Right; Fill Fill_Y];
  pack [ttl] [Side Side_Top; Fill Fill_X];
  pack [ok;cancel] [Side Side_Left; Expand true];
  pack [bottom] [Side Side_Bottom; Fill Fill_X];
  pack [frml] [Side Side_Left; Expand true; Fill Fill_Both];
  pack [frmr] [Side Side_Right; Expand true; Fill Fill_Both];
  let constraints_update () =
    let lcstr = List.map (function rel,xsclist -> 
      let reltype = match rel with
      | Union -> "Union"
      | Disjunction -> "Disjunction"
      and xscs = List.fold_left (function s -> function xsc -> s^xsc^" ") " " xsclist in
      (reltype^xscs)) !constraints in
    Listbox.delete lb_cstr (Number 0) End;
    Listbox.insert lb_cstr End lcstr in
  let constraints_add () =
    let add reltype =
      match List.map (function i -> Listbox.get lb_xsc i) (Listbox.curselection lb_xsc) with
      |	[] | _::[] -> ()
      |	xsclist -> constraints :=  !constraints @ [reltype,xsclist] in
    if (Textvariable.get uniondisjunction) = "Union" then add Union
    else if (Textvariable.get uniondisjunction) = "Disjunction" then add Disjunction;
    constraints_update () in
  let constraints_delete () =
    let dellist = List.map (function i -> List.nth !constraints (match i with | Number n -> n | _ -> 0)) (Listbox.curselection lb_cstr) in
    constraints := List.filter (function e -> not (List.mem e dellist)) !constraints;
    constraints_update () in
  let load_constraints () = constraints := Application.xsc_relative_constraints_list (); constraints_update () in
  let save_constraints () = Application.xsc_relative_constraints_save !constraints in
  bind rbunion [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> constraints_add ()));
  bind rbdisjunction [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> constraints_add ()));
  bind lb_cstr [[Double],ButtonPressDetail 1] (BindSet ([], function _ -> constraints_delete ()));
  Button.configure ok [Command save_constraints];
  Button.configure cancel [Command (function _ -> relative_constraints_close top)];
  load_constraints ();
  bind window [[], Destroy] (BindSet ([],function _ -> relative_constraints_close top));
  window

let absolute_constraints_libname top absdef =
  generic_window_open top (WAbsoluteConstraints absdef) (function () -> absolute_constraints_open top absdef)

let absolute_constraints_main top =
  let lib,name = Application.archi_main_get () in
  match lib,name with
  | "","" -> ()
  | _ -> absolute_constraints_libname top (lib,name)

let absolute_constraints_choose top =
  let alist = Architecture.architectures_list () in
  let title = "Choose architecture to constraint :" in
  choose_libs_defs (absolute_constraints_libname top) alist title Widget.default_toplevel top WAsk_constraints

let relative_constraints top =
  generic_window_open top WRelative (function () -> relative_constraints_open top)


(* get faults input data *)
let faults_data_choose top =
  let npf,nmf,check,ok_action = ref 0, ref 0, ref false, ref false  in
  let ask_for_fun v1 v2 = 
    let rgxp = "\\([0-9]*\\)" in
    let v1,err = analyze v1 rgxp in
    (match err <> "" with 
    | true  -> error_message top err
    | false -> ok_action := true;npf:=int_of_string (List.hd (List.hd v1)));
    let v2,err = analyze v2 rgxp in
    (match err <> "" with 
    | true  -> ok_action := false;error_message top err
    | false -> ok_action := true;nmf:=int_of_string (List.hd (List.hd v2)))  in
  ask_for_more "00" "Enter the number of processor failures" "Enter the number of communication media failures" "Split data"
  ask_for_fun check Widget.default_toplevel top WAsk_create_xsc; !ok_action,!npf,!nmf,!check


let obj_len = ref 0.
let obj_rel = ref 0.
let level_max = ref 1
let theta = ref 45.


(* get reliability input data *)
let reliability_data_choose top =
  let check = ref true in
  let ok_action =  ref false  in
  let ask_for_fun v1 v2 v3 v4 = 
    let rgxp = "\\([0-9]*\\)" in
    let rgxp_float = "\\([0-9]+\\(.[0-9]+\\)?\\)" in 
    let v1,err = analyze v1 rgxp_float in 
      (match err <> "" with 
	 | true  -> error_message top err
	 | false -> ok_action := true;obj_len:=float_of_string (List.hd (List.hd v1)));
    let v2,err = analyze v2 rgxp_float in
      (match err <> "" with 
	 | true  -> error_message top err
	 | false -> ok_action := true;obj_rel:=float_of_string (List.hd (List.hd v2)));
    let v3,err = analyze v3 rgxp in
      (match err <> "" with 
	 | true  -> error_message top err
	 | false -> ok_action := true;level_max:=int_of_string (List.hd (List.hd v3)));
    let v4,err = analyze v4 rgxp_float in
	(match err <> "" with 
	   | true  -> ok_action := false;error_message top err
	   | false -> ok_action := true;theta:=float_of_string (List.hd (List.hd v4)))  in
    ask_for_more_rel (string_of_float !obj_len) (string_of_float !obj_rel) (string_of_int !level_max) 
                     (string_of_float !theta) "Enter the length objective" "Enter the reliability objective"
                "Enter the level of redundancy" "Enter the theta parameter"
    ask_for_fun check Widget.default_toplevel top WAsk_create_xsc;
   !ok_action,!obj_len,!obj_rel,!level_max ,!theta
  
