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
open Ihmcommon_ctk
open Adequationtypes

let ail_of_schedules_list () =
  let dataid com esfs =
    let opn,prt = data_of_communication com in
    (name_of_operation opn)^"_"^prt.t_prt_name^"_"^(string_of_float esfs) in
  let convopn opnlist =
    let opnlist = List.sort (function (_,a,_) -> function (_,b,_) -> compare a b) opnlist in
    let opns1,opns2,_ = List.fold_left (function l,n,e -> function (opn,esfs,eefs) as o -> match (List.filter (function p -> Adequation.pi opn <> Adequation.pi p) (Adequationtypes.predecessors opn))=[] && esfs=e with
    | true -> l,n@[o],eefs
    | false -> l@[n],[o],eefs) ([],[],0.) opnlist in
    let opns = List.filter ((<>) []) (opns1@[opns2]) in
    let logicaltasks l =
      let lts = List.fold_left (function s -> function (opn,_,_) -> s^"<LogicalTask id=\""^(Adequationtypes.name_of_operation opn)^"\"/>\n") "" l in
      "<Tasks>\n"^lts^"</Tasks>\n" in
    let activatingdatas opn =
      let actdta = List.fold_left (function s -> function opnp -> s^(match Adequation.pi opnp = Adequation.pi opn with
      | true -> Adequationtypes.name_of_operation opnp
      | false -> dataid opnp opnp.Adequationtypes.t_opn_esfs)^"\n") "" (Adequationtypes.predecessors opn) in
      "<ActivatingData>\n"^actdta^"</ActivatingData>\n" in
    let f l =
      let opn,_,_ = List.hd l in
      let pbl = match Adequationtypes.predecessors opn with
      | [] -> "<OsTask priority=\"1\" preemptable=\"false\" period=\"1\" initialoffset=\"0\">\n"
      | _ -> "<OsTask priority=\"1\" preemptable=\"false\">\n" in
      pbl^(activatingdatas opn)^(logicaltasks l)^"</OsTask>\n" in
    match opns with
    | [] -> "" 
    | _ -> List.fold_left (function s -> function l -> s^(f l)) "" opns in

  let xsclist opnlist =
    let xscs = remove_copies (List.map (function (opn,_,_) -> Application.xscname_of_ref opn.Adequationtypes.t_opn_path) opnlist) in
    let xscs = List.filter ((<>) "") xscs in
    List.fold_left (function s -> function xsc -> s^"<SoftwareComponent id=\""^xsc^"\"/>\n") "" xscs in

  let convcom comlist =
    let f (com,esfs,_) =
      let data = dataid com esfs in
      "<Frame id=\""^data^"\">\n<Data id=\""^data^"\"/>\n</Frame>\n" in
    List.fold_left (function s -> function com -> s^(f com)) "" comlist in

  let body = List.fold_left (function s -> function opr,opnlist -> s^(match opr with
  | Types.Operator _ -> "<ECU id=\""^(Adequation.name_of_operator opr)^"\">\n"^(xsclist opnlist)^(convopn opnlist)^"</ECU>\n"
  | Types.Media _ -> "<Network id=\""^(Adequation.name_of_operator opr)^"\">\n"^(convcom opnlist)^"</Network>\n")) "" !(Adequation.schedules) in
  let preamble = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<!DOCTYPE OperArchi SYSTEM \"operarchi.dtd\">\n<OperArchi>\n" in
  let enddoc = "</OperArchi>" in
  preamble^body^enddoc

let ail_export top =
  (*ignore (Adequation.adequation top.tw_msg_bar);*)
  (match top.tw_current_file with
  | None -> ();
  | Some name ->
      let filename = (Filename.chop_extension name)^".xml" in
      let f = open_out filename in
      output_string f (ail_of_schedules_list ());
      close_out f)

