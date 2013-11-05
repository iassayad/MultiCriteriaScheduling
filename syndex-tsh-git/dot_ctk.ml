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

open Camltk
open Types
open Adequationtypes

let dot_program = ref ""
let gs_program = ref ""

let ps_of_graph graph filename =
  let labeldependence = [Strong_Precedence_Data,"S";
			 Weak_Precedence_Data,"W";
			 Precedence,"P";
			 Types.Data,"D"] in
  let name_of_identifier opn = identifier_of_path_name (List.tl (List.tl opn.t_opn_path)) "" in
  let pt2us n = Str.global_replace (Str.regexp "\\.") "_" n in
  let cond cl = cut (List.fold_left (function s -> function cond_src,vl -> s^(match cond_src,vl with
  | None,None -> "T,"
  | (Some (opn,prt)),(Some vl) -> (identifier_of_operation opn)^"."^prt.t_prt_name^"="^(string_of_int vl)^","
  | _ -> raise (Failure ("Dot_ctk.error : error")))) "" cl) 1 in
  let create_struct op =
    let ports plist dir = List.filter (function {t_prt_dir=d} -> d=dir) plist in
    let convports plist =
      let s = List.fold_left (function s -> function {t_prt_name=n;t_prt_dir=d} ->
	let d = match d with
	| Port.In -> "In"
	| Port.Out -> "Out" in
	s^"<"^d^(pt2us n)^">"^n^"|") "" plist in
      "{"^(cut s 1)^"}" in
    let pin = convports (ports op.t_opn_ports Port.In)
    and pout = convports (ports op.t_opn_ports Port.Out) in
    let opr o = match o.t_opn_operator with
    | None -> ""
    | Some opr -> " ("^(name_of_operator opr)^","^(string_of_int op.t_opn_rank)^")" in
    "\""^(identifier_of_operation op)^"\"[shape=record,fontsize=9,label=\"{"^pin^"|{"^(identifier_of_operation op)^"\\n"^(cond op.t_opn_condition)^(opr op)^"}|"^pout^"}\"];\n" in
  let create_dep op =
    List.fold_left (function s -> function {t_dpd_sopn=sopn;t_dpd_sprt=sprt;t_dpd_dopn=dopn;t_dpd_dprt=dprt;t_dpd_class=dpdclass;t_dpd_condition=cl} ->
      let tdpd,color = match dpdclass with
      | (Condition i),_ -> ("C "^(string_of_int i)),"color=red,fontcolor=red,"
      | Data,l -> (List.assoc l labeldependence),"" in
      let cond= tdpd^":"^(cond cl) in
      s^"\""^(identifier_of_operation sopn)^"\":\"Out"^(pt2us sprt.t_prt_name)^"\" -> \""^(identifier_of_operation dopn)^"\":\"In"^(pt2us dprt.t_prt_name)^"\" ["^color^"fontsize=9,label=\""^cond^"\"]"^";\n") "" op.t_opn_dependences_successors in
(*  let graphstring = ref "digraph G {\nrankdir=LR;\nnodesep=.1;\nranksep=.3;\npage=\"8,10.5\";\nmargin=\"0.1,0.1\";\nrotate=90;\n" in*)
  let graphstring = ref "digraph G {ratio=auto;\nnodesep=.1;\nranksep=.3;\nmargin=\"0.1,0.1\";\nrotate=90;\n" in
  Hashtbl.iter (function _ -> function o -> graphstring:=(!graphstring)^(create_struct o)) graph;
  Hashtbl.iter (function _ -> function o -> graphstring:=(!graphstring)^(create_dep o)) graph;
  graphstring := (!graphstring)^"}";
  let tmpfile = "tmp_dot.ps" in
  let f = open_out tmpfile in
  output_string f (!graphstring);
  close_out f;
  let pid = exec !dot_program ["-Tps";tmpfile;"-o";filename] in
  ignore(Unix.waitpid [] pid);
  Sys.remove tmpfile

let ps currentdir msgbar graph =
  match Tk.getSaveFile [DefaultExtension ".ps";FileTypes [{typename="Fichiers PostScript";extensions=[".ps"];mactypes=[]}];InitialDir currentdir] with
  | "" -> ()
  | filename ->
      let dirname,basename = (Filename.dirname filename),(Filename.basename filename) in
      let filename = Filename.concat dirname (try Filename.chop_extension basename with _ -> basename) in
      ps_of_graph graph (filename^".ps")

let jpeg currentdir msgbar graph =
  match Tk.getSaveFile [FileTypes [{typename="Fichiers jpeg";extensions=[".jpg";".jpeg"];mactypes=[]}];InitialDir currentdir] with
  | "" -> ()
  | filename ->
      let dirname,basename = (Filename.dirname filename),(Filename.basename filename) in
      let filename = Filename.concat dirname (try Filename.chop_extension basename with _ -> basename) in
      let tmpfile = filename^"tmp_dot_jpg.ps" in
      ps_of_graph graph tmpfile;
      let pid = exec !gs_program ["-q";"-dNOPROMPT";"-dNOPAUSE";"-dBATCH";"-sDEVICE=jpeg";"-sPAPERSIZE=a4";"-sOutputFile="^filename^"%d.jpg";tmpfile] in
      ignore(Unix.waitpid [] pid);
      Sys.remove tmpfile
