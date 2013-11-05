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

(* Anyway, we won't use the progress box in this file. Use a dummy one.*)
(* before : module Adequation = Adequation_core.Make (Progress_box.Dummy) *)

open Adequation_core

exception Error of string * int * string

let library_directory = ref "../libs"

let lexerbuffer_of_file f =
  let channel = open_in f in
  let lbf = Lexing.from_channel channel in
  Parserexpressioninit.lexerbuffer := lbf;
  lbf,channel

let lexerbuffer_of_library f =
  Lexing.from_string ("include \""^f^"\";")

let rec file_read lbf file lib line new_graph =
  Sdx_lexer.line:=line;
  Sdx_lexer.lib:=lib;
  (match new_graph with
  | None -> ()
  | Some g -> Sdx_lexer.adequation_graph := g);
  try
    match Sdx_parser.file Sdx_lexer.lexer lbf with
    | Include f ->
	let line = !Sdx_lexer.line in
	let ilib = Filename.chop_extension f in
	let  result = match Application.is_included ilib with
	  | true -> true
	  | false ->
	      Application.library_add ilib;
	      let ifile = Filename.concat !library_directory (Filename.basename f) in
		
	      let ilbf,ichannel = lexerbuffer_of_file ifile in
	      let result = file_read ilbf ifile ilib 1 None in
		close_in ichannel;
		result in
	  (match result with
	   | true -> 
	       Parserexpressioninit.lexerbuffer := lbf;
	       file_read lbf file lib line new_graph
	   | false -> false)
    | Done -> true
  with
  | Failure msg -> raise (Error (file, !Sdx_lexer.line, msg))
  | Parsing.Parse_error -> raise (Error (file, !Sdx_lexer.line, "Parse error"))

let file_parse file_name new_graph =
  let lbf,channel = lexerbuffer_of_file file_name in
  let result = file_read lbf file_name "" 1 new_graph in
  close_in channel;
  result

let library_parse lib_name = 
  file_read (lexerbuffer_of_library lib_name) "Ihm" "" 1 None

let open_syndex_file name =
  let graph = Adequationtypes.new_adequation_graph () in
  if (not (file_parse name (Some graph))) or ((hashtbl_length graph) = 0)
  then None
  else
    let archilib, archiname = Application.archi_main_get () in
    let oprlist = List.map (Architecture.operator_reference archilib archiname)
	(Architecture.operators_list archilib archiname) in
    let mdalist = List.map (Architecture.media_reference archilib archiname)
	(Architecture.media_list archilib archiname) in
    Adequation_core.route oprlist mdalist;
    Some graph

let open_syndex_library name =
  ignore (library_parse name)
