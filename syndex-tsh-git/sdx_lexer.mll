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

{
  open Sdx_parser
  let line = ref 1
  let lib = ref ""
  let keyword_table = Hashtbl.create 100
  let adequation_graph = ref (Adequationtypes.new_adequation_graph ())
  let _ =
    List.iter (fun (kwd, tok) -> Hashtbl.add keyword_table kwd tok)
      (* Put all keywords which would otherwise be accepted by the NAME rule here.
	 Those are words of more than one letter *)
      [
	(* General application information *)
	"include", INCLUDE;
	"def", DEF;
	"main", MAIN;
	"application", APPLICATION;
	"description", DESCRIPTION;
	"software_component", XSC;
	"constraint", CONSTRAINT;
	"absolute", ABSOLUTE;
	"relative", RELATIVE;
	"union", UNION;
	"disjunction", DISJUNCTION;
	"syndex_version", SYNDEX_VERSION;
	"initseq",INIT_SEQ;
	"loopseq",LOOP_SEQ;
	"endseq",END_SEQ;
	"code_phases", CODE_PHASES;
	(*---- Algorithm ----*)
	"constant", CONSTANT;
	"sensor", SENSOR;
	"actuator", ACTUATOR;
	"memory",(*|"delay"*) MEMORY;
	"algorithm",(*|"function"*) ALGORITHM;
	"internal", INTERNAL;
	"attach_all",ATTACH_ALL;
	"attach_ref", ATTACH_REF;
	"attach_condi", ATTACH_CONDI;
	"attach_condo", ATTACH_CONDO;
	"attach_explode", ATTACH_EXPLODE;
	"attach_implode", ATTACH_IMPLODE;
	"conditions", CONDITIONS;
	"references", REFERENCES;
	"dependences", DEPENDANCES;
	"strong_precedence_data", STRONGPRECEDENCEDATA;
	"weak_precedence_data", WEAKPRECEDENCEDATA;
	"precedence", PRECEDENCE;
	"data", DATA;
	(*---- Architecture ----*)
	"architecture", ARCHITECTURE;
	"operator", OPERATOR;
	"operators", OPERATORS;
	"gate", GATE;
	"media", MEDIA;
	"medias", MEDIAS;
	"sampp", SAMPP;
	"sammp", SAMMP;
	"ram", RAM;
	"broadcast", BROADCAST;
	"no_broadcast", NOBROADCAST;
	"extra_durations_operator", EXTRA_DURATIONS_OPERATOR;
	"extra_durations_media", EXTRA_DURATIONS_MEDIA;
	"connections", CONNECTIONS;
	(*---- Adequation result ----*)
	"ports", PORTS;
	"schedules", SCHEDULES;
	"operation_scheduled", OPERATION_SCHEDULED;
	"scheduled", SCHEDULED;
	"calcul", CALCUL;
	"communication", COMMUNICATION;
	"send", SEND;
	"receive", RECEIVE;
	"sync", SYNC;
	"send_synchro", SEND_SYNCHRO;
	"receive_synchro", RECEIVE_SYNCHRO;
	"read", READ;
	"write", WRITE;
	"ihm", IHM;
	"condI", CONDI;
	"condO", CONDO;
	"explode", EXPLODE;
	"implode", IMPLODE;
	"synchro_constant", SYNCHRO_CONSTANT;
	"cond_level", COND_LEVEL;
	"schedule_dependences", SCHEDULE_DEPENDENCES;
	"schedule_conditions", SCHEDULE_CONDITIONS;
	(*---- Misc ----*)
	"on", ON;
	"true", TRUE;
	"false", FALSE
      ]
}

rule lexer = parse
    (*---- Symbols ----*)
  | eof {EOF}
  | "?" {IN}
  | "!" {OUT}
  | "->" {TO}
  | '@' {AT}
  | '=' {EQU}
  | '\\' {BACKSLASH}
  | '/' {DIV}
  | '-' {MINUS}
  | '|' {BAR}
  | '[' {LDIM}
  | ']' {RDIM}
  | '<' {LARG}
  | '>' {RARG}
  | '{' {LLIST}
  | '}' {RLIST}
  | '(' {LPAR}
  | ')' {RPAR}
  | '&' {AND}
  | ':' {COL}
  | ',' {COMMA}
  | ';' {SCOL}
  | '.' {DOT}
  | [' ''\t'] {lexer lexbuf}
  | ['\n'] {incr line; lexer lexbuf}
  | '#'[^'\n']* {COMMENT}
      (*---- Base value types ----*)
  | '"'[^'"']*'"' * {let lx=Lexing.lexeme lexbuf in STRING(String.sub lx 1 (String.length lx - 2))} 
  | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''-''0'-'9''*']*
    {let lx = Lexing.lexeme lexbuf in
       try
	 Hashtbl.find keyword_table lx
       with Not_found ->
	 NAME(lx)}
  | ['+''-']?['0'-'9']+('.'['0'-'9']*)?('e'['+''-']?['0'-'9']+)? {FLOAT(float_of_string(Lexing.lexeme lexbuf))}
