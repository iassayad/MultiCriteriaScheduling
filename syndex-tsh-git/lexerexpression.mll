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
open Parserexpression
}

rule lexer = parse
| [' ''\t'] {lexer lexbuf}
| '+' {PLUS}
| '-' {MINUS}
| '*' {TIMES}
| '/' {DIV}
| ',' {COMMA}
| '(' {LPAREN}
| ')' {RPAREN}
| '{' {LLIST}
| '}' {RLIST}
| '|' {BAR}
| ['a'-'z''A'-'Z''\\']['a'-'z''A'-'Z''_''0'-'9''\\']* {NAME(Lexing.lexeme lexbuf)}
| ['0'-'9']+('.'['0'-'9']+)?('e'['+''-']?['0'-'9']+)? {FLOAT(float_of_string(Lexing.lexeme lexbuf))}
| '"'[^'"']*'"' {let lx=Lexing.lexeme lexbuf in STRING(String.sub lx 1 (String.length lx - 2))} 
| eof {lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;EOE}
| _ {lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;EOE}
