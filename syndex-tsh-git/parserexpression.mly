/*************************************************************************/
/*                                                                       */
/*                               SynDEx 6                                */
/*                                                                       */
/*                          Christophe Macabiau                          */
/*                                                                       */
/*                     Projet Ostre, INRIA Rocquencourt                  */
/*                                                                       */
/*   Copyright 2000 Institut National de Recherche en Informatique et    */
/*   en Automatique.  All rights reserved.  This file is distributed     */
/*   under the terms of the Q Public License version 1.0.                */
/*                                                                       */
/*************************************************************************/

%{
%}

%token EOE
%token PLUS MINUS TIMES DIV
%token COMMA LPAREN RPAREN LLIST RLIST BAR
%token <string> NAME
%token <float> FLOAT
%token <string> STRING
%left PLUS MINUS
%left TIMES DIV

%type <Symbolic.expression> expression
%start expression

%%

expr_list:
| {[]}
| expr_list_continue {$1};

expr_list_continue:
| expr {[$1]}
| expr_list_continue COMMA expr {$1@[$3]};

expr:
| NAME {Symbolic.Var $1}
| FLOAT {Symbolic.Float $1}
| STRING {Symbolic.String $1}
| LPAREN expr RPAREN {$2}
| expr PLUS expr {Symbolic.Bin(Symbolic.add,$1,$3)}
| expr MINUS expr {Symbolic.Bin(Symbolic.sub,$1,$3)}
| expr TIMES expr {Symbolic.Bin(Symbolic.mul,$1,$3)}
| expr DIV expr {Symbolic.Bin(Symbolic.div,$1,$3)}
| MINUS expr {Symbolic.Un(Symbolic.uminus,$2)}
| LLIST expr_list RLIST {Symbolic.List $2}
| BAR expr BAR {Symbolic.Card $2};

expression: expr EOE {$1};
