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

exception Cannot_compute

type bin_operator = (float->float->float) * string * int
type un_operator = (float->float) * string
let add,sub,mul,div = (( +. ),"+",1),(( -. ),"-",1),(( *. ),"*",2),(( /. ),"/",2)
let uminus=(fun x -> -.x), "-"

type expression = 
  | Var of string
  | Float of float
  | String of string
  | Bin of bin_operator * expression * expression
  | Un of un_operator * expression
  | List of expression list
  | Card of expression

type result =
  | RFloat of float
  | RString of string
  | RList of result list

type context = (string*expression) list

let rec renamevariables expr varlist =
  match expr with
  | Bin(f,left,right) -> Bin(f,(renamevariables left varlist),(renamevariables right varlist))
  | Un(f,x) -> Un(f,(renamevariables x varlist))
  | Var x -> (match List.mem_assoc x varlist with
    | true -> Var (List.assoc x varlist)
    | false -> Var x)
  | Card x -> Card(renamevariables x varlist)
  | _ -> expr

let rec result_of_expression context expr =
  match expr with
  | Bin((f,_,_) as operator,left,right) -> (match (result_of_expression context left),(result_of_expression context right) with
   | RFloat a, RFloat b -> (match (operator==div) && (b=0.) with
     | true -> raise (Failure "Divide by 0")
     | false -> RFloat (f a b))
   | _ -> raise (Failure "Binary operator error"))
  | Un ((f,_),x) -> (match result_of_expression context x with
    | RFloat a -> RFloat (f a)
    | _ -> raise (Failure "Unary operator error"))
  | Float f -> RFloat f
  | Var x -> result_of_variable context x
  | String s -> RString ("\""^s^"\"")
  | List l -> RList (List.map (result_of_expression context) l)
  | Card x -> (match result_of_expression context x with
    | RList l -> RFloat (float_of_int (List.length l))
    | _ -> raise (Failure "Card operator error"))

and result_of_variable context var =
  match List.mem_assoc var context with
  | false -> raise (Failure ("Parameter value undefined "^var))
  | true -> match List.assoc var context with
    | Var "" -> raise (Failure ("Parameter value undefined "^var))
    | expr -> result_of_expression context expr

let rec string_of_expression expr =
  match expr with
  | Var v -> v
  | Float f when fst (modf f) = 0.0 -> string_of_int (int_of_float f)
  | Float f -> string_of_float f
  | String s -> "\""^s^"\""
  | Un ((_,s),e) -> 
      let se = string_of_expression e in
      (match e with
      | Bin _ -> s^"("^se^")"
      | _ -> s^se)
  | Bin ((_,s,p),left,right) ->
      let f e = match e with
      | Bin((_,_,p'),_,_) when p'<p -> "("^(string_of_expression e)^")"
      | _ -> string_of_expression e in
      (f left)^s^(f right)
  | List l -> "{"^(string_of_expressionlist l)^"}"
  | Card l -> "|"^(string_of_expression l)^"|"

and string_of_expressionlist l =
  let s = List.fold_left (function s -> function e -> s^(string_of_expression e)^",") "" l in
  String.sub s 0 (String.length s - 1)

let string_of_expression_list l =
  let s = List.fold_left (function s -> function e -> s^(string_of_expression e)^";") "" l in
  String.sub s 0 (String.length s - 1)

let rec string_of_result r =
  match r with
  | RFloat f when fst (modf f) = 0.0 -> string_of_int (int_of_float f)
  | RFloat f -> string_of_float f
  | RString s -> s
  | RList l ->
      let s = List.fold_left (function s -> function r -> s^(string_of_result r)^",") "" l in
      "{"^(String.sub s 0 (String.length s - 1))^"}"
