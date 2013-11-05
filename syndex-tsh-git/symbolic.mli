type bin_operator = (float -> float -> float) * string * int
and un_operator = (float -> float) * string
val add : bin_operator
val sub : bin_operator
val mul : bin_operator
val div : bin_operator
val uminus : un_operator
type expression =
    Var of string
  | Float of float
  | String of string
  | Bin of bin_operator * expression * expression
  | Un of un_operator * expression
  | List of expression list
  | Card of expression
and result = RFloat of float | RString of string | RList of result list
and context = (string * expression) list
val renamevariables : expression -> (string * string) list -> expression
val result_of_expression : context -> expression -> result
val result_of_variable : context -> string -> result
val string_of_expression : expression -> string
val string_of_expression_list : expression list -> string
val string_of_result : result -> string
