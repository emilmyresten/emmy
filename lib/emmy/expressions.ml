type binop = Plus | Minus | Times | Division | Mod | Equals | LessThan

type expression =
  | Def of string * expression
  | Fn of string list * expression
  (* the expression list is the list of arguments. The evaluation strategy is eager. Call-by-value *)
  | Invoke of expression * expression list
  | LetBinding of (string * expression) list * expression
  | Cond of expression list * expression
  | Binop of binop * expression * expression
  | List of expression list
  | Number of float
  | String of string
  | Identifier of string
  | True
  | False
  | Unit

let rec is_value expr =
  match expr with
  | Number _ | String _ | True | False | Unit | Fn (_, _) -> true
  | List exprs when is_list_of_values exprs -> true
  | _ -> false

and is_list_of_values exprs = List.for_all (fun m -> is_value m) exprs
