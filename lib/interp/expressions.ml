type binop = Plus | Minus | Times | Division | Mod | Equals | LessThan

type expression =
  | Def of string * expression
  | Fn of string list * expression
  (* the expression list is the list of arguments. The evaluation strategy is eager. Call-by-value *)
  | FnInvoke of expression * expression list
  | LetBinding of (string * expression) list * expression
  | Cond of expression list * expression
  | Binop of binop * expression * expression
  | Integer of int
  | String of string
  | Identifier of string
  | True
  | False
  | Unit

let is_value expr =
  match expr with
  | Integer _ | String _ | True | False | Unit | Fn (_, _) -> true
  | _ -> false
