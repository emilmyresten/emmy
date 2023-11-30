

type binop = Plus 
           | Minus
           | Times
           | Division
           | Equals
           | LessThan


type expression = Def of string * expression
                | Fn of string list * expression
                | FnInvoke of expression * expression list (* the list of arguments. The evaluation strategy is eager. Call-by-value *)
                | Cond of expression list * expression
                | Binop of binop * expression * expression
                | Integer of int
                | String of string
                | Identifier of string
                | True
                | False
                | Unit
                
let is_value expr = match expr with
  | Integer _ | String _ | True | False | Unit | Fn (_, _) -> true
  | _ -> false 