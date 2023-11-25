
type binop = Plus
           | Minus
           | Multiply

type expression = Binop of binop * expression * expression 
                | Def of string * expression
                | Fn of string list * expression
                | Integer of int
                | String of string
                | Identifier of string
                | Unit
                
let rec is_value expr = match expr with
  | Integer _ | String _ | Unit -> true
  | Fn (_, expr) when is_value expr -> true
  | _ -> false 