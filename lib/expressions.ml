
type binop = Plus
           | Minus
           | Multiply

type expression = Binop of binop * expression * expression 
                | Def of string * expression
                | Integer of int
                | String of string
                | Identifier of string
                | Unit
                
let is_value expr = match expr with
  | Integer _ | String _ | Unit -> true
  | _ -> false 