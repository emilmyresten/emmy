
type binop = Plus
           | Minus
           | Multiply

type expression = Binop of binop * expression * expression 
                | Def of string * expression
                | Integer of int
                | String of string
                
let rec is_value expr = match expr with
  | Integer _ | String _ -> true
  | Def (_, v) when is_value v -> true
  | _ -> false 