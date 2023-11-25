


type expression = Def of string * expression
                | Fn of string list * expression
                | FnInvoke of expression * expression list (* the list of arguments. The evaluation strategy is eager. Call-by-value *)
                | Integer of int
                | String of string
                | Identifier of string
                | Parameter of string (* Unrealized but bound in function body. *)
                | Unit
                
let rec is_value expr = match expr with
  | Integer _ | String _ | Unit | Parameter _ -> true
  | Fn (_, expr) when is_value expr -> true
  | _ -> false 