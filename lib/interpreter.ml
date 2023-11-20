
let rec eval ast =
  let open Expressions in
  match ast with
  | PlusExpr (lhs, rhs) -> (eval lhs) + (eval rhs)
  | MinusExpr (lhs, rhs) -> (eval lhs) - (eval rhs)
  | Integer v -> v
  (* | _ -> raise (Failure "No matching expression to evaluate") *)


