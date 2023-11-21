open Expressions

let rec string_of_val e = match e with
  | Integer v -> Printf.sprintf "Integer %d" v
  | String str -> Printf.sprintf "String %s" str
  | Def (id, v) -> Printf.sprintf "Def %s %s" id (string_of_val v)
  | _ -> failwith "Not a value, cannot make string"

let rec step expr =  
  match expr with
  | Binop (op, lhs, rhs) when is_value lhs && is_value rhs -> step_binop op lhs rhs
  | Binop (op, lhs, rhs) when is_value lhs -> Binop (op, lhs, step rhs)
  | Binop (op, lhs, rhs) -> Binop (op, step lhs, rhs) 
  | Def (id, expr) when is_value expr -> Def (id, expr)
  | Def (id, expr) -> Def (id, step expr)
  | Integer v -> Integer v
  | String str -> String str

and step_binop op lhs rhs = 
  match op, lhs, rhs with
  | Plus, Integer l, Integer r -> Integer (l + r)
  | Minus, Integer l, Integer r -> Integer (l - r)
  | Multiply, Integer l, Integer r -> Integer (l * r)
  | _ -> failwith "Type-error in plusexpr"



let rec eval e =
  if is_value e then e
  else e |> step |> eval
