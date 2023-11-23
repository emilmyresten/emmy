open Expressions

let string_of_val e = match e with
  | Integer v -> Printf.sprintf "Integer %d" v
  | String str -> Printf.sprintf "String %s" str
  | Unit -> Printf.sprintf "Unit value"
  | _ -> failwith "Not a value, cannot make string"

let print_context ctx = List.iter (fun (k, v) -> Printf.printf "%s: %s\n" k (string_of_val v)) ctx

let rec step ctx expr =  
  match expr with
  | Binop (op, lhs, rhs) when is_value lhs && is_value rhs -> (step_binop op lhs rhs, ctx)
  | Binop (op, lhs, rhs) when is_value lhs -> (Binop (op, lhs, fst (step ctx rhs)), ctx)
  | Binop (op, lhs, rhs) -> (Binop (op, fst (step ctx lhs), rhs), ctx) 
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) -> let (stepped, new_ctx) = step ctx expr in (Def (id, stepped), new_ctx)
  | Integer v -> (Integer v, ctx)
  | String str -> (String str, ctx)
  | Identifier id ->
    let value = snd (List.find (fun (k, _) -> k = id) ctx) in 
    (value, ctx)
  | Unit -> failwith "Should encounter unit when Parsing."


and step_binop op lhs rhs = 
  match op, lhs, rhs with
  | Plus, Integer l, Integer r -> Integer (l + r)
  | Minus, Integer l, Integer r -> Integer (l - r)
  | Multiply, Integer l, Integer r -> Integer (l * r)
  | _ -> failwith "Type-error in plusexpr"



let rec eval e ctx =
  if is_value e then (e, ctx)
  else let (stepped, ctx) = step ctx e in eval stepped ctx
