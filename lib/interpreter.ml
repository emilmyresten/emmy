open Printf
open Expressions

let rec string_of_val v = match v with
  | Integer v -> sprintf "Integer %d" v
  | String str -> sprintf "String %s" str
  | Unit -> sprintf "Unit"
  | Fn (params, expr) -> sprintf "Function %s-> %s" (string_of_params params) (string_of_val expr) 
  | Identifier id -> id
  | _ -> failwith (sprintf "Expected value, found %s" (string_of_expr v))
and string_of_params params = (List.fold_left (fun acc m -> acc ^ m ^ " ") "" params)
and string_of_expr e = match e with
  | Binop (_, lhs, rhs) -> sprintf "(Binop %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Def (id, expr) ->  sprintf "(def %s %s)" id (string_of_expr expr)
  | Identifier id -> sprintf "Identifier %s" id
  (* | Fn (params, expr) when not (is_value expr) -> sprintf "(fn %s -> %s)" (string_of_params params) (string_of_expr expr) *)
  | v -> string_of_val v

let print_context ctx = List.iter (fun (k, v) -> printf "%s = %s\n" k (string_of_val v)) ctx

let get_from_ctx id ctx = (snd (List.find (fun (k, _) -> k = id) ctx))

let rec step ctx expr =  
  match expr with
  | Binop (op, lhs, rhs) when is_value lhs && is_value rhs -> (step_binop op lhs rhs, ctx)
  | Binop (op, lhs, rhs) when is_value lhs -> (Binop (op, lhs, fst (step ctx rhs)), ctx)
  | Binop (op, lhs, rhs) -> (Binop (op, fst (step ctx lhs), rhs), ctx) 
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) -> let (stepped, new_ctx) = step ctx expr in (Def (id, stepped), new_ctx)
  | Fn (params, expr) when is_value expr -> (Fn (params, expr), ctx)
  | Fn (params, expr) -> (Fn (params, fst (step ctx expr)), ctx)
  | Integer v -> (Integer v, ctx)
  | String str -> (String str, ctx)
  | Identifier id ->
    let value = try get_from_ctx id ctx with _ -> failwith (sprintf "Unbound identifier %s" id) in 
    (value, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."


and step_binop op lhs rhs = 
  match op, lhs, rhs with
  | Plus, Integer l, Integer r -> Integer (l + r)
  | Minus, Integer l, Integer r -> Integer (l - r)
  | Multiply, Integer l, Integer r -> Integer (l * r)
  | _ -> failwith "Type-error in plusexpr"



let rec eval e ctx =
  if is_value e then (e, ctx)
  else let (stepped, ctx) = step ctx e in eval stepped ctx
