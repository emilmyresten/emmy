open Printf
open Expressions

let rec string_of_val v = 
  match v with
  | Integer v -> sprintf "Integer %d" v
  | String str -> sprintf "String %s" str
  | True -> sprintf "true"
  | False -> sprintf "false"
  | Unit -> "Unit"
  | Fn (params, expr) -> sprintf "(Function %s-> %s)" (string_of_params params) (string_of_expr expr) 
  | Identifier id -> id
  | _ -> failwith (sprintf "Expected value, found %s" (string_of_expr v))
and string_of_params params = (List.fold_left (fun acc m -> acc ^ m ^ " ") "" params)
and string_of_args args = (List.fold_left (fun acc m -> acc ^ string_of_expr m ^ " ") "" args)
and string_of_cond_cases exprs = (List.fold_left (fun acc m -> acc ^ string_of_expr m ^ " ") "" exprs)
and string_of_binop binop = match binop with
  | (Plus, lhs, rhs) -> sprintf "(+ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | (Minus, lhs, rhs) -> sprintf "(- %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | (Times, lhs, rhs) -> sprintf "(* %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | (Division, lhs, rhs) -> sprintf "(/ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | (Equals, lhs, rhs) -> sprintf "(= %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | (LessThan, lhs, rhs) -> sprintf "(< %s %s)" (string_of_expr lhs) (string_of_expr rhs)
and string_of_expr expr = match expr with
  | Def (id, expr) ->  sprintf "(def %s %s)" id (string_of_expr expr)
  | Identifier id -> sprintf "Identifier %s" id
  | FnInvoke (to_apply, args) -> sprintf "%s %s" (string_of_expr to_apply) (string_of_args args)
  | Cond (exprs, default) -> sprintf "(cond %s %s)" (string_of_cond_cases exprs) (string_of_expr default)
  | Binop (op, lhs, rhs) -> string_of_binop (op, lhs, rhs)
  | Fn (params, expr) -> string_of_val (Fn (params, expr))
  | Integer _ | String _ | True | False | Unit -> string_of_val (expr)

and string_of_context ctx = List.fold_left (fun a (k, v) -> a ^ (sprintf "%s = %s\n" k (string_of_val v))) "" ctx