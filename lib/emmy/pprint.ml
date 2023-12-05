open Printf
open Expressions

let rec string_of_val v =
  match v with
  | Integer v -> sprintf "%d" v
  | String str -> sprintf "\"%s\"" str
  | True -> sprintf "true"
  | False -> sprintf "false"
  | Unit -> "Unit"
  | Fn (params, expr) ->
      sprintf "(function %s -> %s)" (string_of_params params)
        (string_of_expr expr)
  | Identifier id -> id
  | _ -> failwith (sprintf "Expected value, found %s" (string_of_expr v))

and string_of_params params = String.concat " " params

and string_of_expr_list exprs =
  List.map (fun m -> string_of_expr m) exprs |> String.concat " "

and string_of_bindings bindings =
  List.map (fun (id, expr) -> id ^ " " ^ string_of_expr expr) bindings
  |> String.concat " "

and string_of_binop binop =
  match binop with
  | Plus, lhs, rhs ->
      sprintf "(+ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Minus, lhs, rhs ->
      sprintf "(- %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Times, lhs, rhs ->
      sprintf "(* %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Division, lhs, rhs ->
      sprintf "(/ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Mod, lhs, rhs ->
      sprintf "(%% %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Equals, lhs, rhs ->
      sprintf "(= %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | LessThan, lhs, rhs ->
      sprintf "(< %s %s)" (string_of_expr lhs) (string_of_expr rhs)

and string_of_expr expr =
  match expr with
  | Def (id, expr) -> sprintf "(def %s %s)" id (string_of_expr expr)
  | Identifier id -> sprintf "Identifier %s" id
  | Invoke (to_apply, args) ->
      sprintf "%s %s" (string_of_expr to_apply) (string_of_expr_list args)
  | Cond (exprs, default) ->
      sprintf "(cond %s %s)"
        (string_of_expr_list exprs)
        (string_of_expr default)
  | LetBinding (bindings, expr) ->
      sprintf "let (%s) %s" (string_of_bindings bindings) (string_of_expr expr)
  | Binop (op, lhs, rhs) -> string_of_binop (op, lhs, rhs)
  | Fn (params, expr) -> string_of_val (Fn (params, expr))
  | List exprs -> sprintf "[%s]" (string_of_expr_list exprs)
  | Integer _ | String _ | True | False | Unit -> string_of_val expr

and string_of_context ctx =
  List.map (fun (k, v) -> sprintf "%s = %s\n" k (string_of_val v)) ctx
  |> String.concat ""
