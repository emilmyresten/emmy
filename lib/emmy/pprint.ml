open Base
open Expressions

let rec string_of_val v =
  match v with
  | Number v ->
      if Float.is_integer v then Int.to_string (Float.to_int v)
      else Float.to_string v
  | String str -> Printf.sprintf "\"%s\"" str
  | True -> "true"
  | False -> "false"
  | Unit -> "Unit"
  | Fn (params, expr) ->
      Printf.sprintf "(function %s -> %s)" (string_of_params params)
        (string_of_expr expr)
  | Identifier id -> id
  | _ -> failwith (Printf.sprintf "Expected value, found %s" (string_of_expr v))

and string_of_params params = String.concat ~sep:" " params

and string_of_expr_list exprs =
  List.map ~f:(fun m -> string_of_expr m) exprs |> String.concat ~sep:" "

and string_of_bindings bindings =
  List.map ~f:(fun (id, expr) -> id ^ " " ^ string_of_expr expr) bindings
  |> String.concat ~sep:" "

and string_of_binop binop =
  match binop with
  | Plus, lhs, rhs ->
      Printf.sprintf "(+ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Minus, lhs, rhs ->
      Printf.sprintf "(- %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Times, lhs, rhs ->
      Printf.sprintf "(* %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Division, lhs, rhs ->
      Printf.sprintf "(/ %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Mod, lhs, rhs ->
      Printf.sprintf "(%% %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | Equals, lhs, rhs ->
      Printf.sprintf "(= %s %s)" (string_of_expr lhs) (string_of_expr rhs)
  | LessThan, lhs, rhs ->
      Printf.sprintf "(< %s %s)" (string_of_expr lhs) (string_of_expr rhs)

and string_of_expr expr =
  match expr with
  | Def (id, expr) -> Printf.sprintf "(def %s %s)" id (string_of_expr expr)
  | Identifier id -> Printf.sprintf "Identifier %s" id
  | Invoke (to_apply, args) ->
      Printf.sprintf "%s %s" (string_of_expr to_apply)
        (string_of_expr_list args)
  | Cond (exprs, default) ->
      Printf.sprintf "(cond %s %s)"
        (string_of_expr_list exprs)
        (string_of_expr default)
  | LetBinding (bindings, expr) ->
      Printf.sprintf "(let [%s] %s)"
        (string_of_bindings bindings)
        (string_of_expr expr)
  | Do (unit_expr, actual_expr) ->
      Printf.sprintf "(do (%s) %s)" (string_of_expr unit_expr)
        (string_of_expr actual_expr)
  | Binop (op, lhs, rhs) -> string_of_binop (op, lhs, rhs)
  | List exprs -> Printf.sprintf "[%s]" (string_of_expr_list exprs)
  | Fn (_, _) | Number _ | String _ | True | False | Unit -> string_of_val expr

and string_of_context ctx =
  List.map
    ~f:(fun (k, v) -> Printf.sprintf "%s = %s\n" k (string_of_expr v))
    ctx
  |> String.concat ~sep:""

and string_of_namespace namespace =
  match namespace with
  | Namespace (name, requires, exprs) ->
      Printf.sprintf "(namespace %s" name
      ^ (match requires with
        | Some reqs -> "\n\t(requires " ^ String.concat ~sep:" " reqs
        | None -> "")
      ^ ")\n" ^ string_of_expr_list exprs

and string_of_program program =
  match program with
  | Program namespaces ->
      List.fold
        ~f:(fun acc ns -> acc ^ string_of_namespace ns)
        ~init:"" namespaces
