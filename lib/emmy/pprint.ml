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
        (string_of_expr expr.expression)
  | Identifier id -> id
  | _ -> failwith (Printf.sprintf "Expected value, found %s" (string_of_expr v))

and string_of_params params = String.concat ~sep:" " params

and string_of_expr_list exprs =
  List.map ~f:(fun m -> string_of_expr m) exprs |> String.concat ~sep:" "

and string_of_bindings bindings =
  List.map
    ~f:(fun (id, expr) -> id ^ " " ^ string_of_expr expr.expression)
    bindings
  |> String.concat ~sep:" "

and string_of_binop binop =
  match binop with
  | Plus, lhs, rhs ->
      Printf.sprintf "(+ %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | Minus, lhs, rhs ->
      Printf.sprintf "(- %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | Times, lhs, rhs ->
      Printf.sprintf "(* %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | Division, lhs, rhs ->
      Printf.sprintf "(/ %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | Mod, lhs, rhs ->
      Printf.sprintf "(%% %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | Equals, lhs, rhs ->
      Printf.sprintf "(= %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)
  | LessThan, lhs, rhs ->
      Printf.sprintf "(< %s %s)"
        (string_of_expr lhs.expression)
        (string_of_expr rhs.expression)

and string_of_expr expr =
  match expr with
  | Def (id, expr) ->
      Printf.sprintf "(def %s %s)\n" id (string_of_expr expr.expression)
  | Identifier id -> Printf.sprintf "Identifier %s" id
  | Invoke (to_apply, args) ->
      Printf.sprintf "%s %s"
        (string_of_expr to_apply.expression)
        (List.map ~f:(fun expr -> expr.expression) args |> string_of_expr_list)
  | Cond (exprs, default) ->
      Printf.sprintf "(cond %s %s)"
        (List.map ~f:(fun expr -> expr.expression) exprs |> string_of_expr_list)
        (string_of_expr default.expression)
  | LetBinding (bindings, expr) ->
      Printf.sprintf "(let [%s] %s)"
        (string_of_bindings bindings)
        (string_of_expr expr.expression)
  | Do (unit_expr, actual_expr) ->
      Printf.sprintf "(do (%s) %s)"
        (string_of_expr unit_expr.expression)
        (string_of_expr actual_expr.expression)
  | Binop (op, lhs, rhs) -> string_of_binop (op, lhs, rhs)
  | List exprs ->
      Printf.sprintf "[%s]"
        (List.map ~f:(fun expr -> expr.expression) exprs |> string_of_expr_list)
  | Fn (_, _) | Number _ | String _ | True | False | Unit -> string_of_val expr

and string_of_context ctx =
  List.map
    ~f:(fun (k, v) ->
      Printf.sprintf "%s = %s\n" k (string_of_expr v.expression))
    ctx
  |> String.concat ~sep:""

and string_of_namespace namespace =
  match namespace with
  | Namespace (name, requires, exprs) ->
      Printf.sprintf "(namespace %s" name
      ^ (match requires with
        | Some reqs -> "\n\t(requires " ^ String.concat ~sep:" " reqs
        | None -> "")
      ^ ")\n"
      ^ (List.map ~f:(fun expr -> expr.expression) exprs |> string_of_expr_list)

and string_of_program program =
  match program with
  | Program namespaces ->
      List.fold
        ~f:(fun acc ns -> acc ^ string_of_namespace ns)
        ~init:"" namespaces

let string_of_dependency_graph graph =
  Hashtbl.fold graph ~init:"" ~f:(fun ~key ~data acc ->
      let deps =
        List.fold ~init:"{\n"
          ~f:(fun acc dependency -> acc ^ "\t" ^ dependency ^ ",\n")
          data
        ^ "}"
      in
      acc ^ key ^ ": " ^ deps ^ ",\n")
