open Expressions
open Pprint

let apply_builtin expr =
  match expr with
  | Invoke (Identifier "println", args) ->
      Printf.printf "%s\n" (string_of_expr_list args);
      Unit
  | _ -> failwith "No builtin function found"
