open Base
open Tokens

type ast_node = { expression : expression; pos : position; namespace : string }
and node_position = { row : int; col : int }
and program = Program of namespace list
and namespace = Namespace of string * string list option * ast_node list

and expression =
  | Def of string * ast_node
  | Fn of string list * ast_node
  (* the expression list is the list of arguments. The evaluation strategy is eager. Call-by-value *)
  | Invoke of ast_node * ast_node list
  | LetBinding of (string * ast_node) list * ast_node
  | Cond of ast_node list * ast_node
  | Do of ast_node * ast_node
  | Binop of binop * ast_node * ast_node
  | List of ast_node list
  | Number of float
  | String of string
  | Identifier of string
  | True
  | False
  | Unit

and binop = Plus | Minus | Times | Division | Mod | Equals | LessThan

(* Also called a 'normal form' *)
let rec is_value expr =
  match expr.expression with
  | Number _ | String _ | True | False | Unit | Fn (_, _) -> true
  | List exprs when is_list_of_values exprs -> true
  | _ -> false

and is_list_of_values exprs = List.for_all ~f:(fun m -> is_value m) exprs
