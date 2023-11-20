open Expressions
open Tokens

let eat expected remaining =
  let open Lexer in
  let (token, remaining) = next_token remaining in
  match token with
  | Token (tk, _) when tk = expected -> remaining
  | _ -> 
    let (row, col) = get_row_col token in
    let err_msg = Printf.sprintf "Expected token %s, found token %s at %d, %d\n" (string_of_token_type expected) (string_of_token token) row col in
    raise (Failure err_msg)

let rec do_parse chars =
  let open Lexer in
  let (token, remaining) = next_token chars in
  match token with 
  | Token (PLUS, _) -> parse_l0_expr PLUS remaining
  | Token (MINUS, _) -> parse_l0_expr MINUS remaining
  | Token (INTEGER_TOKEN v, _) -> (Integer v, remaining)
  | Token (UNKNOWN c, Position (row, col)) -> let err_msg = Printf.sprintf "Found %s at %d, %d\n" (string_of_token_type (UNKNOWN c)) row col in raise (Failure err_msg)
  | _ -> do_parse remaining

and parse_l0_expr resolve chars =
  let (lhs, initial_remaining) = do_parse chars in
  let (rhs, intermediate_remaining) = do_parse initial_remaining in
  let final_remaining = eat RPAREN intermediate_remaining in
  match resolve with
  | PLUS -> (PlusExpr (lhs, rhs), final_remaining)
  | MINUS -> (MinusExpr (lhs, rhs), final_remaining)
  | _ -> raise (Failure "Unexpected token in l0 expr")
  
let parse chars =
  let (ast, rem) = do_parse chars in 
  let _ = eat EOF rem in 
  ast