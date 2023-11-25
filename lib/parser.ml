open Printf
open Expressions
open Tokens
open Lexer

let eat expected remaining =
  let open Lexer in
  let (token, remaining) = next_token remaining in
  match token with
  | Token (tk, _) when tk = expected -> remaining
  | _ -> 
    let (row, col) = get_row_col token in
    let err_msg = sprintf "Expected token %s, found token %s at %d, %d\n" (string_of_token_type expected) (string_of_token token) row col in
    raise (Failure err_msg)

let rec do_parse chars =
  let (token, remaining) = next_token chars in
  match token with 
  | Token (LPAREN, _) -> do_parse remaining
  | Token ((PLUS | MINUS | MULTIPLY) as tk, _) -> parse_math_expr tk remaining
  | Token (DEF, _) -> parse_def_expr remaining
  | Token (FN, _) -> parse_fn_expr remaining
  | Token (INTEGER_TOKEN v, _) -> (Integer v, remaining)
  | Token (STRING_TOKEN str, _) -> (String str, remaining)
  | Token (IDENTIFIER_TOKEN id, _) -> (Identifier id, remaining)
  | Token (UNKNOWN c, Position (row, col)) -> let err_msg = sprintf "Found %s at %d, %d\n" (string_of_token_type (UNKNOWN c)) row col in failwith err_msg
  | tk -> failwith (sprintf "Unexpected token %s" (string_of_token tk))

and parse_math_expr resolve chars =
  let (lhs, chars) = do_parse chars in
  let (rhs, chars) = do_parse chars in  
  let chars = eat RPAREN chars in
  match resolve with
  | PLUS -> (Binop (Plus, lhs, rhs), chars)
  | MINUS -> (Binop (Minus, lhs, rhs), chars)
  | MULTIPLY -> (Binop (Multiply, lhs, rhs), chars)
  | _ -> raise (Failure "Unexpected token in l0 expr")

and parse_def_expr chars =
  let (id, chars) = 
    match (next_token chars) with 
    | (Token (IDENTIFIER_TOKEN id, _), chars) -> (id, chars) 
    | tk -> failwith (sprintf "Expected Identifier, found %s " (string_of_token (fst tk))) in
  let (expr, chars) = do_parse chars in
  let chars = eat RPAREN chars in 
  (Def (id, expr), chars)

and parse_fn_expr chars =
  let (param, chars) = next_token chars in
  let chars = eat ARROW chars in
  let (expr, chars) = do_parse chars in
  let chars = eat RPAREN chars in
    match param with 
    | Token (IDENTIFIER_TOKEN id, _) -> (Fn ([id], expr), chars)
    | _ -> failwith (sprintf "Expected parameter list, found %s" (string_of_token param))
  


let parse chars =
  let (ast, rem) = do_parse chars in 
  let _ = eat EOF rem in 
  ast