open Printf
open Expressions
open Tokens
open Lexer

let eat expected chars =
  let (token, chars) = next_token chars in
  match token with
  | Token (tk, _) when tk = expected -> chars
  | _ -> 
    let (row, col) = get_row_col token in
    let err_msg = sprintf "Expected token %s, found token %s at %d, %d\n" (string_of_token_type expected) (string_of_token token) row col in
    raise (Failure err_msg)

let rec do_parse chars =
  let (token, chars) = next_token chars in
  match token with 
  | Token (LPAREN, _) -> 
   let (expr, chars) = 
      (match peek chars with
        | DEF -> let chars = eat DEF chars in parse_def_expr chars
        | FN -> let chars = eat FN chars in parse_fn_expr chars
        | _ -> parse_fn_invoke_expr chars) in
    let chars = eat RPAREN chars in 
    (expr, chars)
  | Token (INTEGER_TOKEN v, _) -> (Integer v, chars)
  | Token (STRING_TOKEN str, _) -> (String str, chars)
  | Token (IDENTIFIER_TOKEN id, _) -> (Identifier id, chars)
  | Token (UNKNOWN c, Position (row, col)) -> let err_msg = sprintf "Found %s at %d, %d\n" (string_of_token_type (UNKNOWN c)) row col in failwith err_msg
  | tk -> failwith (sprintf "Unexpected token %s" (string_of_token tk))

and parse_def_expr chars =
  let (id, chars) = 
    match (next_token chars) with 
    | (Token (IDENTIFIER_TOKEN id, _), chars) -> (id, chars) 
    | tk -> failwith (sprintf "Expected Identifier, found %s " (string_of_token (fst tk))) in
  let (expr, chars) = do_parse chars in
  (Def (id, expr), chars)

and parse_fn_expr chars =
  let (param, chars) = next_token chars in
  let chars = eat ARROW chars in
  let (expr, chars) = do_parse chars in
    match param with 
    | Token (IDENTIFIER_TOKEN id, _) -> (Fn ([id], expr), chars)
    | _ -> failwith (sprintf "Expected parameter list, found %s" (string_of_token param))
and parse_fn_invoke_expr chars =
  let (to_apply, chars) = do_parse chars in
  let (args, chars) = do_parse chars in
  (FnInvoke (to_apply, [args]), chars)
  
  


let parse chars =
  let (ast, rem) = do_parse chars in 
  let _ = eat EOF rem in 
  ast