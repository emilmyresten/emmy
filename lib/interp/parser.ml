open Printf
open Expressions
open Tokens
open Lexer

let eat expected chars =
  let token, chars = next_token chars in
  match token with
  | Token (tk, _) when tk = expected -> chars
  | _ ->
      let row, col = get_row_col token in
      let err_msg =
        sprintf "Expected token %s, found token %s at %d, %d\n"
          (string_of_token_type expected)
          (string_of_token token) row col
      in
      raise (Failure err_msg)

let rec do_parse chars =
  let token, chars = next_token chars in
  match token with
  | Token (LPAREN, _) ->
      let expr, chars =
        match peek chars with
        | DEF ->
            let chars = eat DEF chars in
            parse_def_expr chars
        | PLUS ->
            let chars = eat PLUS chars in
            parse_binop_expr Plus chars
        | MINUS ->
            let chars = eat MINUS chars in
            parse_binop_expr Minus chars
        | TIMES ->
            let chars = eat TIMES chars in
            parse_binop_expr Times chars
        | DIVISION ->
            let chars = eat DIVISION chars in
            parse_binop_expr Division chars
        | EQUALS ->
            let chars = eat EQUALS chars in
            parse_binop_expr Equals chars
        | LESS_THAN ->
            let chars = eat LESS_THAN chars in
            parse_binop_expr LessThan chars
        | COND ->
            let chars = eat COND chars in
            parse_cond_expr chars
        | FN ->
            let chars = eat FN chars in
            parse_fn_expr chars
        | _ -> parse_fn_invoke_expr chars
      in
      let chars = eat RPAREN chars in
      (expr, chars)
  | Token (TRUE, _) -> (True, chars)
  | Token (FALSE, _) -> (False, chars)
  | Token (INTEGER_TOKEN v, _) -> (Integer v, chars)
  | Token (STRING_TOKEN str, _) -> (String str, chars)
  | Token (IDENTIFIER_TOKEN id, _) -> (Identifier id, chars)
  | Token (UNKNOWN c, Position (row, col)) ->
      let err_msg =
        sprintf "Found %s at %d, %d\n"
          (string_of_token_type (UNKNOWN c))
          row col
      in
      failwith err_msg
  | tk -> failwith (sprintf "Unexpected token %s" (string_of_token tk))

and parse_def_expr chars =
  let id, chars =
    match next_token chars with
    | Token (IDENTIFIER_TOKEN id, _), chars -> (id, chars)
    | tk ->
        failwith
          (sprintf "Expected Identifier, found %s " (string_of_token (fst tk)))
  in
  let expr, chars = do_parse chars in
  (Def (id, expr), chars)

and parse_binop_expr op chars =
  let lhs, chars = do_parse chars in
  let rhs, chars = do_parse chars in
  (Binop (op, lhs, rhs), chars)

and parse_cond_expr chars =
  let rec parse_cond_aux exprs chars =
    let case, chars = do_parse chars in
    if peek chars = RPAREN then
      if List.length exprs mod 2 = 1 then
        failwith "Must provide default case for cond-expression"
      else (Cond (List.rev exprs, case), chars)
    else
      let expr, chars = do_parse chars in
      if peek chars = RPAREN then
        failwith "Must provide default case for cond-expression"
      else parse_cond_aux (expr :: case :: exprs) chars
  in
  parse_cond_aux [] chars

and parse_fn_expr chars =
  let rec get_params_aux params_acc chars =
    match peek chars with
    | IDENTIFIER_TOKEN _ -> (
        let param, chars = next_token chars in
        match param with
        | Token (IDENTIFIER_TOKEN id, _) ->
            get_params_aux (id :: params_acc) chars
        | _ ->
            failwith
              "Peek said Identifier, Next gave something else in parse_fn_expr."
        )
    | _ -> (List.rev params_acc, chars)
  in
  let params, chars = get_params_aux [] chars in
  let chars = eat ARROW chars in
  let expr, chars = do_parse chars in
  (Fn (params, expr), chars)

and parse_fn_invoke_expr chars =
  let rec get_args_aux args_acc chars =
    match peek chars with
    | RPAREN ->
        (List.rev args_acc, chars)
        (* we want to keep parsing the arguments until we hit the closing bracket of the function invocation. *)
    | _ ->
        let arg, chars = do_parse chars in
        get_args_aux (arg :: args_acc) chars
  in
  let to_apply, chars = do_parse chars in
  let args, chars = get_args_aux [] chars in
  (FnInvoke (to_apply, args), chars)

let parse chars =
  let ast, rem = do_parse chars in
  let _ = eat EOF rem in
  ast
