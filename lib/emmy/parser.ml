open Base
open Expressions
open Tokens
open Lexer
open Pprint

let current_namespace : string ref = ref ""

let eat expected chars =
  let token, chars = next_token chars in
  match token with
  | { kind; _ } when Poly.(kind = expected) -> chars
  | _ ->
      let row, col = get_row_col token in
      let err_msg =
        Printf.sprintf "Expected token %s, found token %s at %d, %d\n"
          (string_of_token_kind expected)
          (string_of_token token) row col
      in
      raise (Failure err_msg)

let rec do_parse_expr chars =
  let token, chars = next_token chars in
  match token with
  | { kind = LPAREN; pos } ->
      let node, chars =
        match peek chars with
        | Some DEF ->
            let chars = eat DEF chars in
            parse_def_expr chars pos
        | Some PLUS ->
            let chars = eat PLUS chars in
            parse_binop_expr Plus chars pos
        | Some MINUS ->
            let chars = eat MINUS chars in
            parse_binop_expr Minus chars pos
        | Some TIMES ->
            let chars = eat TIMES chars in
            parse_binop_expr Times chars pos
        | Some DIVISION ->
            let chars = eat DIVISION chars in
            parse_binop_expr Division chars pos
        | Some MOD ->
            let chars = eat MOD chars in
            parse_binop_expr Mod chars pos
        | Some EQUALS ->
            let chars = eat EQUALS chars in
            parse_binop_expr Equals chars pos
        | Some LESS_THAN ->
            let chars = eat LESS_THAN chars in
            parse_binop_expr LessThan chars pos
        | Some COND ->
            let chars = eat COND chars in
            parse_cond_expr chars pos
        | Some LET ->
            let chars = eat LET chars in
            parse_let_expr chars pos
        | Some DO ->
            let chars = eat DO chars in
            parse_do_expr chars pos
        | Some FN ->
            let chars = eat FN chars in
            parse_fn_expr chars pos
        | _ -> parse_invoke_expr chars pos
      in
      let chars = eat RPAREN chars in
      (node, chars)
  | { kind = LBRACKET; pos } -> parse_list_ds chars pos
  | { kind = TRUE; pos } ->
      ({ expression = True; pos; namespace = !current_namespace }, chars)
  | { kind = FALSE; pos } ->
      ({ expression = False; pos; namespace = !current_namespace }, chars)
  | { kind = NUMBER_TOKEN v; pos } ->
      ({ expression = Number v; pos; namespace = !current_namespace }, chars)
  | { kind = STRING_TOKEN str; pos } ->
      ({ expression = String str; pos; namespace = !current_namespace }, chars)
  | { kind = IDENTIFIER_TOKEN id; pos } ->
      ( { expression = Identifier id; pos; namespace = !current_namespace },
        chars )
  | { kind = UNKNOWN c; pos } ->
      let err_msg =
        Printf.sprintf "Found %s at %d, %d\n"
          (string_of_token_kind (UNKNOWN c))
          pos.row pos.col
      in
      failwith err_msg
  | tk -> failwith (Printf.sprintf "Unexpected token %s " (string_of_token tk))

and aux_parse_list_ds nodes chars =
  match peek chars with
  | Some RBRACKET ->
      let chars = eat RBRACKET chars in
      (List.rev nodes, chars)
  | _ ->
      let node, chars = do_parse_expr chars in
      aux_parse_list_ds (node :: nodes) chars

and parse_list_ds chars pos =
  let nodes, chars = aux_parse_list_ds [] chars in
  ({ expression = List nodes; pos; namespace = !current_namespace }, chars)

and parse_def_expr chars pos =
  let id, chars =
    match next_token chars with
    | { kind = IDENTIFIER_TOKEN id; _ }, chars -> (id, chars)
    | tk ->
        failwith
          (Printf.sprintf "Expected Identifier, found %s "
             (string_of_token (fst tk)))
  in
  let node, chars = do_parse_expr chars in
  ({ expression = Def (id, node); pos; namespace = !current_namespace }, chars)

and parse_binop_expr op chars pos =
  let lhs, chars = do_parse_expr chars in
  let rhs, chars = do_parse_expr chars in
  ( { expression = Binop (op, lhs, rhs); pos; namespace = !current_namespace },
    chars )

and parse_let_expr chars pos =
  let chars = eat LBRACKET chars in
  let rec parse_let_aux bindings chars =
    let id, chars = do_parse_expr chars in
    let id_str =
      match id with
      | { expression = Identifier id; _ } -> id
      | _ ->
          failwith
            (Printf.sprintf "lhs in let-binding must be identifier, found %s."
               (Pprint.string_of_expr id.expression))
    in
    let node, chars = do_parse_expr chars in
    if Poly.(peek chars = Some RBRACKET) then
      let chars = eat RBRACKET chars in
      ((id_str, node) :: bindings, chars)
    else parse_let_aux ((id_str, node) :: bindings) chars
  in
  let bindings, chars = parse_let_aux [] chars in
  let node, chars = do_parse_expr chars in
  ( {
      expression = LetBinding (bindings, node);
      pos;
      namespace = !current_namespace;
    },
    chars )

and parse_do_expr chars pos =
  let unit_node, chars = do_parse_expr chars in
  let actual_node, chars = do_parse_expr chars in
  ( {
      expression = Do (unit_node, actual_node);
      pos;
      namespace = !current_namespace;
    },
    chars )

and parse_cond_expr chars pos =
  let rec parse_cond_aux nodes chars =
    let case, chars = do_parse_expr chars in
    if Poly.(peek chars = Some RPAREN) then
      if List.length nodes % 2 = 1 then
        failwith "Must provide default case for cond-expression"
      else
        ( {
            expression = Cond (List.rev nodes, case);
            pos;
            namespace = !current_namespace;
          },
          chars )
    else
      let node, chars = do_parse_expr chars in
      if Poly.(peek chars = Some RPAREN) then
        failwith "Must provide default case for cond-expression"
      else parse_cond_aux (node :: case :: nodes) chars
  in
  parse_cond_aux [] chars

and parse_fn_expr chars pos =
  let rec get_params_aux params_acc chars =
    match peek chars with
    | Some (IDENTIFIER_TOKEN _) -> (
        let param, chars = next_token chars in
        match param with
        | { kind = IDENTIFIER_TOKEN id; _ } ->
            get_params_aux (id :: params_acc) chars
        | _ ->
            failwith
              "Peek said Identifier, Next gave something else in parse_fn_expr."
        )
    | _ -> (List.rev params_acc, chars)
  in
  let params, chars = get_params_aux [] chars in
  let chars = eat ARROW chars in
  let body, chars = do_parse_expr chars in
  ( { expression = Fn (params, body); pos; namespace = !current_namespace },
    chars )

and parse_invoke_expr chars pos =
  let to_apply, chars = do_parse_expr chars in
  let args, chars = aux_parse_arg_list [] chars in
  ( { expression = Invoke (to_apply, args); pos; namespace = !current_namespace },
    chars )

and aux_parse_arg_list args chars =
  match peek chars with
  | Some RPAREN ->
      (List.rev args, chars)
      (* we want to keep parsing the arguments until we hit the closing bracket of the function invocation. *)
  | _ ->
      let arg, chars = do_parse_expr chars in
      aux_parse_arg_list (arg :: args) chars

let rec parse_namespace chars =
  let chars = eat LPAREN chars in
  let chars = eat NAMESPACE chars in
  let name, chars = next_token chars in
  let requires, chars = parse_requires chars in

  let chars = eat RPAREN chars in
  let rec parse_exprs_aux nodes chars =
    match peek chars with
    | Some _tok ->
        let node, rem = do_parse_expr chars in
        parse_exprs_aux (node :: nodes) rem
    | None ->
        let _ = eat EOF chars in
        List.rev nodes
  in
  match name with
  | { kind = IDENTIFIER_TOKEN ns; _ } ->
      current_namespace := ns;
      Namespace (ns, requires, parse_exprs_aux [] chars)
  | t ->
      let err_msg =
        Printf.sprintf "Found %s when parsing namespace, expected identifier.\n"
          (string_of_token t)
      in
      failwith err_msg

and parse_requires chars =
  match peek chars with
  | Some LPAREN ->
      let chars = eat LPAREN chars in
      let chars = eat REQUIRES chars in
      let rec get_requires_aux requires chars =
        let token, chars = next_token chars in
        match token with
        | { kind = IDENTIFIER_TOKEN filename; _ } ->
            get_requires_aux (filename :: requires) chars
        | { kind = RPAREN; _ } -> (Some (List.rev requires), chars)
        | _ -> (None, chars)
      in
      get_requires_aux [] chars
  | _ -> (None, chars)

and parse_namespaces chars =
  let dependency_graph = Hashtbl.create (module String) in
  let rec parse_namespaces_aux chars =
    let namespace = parse_namespace chars in
    match namespace with
    | Namespace (_, None, _) -> [ namespace ]
    | Namespace (name, Some reqs, _) ->
        let _ =
          match Hashtbl.find dependency_graph name with
          | None -> Hashtbl.add dependency_graph ~key:name ~data:reqs
          | Some _ ->
              raise
                (Failure
                   (Printf.sprintf "Cyclic dependency to '%s':\n%s" name
                      (string_of_dependency_graph dependency_graph)))
        in
        List.fold
          ~f:(fun namespaces req ->
            let chars = Io.get_char_stream ~namespace:req in
            parse_namespaces_aux chars @ namespaces)
          ~init:[ namespace ] reqs
  in
  parse_namespaces_aux chars

let parse chars =
  let namespaces = parse_namespaces chars in
  Stdio.printf "%s\n" (string_of_program (Program namespaces));
  Program namespaces
