open Base
open Pprint
open Extensions
open Expressions
open Builtins
open Utils

let () = Random.init 0

let get_from_ctx id ctx =
  match List.Assoc.find ctx ~equal:String.equal id with
  | Some value -> value
  | None ->
      failwith
        (Printf.sprintf "Unbound identifier %s in context [%s]" id
           (string_of_context ctx))

let rec get_unique_name p bindings =
  let suffix = Random.int 512 in
  let name_attempt = p ^ "#" ^ Int.to_string suffix in
  if List.exists bindings ~f:(fun binding -> Poly.(binding = name_attempt)) then
    get_unique_name p bindings
  else name_attempt

let rec beta_reduce param_ids replacements expr =
  match expr.expression with
  | Identifier id ->
      if List.exists param_ids ~f:(fun param_id -> Poly.(param_id = id)) then
        let param_position_opt =
          Stdlib.(List.find_index (fun m -> Poly.(m = id)) param_ids)
        in
        match param_position_opt with
        | Some pos -> (
            match List.nth replacements pos with
            | Some arg -> { expr with expression = arg }
            | None ->
                failwith
                  (Printf.sprintf
                     "Identifier %s is member of the parameter list, but \
                      couldn't find corresponding argument during \
                      beta-reduction."
                     id))
        | None ->
            failwith
              (Printf.sprintf
                 "Identifier %s is member of the parameter list, but couldn't \
                  find index during beta-reduction."
                 id)
      else expr
  | Def (id, expr) ->
      {
        expr with
        expression = Def (id, beta_reduce param_ids replacements expr);
      }
  | Fn (params, expr) ->
      {
        expr with
        expression = Fn (params, beta_reduce param_ids replacements expr);
      }
  | Invoke (to_apply, args) ->
      {
        expr with
        expression =
          Invoke
            (to_apply, List.map ~f:(beta_reduce param_ids replacements) args);
      }
  | Binop (op, lhs, rhs) ->
      {
        expr with
        expression =
          Binop
            ( op,
              beta_reduce param_ids replacements lhs,
              beta_reduce param_ids replacements rhs );
      }
  | List exprs ->
      {
        expr with
        expression =
          List (List.map ~f:(beta_reduce param_ids replacements) exprs);
      }
  | True | False | Number _ | String _ | Unit -> expr
  | Cond (exprs, default) ->
      {
        expr with
        expression =
          Cond
            ( List.map ~f:(beta_reduce param_ids replacements) exprs,
              beta_reduce param_ids replacements default );
      }
  | LetBinding (bindings, expr) ->
      let beta_reduced_bindings =
        List.map
          ~f:(fun (id, expr) -> (id, beta_reduce param_ids replacements expr))
          bindings
      in
      {
        expr with
        expression =
          LetBinding
            (beta_reduced_bindings, beta_reduce param_ids replacements expr);
      }
  | Do (unit_expr, actual_expr) ->
      {
        expr with
        expression =
          Do
            ( beta_reduce param_ids replacements unit_expr,
              beta_reduce param_ids replacements actual_expr );
      }

and alpha_convert scope replacements expr =
  (* [bindings: list of seen bindings.
      replace: look in this associative list/map to find replacement names for the current.]*)
  (* Rename nested identifiers to resolve scope conflicts.
     As soon as bindings occur in two scopes, all subsequent have to be renamed to the binding of the nearest scope. *)
  match expr.expression with
  | Identifier id -> (
      match List.Assoc.find replacements id ~equal:String.equal with
      | Some name -> { expr with expression = Identifier name }
      | None -> expr)
  | Def (_, expr) -> alpha_convert scope replacements expr
  | Fn (params, expr) ->
      let conflicts = List.overlap scope params in
      (* replace these *)
      let bind = List.unique_right scope params in
      (* add these to the seen bindings *)
      let new_replace =
        List.fold
          ~f:(fun a p -> [ (p, get_unique_name p scope) ] @ a)
          ~init:[] conflicts
        |> List.rev
      in
      let new_params = List.map ~f:(fun (_, v) -> v) new_replace @ bind in
      let new_scope = new_params @ bind @ scope in
      {
        expr with
        expression = Fn (new_params, alpha_convert new_scope new_replace expr);
      }
  | Invoke (to_apply, args) ->
      {
        expr with
        expression =
          Invoke
            ( alpha_convert scope replacements to_apply,
              List.map ~f:(alpha_convert scope replacements) args );
      }
  | Binop (op, lhs, rhs) ->
      {
        expr with
        expression =
          Binop
            ( op,
              alpha_convert scope replacements lhs,
              alpha_convert scope replacements rhs );
      }
  | List exprs ->
      {
        expr with
        expression = List (List.map ~f:(alpha_convert scope replacements) exprs);
      }
  | True | False | Number _ | String _ | Unit -> expr
  | Cond (exprs, default) ->
      {
        expr with
        expression =
          Cond
            ( List.map ~f:(alpha_convert scope replacements) exprs,
              alpha_convert scope replacements default );
      }
  | LetBinding (bnds, expr) ->
      let bindings = List.map ~f:(fun (id, _) -> id) bnds in
      let conflicts = List.overlap scope bindings in
      let bind = List.unique_right scope bindings in
      let new_replace =
        List.fold
          ~f:(fun a p -> [ (p, get_unique_name p scope) ] @ a)
          ~init:[] conflicts
      in
      let new_let_bindings = List.map ~f:(fun (_, v) -> v) new_replace @ bind in
      let maybe_new_bnds =
        List.map ~f:(fun (_, v) -> v) bnds |> List.zip new_let_bindings
      in
      let new_bnds =
        match maybe_new_bnds with
        | Ok bnds -> bnds
        | Unequal_lengths -> failwith "trying to zip lists of unequal lengths."
      in
      let new_scope = new_let_bindings @ bind @ scope in
      {
        expr with
        expression =
          LetBinding (new_bnds, alpha_convert new_scope new_replace expr);
      }
  | Do (unit_expr, actual_expr) ->
      {
        expr with
        expression =
          Do
            ( alpha_convert scope replacements unit_expr,
              alpha_convert scope replacements actual_expr );
      }

(* let map_of_params params = List.map (fun p -> (p, Identifier p)) params *)

and step expr ctx =
  match expr.expression with
  | Identifier id ->
      let value = get_from_ctx id ctx in
      (value, ctx)
  | Def (id, expr) when is_value expr ->
      ({ expr with expression = Unit }, (id, expr) :: ctx)
  | Def (id, expr) ->
      let stepped, new_ctx = step expr ctx in
      ({ expr with expression = Def (id, stepped) }, new_ctx)
  | Fn (params, expr) -> ({ expr with expression = Fn (params, expr) }, ctx)
  | Invoke (to_apply, args) when is_value to_apply && is_list_of_values args
    -> (
      match to_apply.expression with
      | Fn (params, expr) ->
          check_arity (List.length params) args;
          let alpha_converted = alpha_convert params [] expr in
          let beta_reduced =
            beta_reduce params
              (List.map ~f:(fun arg -> arg.expression) args)
              alpha_converted
          in
          step beta_reduced ctx
      | List exprs ->
          check_arity 1 args;
          step_list exprs args ctx
      | _ ->
          failwith
            (Printf.sprintf "%s is not a function."
               (string_of_expr to_apply.expression)))
  | Invoke (to_apply, args) when is_list_of_values args -> (
      try (apply_builtin to_apply args ctx, ctx) with
      | Builtin_error e | Arity_exn e -> failwith e
      | _ ->
          ( { expr with expression = Invoke (fst (step to_apply ctx), args) },
            ctx ))
  | Invoke (to_apply, args) ->
      ( {
          expr with
          expression =
            Invoke (to_apply, List.map ~f:(fun m -> fst (step m ctx)) args);
        },
        ctx )
  | Binop (_, lhs, rhs) when is_value lhs && is_value rhs -> step_binop expr ctx
  | Binop (op, lhs, rhs) when is_value lhs ->
      ({ expr with expression = Binop (op, lhs, fst (step rhs ctx)) }, ctx)
  | Binop (op, lhs, rhs) ->
      ({ expr with expression = Binop (op, fst (step lhs ctx), rhs) }, ctx)
  | List exprs when is_list_of_values exprs ->
      ({ expr with expression = List exprs }, ctx)
  | List exprs ->
      ( {
          expr with
          expression =
            List (List.map ~f:(fun expr -> fst (step expr ctx)) exprs);
        },
        ctx )
  | True | False | Number _ | String _ -> (expr, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."
  | Cond (exprs, default) -> lazy_step_cond (exprs, default) ctx
  | LetBinding (bindings, expr)
    when List.for_all ~f:(fun (_, expr) -> is_value expr) bindings ->
      let binding_ids = List.map ~f:(fun (id, _) -> id) bindings in
      let binding_exprs = List.map ~f:(fun (_, expr) -> expr) bindings in
      let alpha_converted = alpha_convert binding_ids [] expr in
      let beta_reduced =
        beta_reduce binding_ids
          (List.map ~f:(fun b -> b.expression) binding_exprs)
          alpha_converted
      in
      (fst (step beta_reduced (bindings @ ctx)), ctx)
      (* need to beta-reduce here. *)
  | LetBinding (bindings, expr) ->
      let stepped_bindings =
        List.map ~f:(fun (id, expr) -> (id, fst (step expr ctx))) bindings
      in
      ({ expr with expression = LetBinding (stepped_bindings, expr) }, ctx)
  | Do (unit_expr, actual_expr) when is_value unit_expr -> step actual_expr ctx
  | Do (unit_expr, actual_expr) ->
      let stepped_unit, _ = step unit_expr ctx in
      ({ expr with expression = Do (stepped_unit, actual_expr) }, ctx)

and step_binop expr ctx =
  match expr.expression with
  (* Numbers *)
  | Binop (Plus, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      ({ expr with expression = Number (lhs +. rhs) }, ctx)
  | Binop (Minus, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      ({ expr with expression = Number (lhs -. rhs) }, ctx)
  | Binop (Times, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      ({ expr with expression = Number (lhs *. rhs) }, ctx)
  | Binop
      (Division, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      ({ expr with expression = Number (lhs /. rhs) }, ctx)
  | Binop (Mod, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    when Float.is_integer lhs && Float.is_integer rhs ->
      ( {
          expr with
          expression =
            Number (Int.to_float (Float.to_int lhs % Float.to_int rhs));
        },
        ctx )
  (* Strings *)
  | Binop (Plus, { expression = String lhs; _ }, { expression = String rhs; _ })
    ->
      ({ expr with expression = String (lhs ^ rhs) }, ctx)
  | Binop (Minus, { expression = String lhs; _ }, { expression = String rhs; _ })
    ->
      ({ expr with expression = String (lhs ^ rhs) }, ctx)
  (* Boolean logic *)
  | Binop
      (Equals, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      if Poly.(lhs = rhs) then ({ expr with expression = True }, ctx)
      else ({ expr with expression = False }, ctx)
  | Binop
      (LessThan, { expression = Number lhs; _ }, { expression = Number rhs; _ })
    ->
      if Poly.(lhs < rhs) then ({ expr with expression = True }, ctx)
      else ({ expr with expression = False }, ctx)
  | Binop
      (Equals, { expression = String lhs; _ }, { expression = String rhs; _ })
    ->
      if Poly.(lhs = rhs) then ({ expr with expression = True }, ctx)
      else ({ expr with expression = False }, ctx)
  | Binop (Equals, { expression = True; _ }, { expression = True; _ }) ->
      ({ expr with expression = True }, ctx)
  | Binop (Equals, { expression = False; _ }, { expression = False; _ }) ->
      ({ expr with expression = True }, ctx)
  | Binop (Equals, _, _) -> ({ expr with expression = False }, ctx)
  | _ ->
      failwith
        (Printf.sprintf "Could not execute binary operation on %s."
           (string_of_expr expr.expression))

and step_list exprs args ctx =
  let args = List.map ~f:(fun a -> a.expression) args in
  match args with
  | Number nth :: [] when Float.is_integer nth ->
      let res =
        match List.nth exprs (Float.to_int nth) with
        | Some r -> r
        | None ->
            failwith
              (Printf.sprintf "Index %d is out of bounds" (Float.to_int nth))
      in

      (res, ctx)
  | _ -> failwith "Key must be integer"

and lazy_step_cond (exprs, default) ctx =
  match exprs with
  | case :: expr :: t -> (
      match fst (step case ctx) with
      | { expression = False; _ } | { expression = Unit; _ } ->
          lazy_step_cond (t, default) ctx
      | x when not (is_value x) ->
          lazy_step_cond (fst (step case ctx) :: expr :: t, default) ctx
      | _ -> (fst (step expr ctx), ctx))
  | [] -> (fst (step default ctx), ctx)
  | _ -> failwith "Cond-expression with uneven cases."

and apply_builtin to_apply args ctx =
  match to_apply.expression with
  | Identifier name -> (
      match name with
      | "println" ->
          check_arity 1 args;
          {
            to_apply with
            expression = println_impl (List.map ~f:(fun a -> a.expression) args);
          }
      (* | "readfile" -> (
          check_arity 1 args;
          match List.hd args with
          | Some (String filename) -> readfile_impl filename
          | _ ->
              raise
                (Builtin_error "Filename passed to readfile must be string.")) *)
      | "eval" -> (
          check_arity 1 args;
          match List.hd args with
          | Some { expression = String meta_expr; _ } ->
              let nsed_expr = "(namespace meta_eval)\n" ^ meta_expr in
              let res, _ = eval_program nsed_expr ctx in
              res
          | _ ->
              raise
                (Builtin_error "Meta-evaluation can only be done on strings."))
      | _ ->
          failwith (Printf.sprintf "No builtin function found for '%s'." name))
  | _ -> failwith "Function application did not refer to Identifier."

and eval e ctx =
  if is_value e then (e, ctx)
  else
    let stepped, ctx = step e ctx in
    eval stepped ctx

and eval_program p initial_ctx =
  let char_seq = String.to_list p in
  let program = Parser.parse char_seq in
  match program with
  | Program namespaces -> (
      let ctx, res =
        List.fold
          ~f:(fun (ctx_acc, result) (Namespace (_, _, exprs)) ->
            let new_ctx, new_result =
              List.fold
                ~f:(fun (ctx_acc, _) expr ->
                  let result, extended_ctx = eval expr ctx_acc in
                  (extended_ctx, Some result))
                ~init:(ctx_acc, result) exprs
            in
            (new_ctx, new_result))
          ~init:(initial_ctx, None) namespaces
      in
      match res with
      | Some result -> (result, ctx)
      | None -> failwith "Couldn't evaluate program")
