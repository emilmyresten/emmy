open Printf
open Pprint
open Extensions
open Expressions

let () = Random.init 0

let get_from_ctx id ctx =
  try List.assoc id ctx
  with _ ->
    failwith
      (sprintf "Unbound identifier %s in context %s" id (string_of_context ctx))

let rec get_unique_name p bindings =
  let suffix = Random.int 512 in
  let name_attempt = p ^ "#" ^ string_of_int suffix in
  if List.mem name_attempt bindings then get_unique_name p bindings
  else name_attempt

let rec beta_reduce param_ids replacements expr =
  match expr with
  | Identifier id ->
      if List.mem id param_ids then
        let param_position_opt = List.find_index (fun m -> m = id) param_ids in
        match param_position_opt with
        | Some pos -> List.nth replacements pos
        | None ->
            failwith
              (sprintf
                 "Identifier %s is member of the parameter list, but couldn't \
                  find index during beta-reduction."
                 id)
      else expr
  | Def (id, expr) -> Def (id, beta_reduce param_ids replacements expr)
  | Fn (params, expr) -> Fn (params, beta_reduce param_ids replacements expr)
  | Invoke (to_apply, args) ->
      Invoke (to_apply, List.map (beta_reduce param_ids replacements) args)
  | Binop (op, lhs, rhs) ->
      Binop
        ( op,
          beta_reduce param_ids replacements lhs,
          beta_reduce param_ids replacements rhs )
  | List exprs -> List (List.map (beta_reduce param_ids replacements) exprs)
  | True | False | Number _ | String _ | Unit -> expr
  | Cond (exprs, default) ->
      Cond
        ( List.map (beta_reduce param_ids replacements) exprs,
          beta_reduce param_ids replacements default )
  | LetBinding (bindings, expr) ->
      let beta_reduced_bindings =
        List.map
          (fun (id, expr) -> (id, beta_reduce param_ids replacements expr))
          bindings
      in
      LetBinding (beta_reduced_bindings, beta_reduce param_ids replacements expr)

and alpha_convert scope replacements expr =
  (* [bindings: list of seen bindings.
      replace: look in this associative list/map to find replacement names for the current.]*)
  (* Rename nested identifiers to resolve scope conflicts.
     As soon as bindings occur in two scopes, all subsequent have to be renamed to the binding of the nearest scope. *)
  match expr with
  | Identifier id -> (
      match List.assoc_opt id replacements with
      | Some name -> Identifier name
      | None -> expr)
  | Def (_, expr) -> alpha_convert scope replacements expr
  | Fn (params, expr) ->
      let conflicts = List.overlap scope params in
      (* replace these *)
      let bind = List.unique_right scope params in
      (* add these to the seen bindings *)
      let new_replace =
        List.fold_left
          (fun a p -> [ (p, get_unique_name p scope) ] @ a)
          [] conflicts
      in
      let new_params = List.map (fun (_, v) -> v) new_replace @ bind in
      let new_scope = new_params @ bind @ scope in
      Fn (new_params, alpha_convert new_scope new_replace expr)
  | Invoke (to_apply, args) ->
      Invoke
        ( alpha_convert scope replacements to_apply,
          List.map (alpha_convert scope replacements) args )
  | Binop (op, lhs, rhs) ->
      Binop
        ( op,
          alpha_convert scope replacements lhs,
          alpha_convert scope replacements rhs )
  | List exprs -> List (List.map (alpha_convert scope replacements) exprs)
  | True | False | Number _ | String _ | Unit -> expr
  | Cond (exprs, default) ->
      Cond
        ( List.map (alpha_convert scope replacements) exprs,
          alpha_convert scope replacements default )
  | LetBinding (bnds, expr) ->
      let bindings = List.map (fun (id, _) -> id) bnds in
      let conflicts = List.overlap scope bindings in
      let bind = List.unique_right scope bindings in
      let new_replace =
        List.fold_left
          (fun a p -> [ (p, get_unique_name p scope) ] @ a)
          [] conflicts
      in
      let new_let_bindings = List.map (fun (_, v) -> v) new_replace @ bind in
      let new_bnds =
        List.map (fun (_, v) -> v) bnds |> List.combine new_let_bindings
      in
      let new_scope = new_let_bindings @ bind @ scope in
      LetBinding (new_bnds, alpha_convert new_scope new_replace expr)

(* let map_of_params params = List.map (fun p -> (p, Identifier p)) params *)
let check_arity expected args =
  if not (List.length args = expected) then
    failwith
      (sprintf "Wrong arity: expected %d args, received %d." expected
         (List.length args))

let rec step expr ctx =
  match expr with
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) ->
      let stepped, new_ctx = step expr ctx in
      (Def (id, stepped), new_ctx)
  | Fn (params, expr) -> (alpha_convert params [] (Fn (params, expr)), ctx)
  | Invoke (to_apply, args) when is_value to_apply && is_list_of_values args
    -> (
      match to_apply with
      | Fn (params, expr) ->
          check_arity (List.length params) args;
          let stepped, _ = step expr (List.combine params args @ ctx) in
          (beta_reduce params args stepped, ctx)
      | List exprs ->
          check_arity 1 args;
          step_list exprs args ctx
      | _ ->
          failwith (sprintf "%s is not a function." (string_of_expr to_apply)))
  | Invoke (to_apply, args) when not (is_list_of_values args) ->
      (Invoke (to_apply, List.map (fun m -> fst (step m ctx)) args), ctx)
  | Invoke (to_apply, args) -> (
      match to_apply with
      | Identifier "println" ->
          Printf.printf "%s\n" (string_of_expr_list args);
          (Unit, ctx)
      | _ -> (Invoke (fst (step to_apply ctx), args), ctx))
  | Binop (_, lhs, rhs) when is_value lhs && is_value rhs -> step_binop expr ctx
  | Binop (op, lhs, rhs) when is_value lhs ->
      (Binop (op, lhs, fst (step rhs ctx)), ctx)
  | Binop (op, lhs, rhs) -> (Binop (op, fst (step lhs ctx), rhs), ctx)
  | List exprs when is_list_of_values exprs -> (List exprs, ctx)
  | List exprs -> (List (List.map (fun expr -> fst (step expr ctx)) exprs), ctx)
  | True | False | Number _ | String _ -> (expr, ctx)
  | Identifier id ->
      let value = get_from_ctx id ctx in
      (value, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."
  | Cond (exprs, default) -> lazy_step_cond (exprs, default) ctx
  | LetBinding (bindings, expr)
    when List.for_all (fun (_, expr) -> is_value expr) bindings ->
      let binding_ids = List.map (fun (id, _) -> id) bindings in
      let binding_exprs = List.map (fun (_, expr) -> expr) bindings in
      let alpha_converted = alpha_convert binding_ids [] expr in
      let beta_reduced =
        beta_reduce binding_ids binding_exprs alpha_converted
      in
      (fst (step beta_reduced (bindings @ ctx)), ctx)
      (* need to beta-reduce here. *)
  | LetBinding (bindings, expr) ->
      let stepped_bindings =
        List.map (fun (id, expr) -> (id, fst (step expr ctx))) bindings
      in
      (LetBinding (stepped_bindings, expr), ctx)

and step_binop expr ctx =
  match expr with
  (* Numbers *)
  | Binop (Plus, Number lhs, Number rhs) -> (Number (lhs +. rhs), ctx)
  | Binop (Minus, Number lhs, Number rhs) -> (Number (lhs -. rhs), ctx)
  | Binop (Times, Number lhs, Number rhs) -> (Number (lhs *. rhs), ctx)
  | Binop (Division, Number lhs, Number rhs) -> (Number (lhs /. rhs), ctx)
  | Binop (Mod, Number lhs, Number rhs)
    when Float.is_integer lhs && Float.is_integer rhs ->
      (Number (float_of_int (int_of_float lhs mod int_of_float rhs)), ctx)
  (* Strings *)
  | Binop (Plus, String lhs, String rhs) -> (String (lhs ^ rhs), ctx)
  | Binop (Minus, String lhs, String rhs) -> (String (lhs ^ rhs), ctx)
  (* Boolean logic *)
  | Binop (Equals, Number lhs, Number rhs) ->
      if lhs = rhs then (True, ctx) else (False, ctx)
  | Binop (LessThan, Number lhs, Number rhs) ->
      if lhs < rhs then (True, ctx) else (False, ctx)
  | Binop (Equals, String lhs, String rhs) ->
      if lhs = rhs then (True, ctx) else (False, ctx)
  | Binop (Equals, True, True) -> (True, ctx)
  | Binop (Equals, False, False) -> (False, ctx)
  | Binop (Equals, _, _) -> (False, ctx)
  | _ ->
      failwith
        (sprintf "Could not execute binary operation on %s."
           (string_of_expr expr))

and step_list exprs args ctx =
  match args with
  | Number nth :: [] when Float.is_integer nth ->
      let res =
        try List.nth exprs (int_of_float nth)
        with _ ->
          failwith (sprintf "Index %d is out of bounds" (int_of_float nth))
      in
      (res, ctx)
  | _ -> failwith "Key must be integer"

and lazy_step_cond (exprs, default) ctx =
  match exprs with
  | case :: expr :: t -> (
      match fst (step case ctx) with
      | False | Unit -> lazy_step_cond (t, default) ctx
      | x when not (is_value x) ->
          lazy_step_cond (fst (step case ctx) :: expr :: t, default) ctx
      | _ -> (fst (step expr ctx), ctx))
  | [] -> (fst (step default ctx), ctx)
  | _ -> failwith "Cond-expression with uneven cases."

let rec eval e ctx =
  if is_value e then (e, ctx)
  else
    let stepped, ctx = step e ctx in
    eval stepped ctx

let eval_program p initial_ctx =
  let char_seq = String.to_list p in
  let program = Parser.parse char_seq in
  match program with
  | Program exprs ->
      let last = List.hd (List.rev exprs)
      and other = List.rev (List.tl (List.rev exprs)) in
      let new_ctx =
        List.fold_left
          (fun accumulating_ctx expr ->
            let _, extended_ctx = eval expr accumulating_ctx in
            extended_ctx @ accumulating_ctx)
          initial_ctx other
      in
      eval last new_ctx
