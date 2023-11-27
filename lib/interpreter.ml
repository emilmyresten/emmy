open Printf
open Pprint
open Extensions
open Expressions

let () = Random.init 0


let get_from_ctx id ctx = try (List.assoc id ctx) with _ -> failwith (sprintf "Unbound identifier %s in context %s" id (string_of_context ctx))

let rec get_unique_name p bindings = 
  let suffix = Random.int 512 in
  let name_attempt = p ^ "#" ^ string_of_int suffix in
  if List.mem name_attempt bindings then 
    get_unique_name p bindings 
else
    name_attempt

let rec beta_reduce param_ids args expr =
  match expr with
  | Identifier id -> if List.mem id param_ids then 
    let param_position_opt = List.find_index (fun m -> m = id) param_ids in
      (match param_position_opt with
        | Some pos -> 
          (match (List.nth_opt args pos) with
            | Some elem -> elem
            | None -> failwith "Wrong arity!")
        | None -> failwith "Identifier id is member of the parameter list, but couldn't find index during beta-reduction.")
   else expr
  | Def (id, expr) -> Def (id, beta_reduce param_ids args expr)
  | Fn (params, expr) -> Fn (params, beta_reduce param_ids args expr)
  | FnInvoke (to_apply, fn_args) -> FnInvoke (to_apply, List.map (fun arg -> beta_reduce param_ids args arg) fn_args)
  | Binop (op, lhs, rhs) -> Binop (op, (beta_reduce param_ids args lhs), (beta_reduce param_ids args rhs))
  | True | False | Integer _ | String _ | Unit -> expr
  | Cond (exprs, default) -> Cond (List.map (fun expr -> beta_reduce param_ids args expr) exprs, beta_reduce param_ids args default)
  
and alpha_convert bindings replace expr = 
  (* [bindings: list of seen bindings.
      replace: look in this associative list/map to find replacement names for the current.]*)
  (* Rename nested identifiers to resolve scope conflicts. 
   As soon as bindings occur in two scopes, all subsequent have to be renamed to the binding of the nearest scope. *)
  match expr with
  | Identifier id -> 
      (match (List.assoc_opt id replace) with 
      | Some name -> Identifier name 
      | None -> expr)
  | Def (_, expr) -> alpha_convert bindings replace expr
  | Fn (params, expr) -> 
    let conflicts = List.overlap bindings params in (* replace these *)
    let bind = List.unique_right bindings params in (* add these to the seen bindings *)
    let new_replace = List.fold_left (fun a p -> [(p, get_unique_name p bindings)] @ a) [] conflicts in
    let new_params = (List.map (fun (_, v) -> v) new_replace) @ bind in
    let new_bindings = new_params @ bind @ bindings in
    Fn (new_params, alpha_convert new_bindings new_replace expr)
  | FnInvoke (to_apply, args) -> FnInvoke (alpha_convert bindings replace to_apply, List.map (fun arg -> alpha_convert bindings replace arg) args)
  | Binop (op, lhs, rhs) -> Binop (op, (alpha_convert bindings replace lhs), (alpha_convert bindings replace rhs))
  | True | False | Integer _ | String _ | Unit -> expr
  | Cond (exprs, default) ->  Cond (List.map (fun expr -> alpha_convert bindings replace expr) exprs, alpha_convert bindings replace default)



(* let map_of_params params = List.map (fun p -> (p, Identifier p)) params *)
let check_arity params args = List.length (args) = List.length (params)
let is_list_of_values exprs = List.for_all (fun m -> is_value m) exprs

let rec step expr ctx =  
  match expr with
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) -> let (stepped, new_ctx) = step expr ctx in (Def (id, stepped), new_ctx)
  | Fn (params, expr) -> (alpha_convert params [] (Fn (params, expr)), ctx)
  | FnInvoke (to_apply, args) when is_value to_apply && is_list_of_values args -> 
    (match to_apply with
    | Fn (params, expr) -> 
      if check_arity params args then 
        let (stepped, _) = step expr ((List.combine params args) @ ctx) in
        (beta_reduce params args stepped, ctx)
      else
        failwith (sprintf "Wrong arity: function expected %d args, received %d." (List.length params) (List.length args))
    | _ -> failwith (sprintf "%s is not a function." (string_of_expr to_apply)))
  | FnInvoke (to_apply, args) when is_value to_apply -> (FnInvoke (to_apply, (List.map (fun m -> fst (step m ctx)) args)), ctx)
  | FnInvoke (to_apply, args) -> (FnInvoke (fst (step to_apply ctx), args), ctx)
  | Binop (_, lhs, rhs) when is_value lhs && is_value rhs -> step_binop expr ctx
  | Binop (op, lhs, rhs) when is_value lhs -> (Binop (op, lhs, fst (step rhs ctx)), ctx)
  | Binop (op, lhs, rhs) -> (Binop (op, fst (step lhs ctx), rhs), ctx)
  | True | False | Integer _ | String _ -> (expr, ctx)
  | Identifier id -> let value = get_from_ctx id ctx in (value, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."
  | Cond (exprs, default) -> lazy_step_cond (exprs, default) ctx
and step_binop expr ctx = 
  match expr with
  (* Numbers *)
  | Binop (Plus, Integer lhs, Integer rhs) -> (Integer (lhs + rhs), ctx)
  | Binop (Minus, Integer lhs, Integer rhs) -> (Integer (lhs - rhs), ctx)
  | Binop (Times, Integer lhs, Integer rhs) -> (Integer (lhs * rhs), ctx)
  | Binop (Division, Integer lhs, Integer rhs) -> (Integer (lhs / rhs), ctx)
  (* Strings *)
  | Binop (Plus, String lhs, String rhs) -> (String (lhs ^ rhs), ctx)
  | Binop (Minus, String lhs, String rhs) -> (String (lhs ^ rhs), ctx)
  (* Boolean logic *)
  | Binop (Equals, Integer lhs, Integer rhs) -> if lhs = rhs then (True, ctx) else (False, ctx)
  | Binop (Equals, String lhs, String rhs) -> if lhs = rhs then (True, ctx) else (False, ctx) 
  | Binop (Equals, True, True) -> (True, ctx)
  | Binop (Equals, False, False) -> (False, ctx)
  | Binop (Equals, _, _) -> (False, ctx)

  | _ -> failwith (sprintf "Could not execute binary operation on %s." (string_of_expr expr))
and lazy_step_cond (exprs, default) ctx =
  match exprs with
  | case :: expr :: t -> 
    (match (fst (step case ctx)) with
    | (False | Unit) -> lazy_step_cond (t, default) ctx
    | x when not (is_value x) -> lazy_step_cond ((fst (step case ctx)) :: expr :: t, default) ctx
    | _ -> (fst (step expr ctx), ctx))
  | [] -> (fst (step default ctx), ctx)
  | _ -> failwith "Cond-expression with uneven cases."

let rec eval e ctx =
  if is_value e then (e, ctx)
  else let (stepped, ctx) = step e ctx in eval stepped ctx

let eval_program p initial_ctx = 
  let cmds = String.split_on_char ';' p in
  let rec eval_all_aux ctx cmds = 
    (match cmds with
    | [] -> failwith "empty program"
    | final_cmd :: [] -> 
      let char_seq = String.to_seq final_cmd |> List.of_seq in
      let (eval, ctx) = eval (Parser.parse char_seq) ctx in
      (eval, ctx)
    | cmd :: t -> 
      let char_seq = String.to_seq cmd |> List.of_seq in
      let (_, ctx) = eval (Parser.parse char_seq) ctx in (* we do not care about intermediate evals other than storing in ctx *)
      eval_all_aux ctx t) in
  eval_all_aux initial_ctx cmds