open Printf
open Extensions
open Expressions

let () = Random.init 0

let rec string_of_val v = match v with
  | Integer v -> sprintf "Integer %d" v
  | String str -> sprintf "String %s" str
  | Unit -> "Unit"
  | Parameter id -> sprintf "Parameter %s" id
  | Fn (params, expr) -> sprintf "Function %s-> %s" (string_of_params params) (string_of_val expr) 
  | Identifier id -> id
  | _ -> failwith (sprintf "Expected value, found %s" (string_of_expr v))
and string_of_params params = (List.fold_left (fun acc m -> acc ^ m ^ " ") "" params)
and string_of_expr e = match e with
  | Def (id, expr) ->  sprintf "(def %s %s)" id (string_of_expr expr)
  | Identifier id -> sprintf "Identifier %s" id
  | v -> string_of_val v

let string_of_context ctx = List.fold_left (fun a (k, v) -> a ^ (sprintf "%s = %s\n" k (string_of_val v))) "" ctx

let get_from_ctx id ctx = try (List.assoc id ctx) with _ -> failwith (sprintf "Unbound identifier %s" id)

let rec get_unique_name p bindings = 
  let suffix = Random.int 512 in
  let name_attempt = p ^ "#" ^ string_of_int suffix in
  if List.mem name_attempt bindings then 
    get_unique_name p bindings 
else
    name_attempt

let rec beta_reduce param_id arg expr =
  match expr with
  | Parameter id -> if id = param_id then arg else expr
  | Def (id, expr) -> Def (id, beta_reduce param_id arg expr)
  | Fn (params, expr) -> Fn (params, beta_reduce param_id arg expr)
  | _ -> expr
and alpha_conversion bindings replace expr = 
  (* [bindings: list of seen bindings.
      replace: look in this associative list/map to find replacement names for the current.]*)
  (* Rename nested identifiers to resolve scope conflicts. 
   As soon as bindings occur in two scopes, all subsequent have to be renamed to the binding of the nearest scope. *)
  match expr with
  | Parameter id -> 
      (match (List.assoc_opt id replace) with 
      | Some name -> Parameter name 
      | None -> expr)
  | Def (_, expr) -> alpha_conversion bindings replace expr
  | Fn (params, expr) -> 
    let conflicts = List.overlap bindings params in (* replace these *)
    let bind = List.unique_right bindings params in (* add these to the seen bindings *)
    let new_replace = List.fold_left (fun a p -> [(p, get_unique_name p bindings)] @ a) [] conflicts in
    let new_params = (List.map (fun (_, v) -> v) new_replace) @ bind in
    let new_bindings = new_params @ bind @ bindings in
    Fn (new_params, alpha_conversion new_bindings new_replace expr)
  | _ -> expr


let rec step expr ctx =  
  match expr with
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) -> let (stepped, new_ctx) = step expr ctx in (Def (id, stepped), new_ctx)
  | Fn (params, expr) when is_value expr -> (Fn (params, expr), ctx)
  | Fn (params, expr) -> let step_ctx = (List.hd params, Parameter (List.hd params)) :: ctx in (alpha_conversion [] [] (Fn (params, fst (step expr step_ctx))), ctx)
  | FnInvoke (to_apply, args) when is_value to_apply && is_value (List.hd args) -> 
    (match to_apply with
    | Fn (params, expr) -> (beta_reduce (List.hd params) (List.hd args) expr, ctx)
    | _ -> failwith (sprintf "%s is not a function." (string_of_val to_apply)))
  | FnInvoke (to_apply, args) when is_value to_apply -> (FnInvoke (to_apply, (List.map (fun m -> fst (step m ctx)) args)), ctx)
  | FnInvoke (to_apply, args) -> (FnInvoke (fst (step to_apply ctx) , args), ctx)
  | Integer _ -> (expr, ctx)
  | String _ -> (expr, ctx)
  | Identifier id -> let value = get_from_ctx id ctx in (value, ctx)
  | Parameter id -> (Parameter id, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."


let rec eval e ctx =
  if is_value e then (e, ctx)
  else let (stepped, ctx) = step e ctx in eval stepped ctx
