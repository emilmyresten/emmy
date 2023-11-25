open Printf
open Expressions

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

let print_context ctx = List.iter (fun (k, v) -> printf "%s = %s\n" k (string_of_val v)) ctx

let get_from_ctx id ctx = try (snd (List.find (fun (k, _) -> k = id) ctx)) with _ -> failwith (sprintf "Unbound identifier %s" id)

let rec beta_reduce param_id arg expr =
  match expr with
  | Parameter id -> if id = param_id then arg else expr
  | Def (_, expr) -> beta_reduce param_id arg expr
  | Fn (_, expr) -> beta_reduce param_id arg expr
  | _ -> expr

let rec step expr ctx =  
  match expr with
  | Def (id, expr) when is_value expr -> (Unit, (id, expr) :: ctx)
  | Def (id, expr) -> let (stepped, new_ctx) = step expr ctx in (Def (id, stepped), new_ctx)
  | Fn (params, expr) when is_value expr -> (Fn (params, expr), ctx)
  | Fn (params, expr) -> let step_ctx = (List.hd params, Parameter (List.hd params)) :: ctx in (Fn (params, fst (step expr step_ctx)), ctx)
  | FnInvoke (id, args) when is_value (List.hd args) -> 
    (let fn_definition = get_from_ctx id ctx in
    match fn_definition with
    | Fn (params, expr) -> (beta_reduce (List.hd params) (List.hd args) expr, ctx)
    | _ -> failwith (sprintf "%s is not a function." id)
    )
  | FnInvoke (id, args) -> (FnInvoke (id, (List.map (fun m -> fst (step m ctx)) args)), ctx)
  | Integer _ -> (expr, ctx)
  | String _ -> (expr, ctx)
  | Identifier id ->
    (let value = get_from_ctx id ctx in 
      match value with
      | Fn (_, _) -> print_endline "found function in ctx"; (value, ctx)
      | _ -> (value, ctx))
  | Parameter id -> (Parameter id, ctx)
  | Unit -> failwith "Shouldn't encounter unit when Parsing."


let rec eval e ctx =
  if is_value e then (e, ctx)
  else let (stepped, ctx) = step e ctx in eval stepped ctx
