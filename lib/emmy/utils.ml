open Base
open Printf

let check_arity expected args =
  if not (List.length args = expected) then
    failwith
      (sprintf "Wrong arity: expected %d args, received %d." expected
         (List.length args))
