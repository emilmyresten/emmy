open Base
open Printf

exception Arity_exn of string

let check_arity expected args =
  if not (List.length args = expected) then
    raise
      (Arity_exn
         (sprintf "Wrong arity: expected %d args, received %d." expected
            (List.length args)))
