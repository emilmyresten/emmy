open Interp 

let read () = Stdlib.read_line ()

let rec loop ctx =
  
  let read = read () |> String.to_seq |> List.of_seq in
  let (eval, ctx) = let ast = Parser.parse read in Interpreter.eval ast ctx in
  print_endline "Context:";
  Interpreter.print_context ctx;
  let _print = Printf.printf "%s\n" (Interpreter.string_of_val eval) in
  loop ctx

let () = let ctx = [] in loop ctx
  