open Interp 

let read () = Stdlib.read_line ()

let rec loop ctx =
  try
    let read = read () |> String.to_seq |> List.of_seq in
    let (eval, ctx) = let ast = Parser.parse read in Interpreter.eval ast ctx in
    let _print = Printf.printf "%s\n" (Pprint.string_of_expr eval) in
    print_endline "Context:";
    print_endline (Pprint.string_of_context ctx);
    loop ctx
  with e -> print_endline (Printexc.to_string e); loop ctx

let () = let ctx = [] in loop ctx
  