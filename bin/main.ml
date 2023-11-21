open Interp 

let read () = Stdlib.read_line ()

let rec loop () =
  
  let read = read () |> String.to_seq |> List.of_seq in
  let eval = Parser.parse read |> Interpreter.eval in
  let _print = Printf.printf "%s\n" (Interpreter.string_of_val eval) in
  loop ()

let () = loop ()
  