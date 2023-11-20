open Interp 

let read () = Stdlib.read_line ()

let rec loop () =
  
  let read = read () |> String.to_seq |> List.of_seq in
  let eval = Parser.parse read |> Interpreter.eval in
  let _print = Printf.printf "%d\n" eval in
  loop ()

let () = loop ()
  