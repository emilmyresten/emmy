open Interp 
open Unix


let rec loop ctx =
  try
    let read = Stdlib.read_line () in
    let (eval, ctx) = Interpreter.eval_program read ctx in
    let _print = Printf.printf "%s\n" (Pprint.string_of_expr eval) in
    print_endline "Context:";
    print_endline (Pprint.string_of_context ctx);
    loop ctx
  with e -> print_endline (Printexc.to_string e); loop ctx

let () = let ctx = [] in 
  let t = Unix.localtime (Unix.time ()) in
  let with_leading_zero n = if n < 10 then Printf.sprintf "0%d" n else Printf.sprintf "%d" n in
  let formatted_time = Printf.sprintf "%s:%s:%s" (with_leading_zero t.tm_hour) (with_leading_zero t.tm_min) (with_leading_zero t.tm_sec) in
  Printf.printf "REPL started at %s\n" formatted_time;
  loop ctx
  