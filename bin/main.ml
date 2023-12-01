open Interp 
open Interepl.Util

let rec loop ctx cmd_history =
  if List.length cmd_history > 0 then iprint "cmd-history: \n";
  List.iter (fun m -> iprint_format "%s\n" m) cmd_history;
  iprint "\n";
  let read = next_cmd cmd_history in (* this one needs to be rebuilt for better UX, with cmd-history passed to it etc. *)
  let history = read :: cmd_history in
  try
    let (eval, ctx) = Interpreter.eval_program read ctx in
    let _print = iprint_format "%s\n" (Pprint.string_of_expr eval) in
    print_endline "Context:";
    print_endline (Pprint.string_of_context ctx);
    loop ctx history
  with e -> print_endline (Printexc.to_string e); loop ctx history
  
let () = let ctx = [] in 
  let t = Unix.localtime (Unix.time ()) in
  let with_leading_zero n = if n < 10 then Printf.sprintf "0%d" n else Printf.sprintf "%d" n in
  let formatted_time = Printf.sprintf "%s:%s:%s" (with_leading_zero t.tm_hour) (with_leading_zero t.tm_min) (with_leading_zero t.tm_sec) in
  iprint_format "REPL started at %s\n" formatted_time;
  set_raw_mode ();
  loop ctx []
  