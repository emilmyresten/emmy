open Interp
open Interepl

let rec loop ctx cmd_history =
  let read = Term.next_cmd cmd_history in
  Term.iprint_format "%s\n" read;
  let history = read :: cmd_history in
  try
    let eval, ctx = Interpreter.eval_program read ctx in
    let _print = Term.iprint_format "%s\n" (Pprint.string_of_expr eval) in
    (* Term.iprint "Context:\n";
       Term.iprint (Pprint.string_of_context ctx); *)
    loop ctx history
  with e ->
    Term.iprint_format "%s\n" (Printexc.to_string e);
    loop ctx history

let () =
  let ctx = [] in
  let t = Unix.localtime (Unix.time ()) in
  let with_leading_zero n =
    if n < 10 then Printf.sprintf "0%d" n else Printf.sprintf "%d" n
  in
  let formatted_time =
    Printf.sprintf "%s:%s:%s"
      (with_leading_zero t.tm_hour)
      (with_leading_zero t.tm_min)
      (with_leading_zero t.tm_sec)
  in
  Term.iprint_format "REPL started at %s\n" formatted_time;
  Term.set_raw_mode ();
  loop ctx []
