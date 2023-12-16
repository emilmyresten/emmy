open Repl
open Emmy

let usage_msg =
  "emmy <filename>. If no files are supplied, the REPL will start."

let input_files = ref []
let anon_fun filename = input_files := filename :: !input_files
let speclist = []

let () =
  Arg.parse speclist anon_fun usage_msg;
  if List.is_empty !input_files then start_repl ()
  else
    let input_file = List.hd !input_files in
    let program = Io.read_lines (open_in input_file) |> String.concat "\n" in
    try
      let eval, _ = Interpreter.eval_program program [] in
      match eval with
      | Expressions.Unit -> ()
      | _ -> Printf.printf "%s\n" (Pprint.string_of_expr eval)
    with e -> Printf.printf "%s\n" (Printexc.to_string e)
