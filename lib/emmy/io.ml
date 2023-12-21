open Base
open Stdio

let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    let line = In_channel.input_line in_channel in
    match line with
    | Some l -> read_lines_aux (l :: lines) in_channel
    | None -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let printf str = printf "%s\n" str
