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

let source_file ~filename =
  let has_extension = String.is_suffix filename ~suffix:".emmy" in
  let filename = if has_extension then filename else filename ^ ".emmy" in
  read_lines (In_channel.create filename) |> String.concat ~sep:"\n"
