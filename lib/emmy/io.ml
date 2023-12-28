open Base
open Stdio

let ( >>| ) x f = Option.map ~f x
let sources_path = ref ""

let set_sources_path ~filename =
  let file_ending =
    String.split filename ~on:'/' |> List.drop_last >>| String.concat ~sep:"/"
  in
  match file_ending with None -> () | Some f -> sources_path := f ^ "/"

let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    let line = In_channel.input_line in_channel in
    match line with
    | Some l -> read_lines_aux (l :: lines) in_channel
    | None -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let open_source_file filename =
  let has_extension = String.is_suffix filename ~suffix:".emmy" in
  let filename = if has_extension then filename else filename ^ ".emmy" in
  read_lines (In_channel.create filename) |> String.concat ~sep:"\n"

let get_char_stream ~namespace =
  String.substr_replace_all namespace ~pattern:"." ~with_:"/"
  |> ( ^ ) !sources_path |> open_source_file |> String.to_list
