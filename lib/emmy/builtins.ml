open Base
open Stdio
open Expressions
open Pprint

exception Builtin_error of string

let readfile_impl filename =
  try
    let lines = Io.read_lines (In_channel.create filename) in
    List (List.map ~f:(fun str -> String str) lines)
  with _ -> raise (Builtin_error ("Couldn't read file '" ^ filename ^ "'"))

let println_impl args =
  printf "%s\n" (string_of_expr_list args);
  Unit
