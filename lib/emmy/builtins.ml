open Expressions
open Utils
open Pprint

let readfile_impl filename =
  let lines = Io.read_lines (open_in filename) in
  List (List.map (fun str -> String str) lines)

let println_impl args =
  Io.printf (string_of_expr_list args);
  Unit

let apply_builtin to_apply args =
  match to_apply with
  | Identifier name -> (
      match name with
      | "println" ->
          check_arity 1 args;
          println_impl args
      | "readfile" -> (
          check_arity 1 args;
          match List.hd args with
          | String filename -> readfile_impl filename
          | _ -> failwith "Filename passed to readfile must be string.")
      | _ -> failwith "No builtin function found.")
  | _ -> failwith "Function application did not refer to Identifier."
