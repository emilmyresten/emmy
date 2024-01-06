open Base
open Printf

type current_position = { mutable row : int; mutable col : int }
(* mutable but easiest way to keep track of cursor position in source. *)

type token_position = { row : int; col : int }

type token_kind =
  | LPAREN
  | RPAREN
  | LBRACKET
  | RBRACKET
  | DEF
  | FN
  | ARROW
  | PLUS
  | MINUS
  | TIMES
  | DIVISION
  | MOD
  | TRUE
  | FALSE
  | EQUALS
  | LESS_THAN
  | COND
  | LET
  | DO
  | NAMESPACE
  | REQUIRES
  | NUMBER_TOKEN of float
  | STRING_TOKEN of string
  | IDENTIFIER_TOKEN of string
  | UNKNOWN of char
  | EOF

type token = { kind : token_kind; pos : token_position }

let keywords =
  [
    ("def", DEF);
    ("fn", FN);
    ("true", TRUE);
    ("false", FALSE);
    ("cond", COND);
    ("let", LET);
    ("do", DO);
    ("namespace", NAMESPACE);
    ("requires", REQUIRES);
  ]

let get_position = function token -> token.pos

let string_of_token_kind = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | DEF -> "keyword def"
  | FN -> "keyword fn"
  | ARROW -> "->"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVISION -> "/"
  | MOD -> "%"
  | EQUALS -> "="
  | LESS_THAN -> "<"
  | TRUE -> "true"
  | FALSE -> "false"
  | COND -> "keyword cond"
  | LET -> "keyword let"
  | DO -> "keyword do"
  | NAMESPACE -> "keyword namespace"
  | REQUIRES -> "keyword requires"
  | NUMBER_TOKEN v ->
      if Float.is_integer v then Int.to_string (Float.to_int v)
      else Float.to_string v
  | STRING_TOKEN str -> sprintf "\"%s\"" str
  | IDENTIFIER_TOKEN id -> sprintf "IDENTIFIER %s" id
  | UNKNOWN c -> sprintf "UNKNOWN %c" c
  | EOF -> "EOF"

let string_of_token_pos = function { row; col } -> sprintf "(%d, %d)" row col

let string_of_token token =
  sprintf "%s, pos: %s"
    (string_of_token_kind token.kind)
    (string_of_token_pos token.pos)

let get_row_col tok = (tok.pos.row, tok.pos.col)
