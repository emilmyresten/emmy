open Printf

type current_position = { mutable row : int; mutable col : int }
(* mutable but easiest way to keep track of cursor position in source. *)

type token_position = Position of int * int

type token_type =
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
  | INTEGER_TOKEN of int
  | STRING_TOKEN of string
  | IDENTIFIER_TOKEN of string
  | UNKNOWN of char
  | EOF

type token = Token of token_type * token_position

let keywords =
  [
    ("def", DEF);
    ("fn", FN);
    ("true", TRUE);
    ("false", FALSE);
    ("cond", COND);
    ("let", LET);
  ]

let get_position = function Token (_, pos) -> pos

let string_of_token_type = function
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
  | INTEGER_TOKEN v -> string_of_int v
  | STRING_TOKEN str -> sprintf "\"%s\"" str
  | IDENTIFIER_TOKEN id -> sprintf "IDENTIFIER %s" id
  | UNKNOWN c -> sprintf "UNKNOWN %c" c
  | EOF -> "EOF"

let string_of_token_pos = function
  | Position (row, col) -> sprintf "(%d, %d)" row col

let string_of_token = function
  | Token (tk, pos) ->
      sprintf "%s, pos: %s" (string_of_token_type tk) (string_of_token_pos pos)

let get_row_col tok =
  match get_position tok with Position (row, col) -> (row, col)
