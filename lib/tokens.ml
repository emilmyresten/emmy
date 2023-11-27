open Printf

type current_position = {
  mutable row: int;
  mutable col: int;
} (* mutable but easiest way to keep track of cursor position in source. *)

type token_position = Position of int * int


type token_type = LPAREN
                | RPAREN 
                | DEF
                | FN
                | ARROW
                | PLUS
                | MINUS
                | TIMES
                | DIVISION
                | TRUE
                | FALSE
                | EQUALS
                | COND
                | INTEGER_TOKEN of int
                | STRING_TOKEN of string
                | IDENTIFIER_TOKEN of string
                | UNKNOWN of char
                | EOF

type token = Token of token_type * token_position

let keywords = [
  ("def", DEF);
  ("fn", FN);
  ("true", TRUE);
  ("false", FALSE);
  ("cond", COND);
]

let get_position = function
  | Token (_, pos) -> pos

let string_of_token_type = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | DEF -> "DEF"
  | FN -> "FN"
  | ARROW -> "->"
  | PLUS -> "+"
  | MINUS -> "-"
  | TIMES -> "*"
  | DIVISION -> "/"
  | EQUALS -> "="
  | TRUE -> "true"
  | FALSE -> "false"
  | COND -> "cond"
  | INTEGER_TOKEN v -> string_of_int v 
  | STRING_TOKEN str -> sprintf "\"%s\"" str
  | IDENTIFIER_TOKEN id -> sprintf "IDENTIFIER %s" id
  | UNKNOWN c -> sprintf "UNKNOWN %c" c
  | EOF -> "EOF"

let string_of_token = function
  | Token (tk, _) -> string_of_token_type tk

let get_row_col tok = match get_position tok with
  | (Position (row, col)) -> (row, col)
