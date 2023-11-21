type current_position = {
  mutable row: int;
  mutable col: int;
} (* mutable but easiest way to keep track of cursor position in source. *)

type token_position = Position of int * int


type token_type = LPAREN
                | RPAREN 
                | PLUS
                | MINUS 
                | MULTIPLY
                | DEF
                | INTEGER_TOKEN of int
                | STRING_TOKEN of string
                | IDENTIFIER_TOKEN of string
                | UNKNOWN of char
                | EOF

type token = Token of token_type * token_position

let get_position = function
  | Token (_, pos) -> pos

let string_of_token_type = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | PLUS -> "+"
  | MINUS -> "-"
  | MULTIPLY -> "*"
  | DEF -> "def"
  | INTEGER_TOKEN v -> string_of_int v 
  | STRING_TOKEN str -> Printf.sprintf "\"%s\"" str
  | IDENTIFIER_TOKEN id -> Printf.sprintf "IDENTIFIER %s" id
  | UNKNOWN c -> Printf.sprintf "UNKNOWN %c" c
  | EOF -> "EOF"

let string_of_token = function
  | Token (tk, _) -> string_of_token_type tk

let get_row_col tok = match get_position tok with
  | (Position (row, col)) -> (row, col)
