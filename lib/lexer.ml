open Tokens

let position = {
  row = 0;
  col = -1;
}

let is_digit char = 
  match int_of_char char with
  | x when x > 47 && x < 58 -> true
  | _ -> false

  let is_letter char = 
    match int_of_char char with
    | x when x > 96 && x < 123 -> true
    | _ -> false

let is_whitespace = function
  | ' ' | '\n' | '\r' | '\t' -> true
  | _ -> false

let get_as_int char = int_of_char char - 48

let incr_row () = 
  position.row <- position.row + 1;
  position.col <- -1

let incr_col () = position.col <- position.col + 1

let reset_pos () = position.row <- 0; position.col <- -1

let reified_position () = 
  Printf.printf "reify at %d %d\n" position.row position.col;
  Position (position.row, position.col)

let lex_number chars = 
  let rec aux acc rem = match rem with
  | h :: t when is_digit h -> 
    aux (acc * 10 + get_as_int h) t
  | _ -> (Token (INTEGER_TOKEN acc, reified_position ()), rem)
  in aux 0 chars

let lex_string chars =
  let rec aux acc rem = match rem with
  | '"' :: t -> (Token (STRING_TOKEN acc, reified_position ()), t)
  | h :: t -> aux (acc ^ (String.make 1 h)) t  
  | [] -> raise (Failure "unmatched string")
  in aux "" chars

let lex_identifier_or_keyword chars = (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem = match rem with
  | h :: t when is_whitespace h -> ((acc, reified_position ()), t)
  | [] -> ((acc, reified_position ()), [])
  | h :: t -> aux (acc ^ (String.make 1 h)) t  
  in let ((id, pos), remaining) = aux "" chars in
  if id = "def" then (Token (DEF, pos), remaining) 
  else (Token (IDENTIFIER_TOKEN id, pos), remaining) 

let rec next_token chars = 
  incr_col (); 
  match chars with
  | h :: t when is_whitespace h -> 
    next_token t
  | '\n' :: t -> 
    incr_row (); 
    next_token t
  | '(' :: t -> (Token (LPAREN, reified_position ()), t)
  | ')' :: t -> (Token (RPAREN, reified_position ()), t)
  | '+' :: t -> (Token (PLUS, reified_position ()), t)
  | '-' :: t -> (Token (MINUS, reified_position ()), t)
  | '*' :: t -> (Token (MULTIPLY, reified_position ()), t)
  | '"' :: t -> lex_string (t) 
  | h :: t when is_letter h -> lex_identifier_or_keyword (h :: t)
  | h :: t when is_digit h -> lex_number (h :: t)
  | h :: t -> (Token (UNKNOWN h, reified_position ()), t)
  | [] -> 
    let ret = (Token (EOF, reified_position ()), []) 
    in reset_pos (); ret
