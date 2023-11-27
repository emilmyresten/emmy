open Extensions
open Tokens

let position = {
  row = 0;
  col = -1;
}

let incr_row () = 
  position.row <- position.row + 1;
  position.col <- -1

let incr_col () = position.col <- position.col + 1

let reset_pos () = position.row <- 0; position.col <- -1

let reified_position () = 
  Position (position.row, position.col)

let lex_number chars = 
  let rec aux acc rem = match rem with
  | h :: t when Char.is_digit h -> 
    aux (acc * 10 + Char.as_int h) t
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
  | h :: t when Char.is_whitespace h || Char.is_symbol h -> ((acc, reified_position ()), h :: t)
  | [] -> ((acc, reified_position ()), [])
  | h :: t -> aux (acc ^ (String.make 1 h)) t  
  in let ((parsed, pos), remaining) = aux "" chars in
  match (List.assoc_opt parsed keywords) with
  | Some token_type -> (Token (token_type, pos), remaining)
  | None -> (Token (IDENTIFIER_TOKEN parsed, pos), remaining) 


let rec next_token chars = 
  incr_col (); 
  match chars with
  | h :: t when Char.is_whitespace h -> 
    next_token t
  | '\n' :: t -> 
    incr_row (); 
    next_token t
  | '(' :: t -> (Token (LPAREN, reified_position ()), t)
  | ')' :: t -> (Token (RPAREN, reified_position ()), t)
  | '-' :: '>' :: t -> (Token (ARROW, reified_position ()), t)
  | '+' :: t -> (Token (PLUS, reified_position ()), t)
  | '-' :: t -> (Token (MINUS, reified_position ()), t)
  | '*' :: t -> (Token (TIMES, reified_position ()), t)
  | '/' :: t -> (Token (DIVISION, reified_position ()), t)  
  | '=' :: t -> (Token (EQUALS, reified_position ()), t)
  | '"' :: t -> lex_string (t) 
  | h :: t when Char.is_letter h -> lex_identifier_or_keyword (h :: t)
  | h :: t when Char.is_digit h -> lex_number (h :: t)
  | h :: t -> (Token (UNKNOWN h, reified_position ()), t)
  | [] -> 
    let ret = (Token (EOF, reified_position ()), []) 
    in reset_pos (); ret

let rec peek chars =
  match chars with
  | h :: t when Char.is_whitespace h -> 
    peek t
  | '\n' :: t -> 
    peek t
  | '(' :: _ -> LPAREN
  | ')' :: _ -> RPAREN
  | '-' :: '>' :: _ -> ARROW
  | '+' :: _ -> PLUS
  | '-' :: _ -> MINUS
  | '*' :: _ -> TIMES
  | '/' :: _ -> DIVISION
  | '=' :: _ -> EQUALS
  | '"' :: t -> peek_string (t) 
  | h :: t when Char.is_letter h -> peek_identifier_or_keyword (h :: t)
  | h :: t when Char.is_digit h -> peek_number (h :: t)
  | h :: _ -> UNKNOWN h
  | [] -> 
    let ret = EOF
    in reset_pos (); ret
and peek_string chars =
  let rec aux acc rem = match rem with
  | '"' :: _ -> STRING_TOKEN acc
  | h :: t -> aux (acc ^ (String.make 1 h)) t  
  | [] -> raise (Failure "unmatched string")
  in aux "" chars
and peek_number chars = 
  let rec aux acc rem = match rem with
  | h :: t when Char.is_digit h -> 
    aux (acc * 10 + Char.as_int h) t
  | _ -> INTEGER_TOKEN acc
  in aux 0 chars
and peek_identifier_or_keyword chars = (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem = match rem with
  | h :: _ when Char.is_whitespace h || Char.is_symbol h -> acc
  | [] -> acc
  | h :: t -> aux (acc ^ (String.make 1 h)) t  
  in let id = aux "" chars in
  if id = "def" then DEF
  else if id = "fn" then FN
  else IDENTIFIER_TOKEN id