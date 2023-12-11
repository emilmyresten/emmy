open Extensions
open Tokens

let position = { row = 0; col = -1 }

let incr_row () =
  position.row <- position.row + 1;
  position.col <- -1

let incr_col () = position.col <- position.col + 1

let reset_pos () =
  position.row <- 0;
  position.col <- -1

let reified_position () = Position (position.row, position.col)

let lex_number chars =
  let rec aux acc rem ~lexing_decimal_part =
    match rem with
    | h :: t when Char.is_digit h ->
        aux ((acc * 10) + Char.as_int h) t ~lexing_decimal_part
    | h :: t when h = '.' ->
        if not lexing_decimal_part then
          let integer_part = acc in
          let decimal_part, rem = aux 0 t ~lexing_decimal_part:true in
          let float = string_of_int integer_part ^ "." ^ decimal_part in
          (float, rem)
        else failwith "Error while parsing float"
    | _ -> (string_of_int acc, rem)
  in
  let number, rem = aux 0 chars ~lexing_decimal_part:false in
  (Token (NUMBER_TOKEN (float_of_string number), reified_position ()), rem)

let lex_string chars =
  let rec aux acc rem =
    match rem with
    | '"' :: t -> (Token (STRING_TOKEN acc, reified_position ()), t)
    | h :: t -> aux (acc ^ String.make 1 h) t
    | [] -> failwith "unmatched string"
  in
  aux "" chars

let lex_identifier_or_keyword chars =
  (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem =
    match rem with
    | h :: t when Char.is_whitespace h || Char.is_symbol h ->
        ((acc, reified_position ()), h :: t)
    | [] -> ((acc, reified_position ()), [])
    | h :: t -> aux (acc ^ String.make 1 h) t
  in
  let (parsed, pos), remaining = aux "" chars in
  match List.assoc_opt parsed keywords with
  | Some token_type -> (Token (token_type, pos), remaining)
  | None -> (Token (IDENTIFIER_TOKEN parsed, pos), remaining)

let rec next_token chars =
  incr_col ();
  match chars with
  | h :: t when Char.is_whitespace h -> next_token t
  | '\n' :: t ->
      incr_row ();
      next_token t
  | '(' :: t -> (Token (LPAREN, reified_position ()), t)
  | ')' :: t -> (Token (RPAREN, reified_position ()), t)
  | '[' :: t -> (Token (LBRACKET, reified_position ()), t)
  | ']' :: t -> (Token (RBRACKET, reified_position ()), t)
  | '-' :: '>' :: t -> (Token (ARROW, reified_position ()), t)
  | '+' :: t -> (Token (PLUS, reified_position ()), t)
  | '-' :: t -> (Token (MINUS, reified_position ()), t)
  | '*' :: t -> (Token (TIMES, reified_position ()), t)
  | '/' :: t -> (Token (DIVISION, reified_position ()), t)
  | '%' :: t -> (Token (MOD, reified_position ()), t)
  | '=' :: ' ' :: t -> (Token (EQUALS, reified_position ()), t)
  | '<' :: ' ' :: t -> (Token (LESS_THAN, reified_position ()), t)
  | '"' :: t -> lex_string t
  | h :: t when not (Char.is_digit h || Char.is_whitespace h || Char.is_symbol h)
    ->
      lex_identifier_or_keyword (h :: t)
  | h :: t when Char.is_digit h -> lex_number (h :: t)
  | h :: t -> (Token (UNKNOWN h, reified_position ()), t)
  | [] ->
      let ret = (Token (EOF, reified_position ()), []) in
      reset_pos ();
      ret

let rec peek chars =
  match chars with
  | h :: t when Char.is_whitespace h -> peek t
  | '\n' :: t -> peek t
  | '(' :: _ -> Some LPAREN
  | ')' :: _ -> Some RPAREN
  | '[' :: _ -> Some LBRACKET
  | ']' :: _ -> Some RBRACKET
  | '-' :: '>' :: _ -> Some ARROW
  | '+' :: _ -> Some PLUS
  | '-' :: _ -> Some MINUS
  | '*' :: _ -> Some TIMES
  | '/' :: _ -> Some DIVISION
  | '%' :: _ -> Some MOD
  | '=' :: ' ' :: _ -> Some EQUALS
  | '<' :: ' ' :: _ -> Some LESS_THAN
  | '"' :: t -> peek_string t
  | h :: t when not (Char.is_digit h || Char.is_whitespace h || Char.is_symbol h)
    ->
      peek_identifier_or_keyword (h :: t)
  | h :: t when Char.is_digit h -> peek_number (h :: t)
  | h :: _ -> Some (UNKNOWN h)
  | [] -> None

and peek_string chars =
  let rec aux acc rem =
    match rem with
    | '"' :: _ -> Some (STRING_TOKEN acc)
    | h :: t -> aux (acc ^ String.make 1 h) t
    | [] -> failwith "unmatched string"
  in
  aux "" chars

and peek_number chars =
  let rec aux acc rem ~lexing_decimal_part =
    match rem with
    | h :: t when Char.is_digit h ->
        aux ((acc * 10) + Char.as_int h) t ~lexing_decimal_part
    | h :: t when h = '.' ->
        if not lexing_decimal_part then
          let integer_part = acc in
          let decimal_part = aux 0 t ~lexing_decimal_part:true in
          let float = string_of_int integer_part ^ "." ^ decimal_part in
          float
        else failwith "Error while lexig float"
    | _ -> string_of_int acc
  in
  let number = aux 0 chars ~lexing_decimal_part:false in
  Some (NUMBER_TOKEN (float_of_string number))

and peek_identifier_or_keyword chars =
  (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem =
    match rem with
    | h :: _ when Char.is_whitespace h || Char.is_symbol h -> acc
    | [] -> acc
    | h :: t -> aux (acc ^ String.make 1 h) t
  in
  let parsed = aux "" chars in
  match List.assoc_opt parsed keywords with
  | Some token_type -> Some token_type
  | None -> Some (IDENTIFIER_TOKEN parsed)
