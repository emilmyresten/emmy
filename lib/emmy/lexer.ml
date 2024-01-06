open Base
open Extensions
open Tokens

let current_position : current_position = { row = 0; col = -1 }

let incr_row () =
  current_position.row <- current_position.row + 1;
  current_position.col <- -1

let incr_col () = current_position.col <- current_position.col + 1

let reset_pos () =
  current_position.row <- 0;
  current_position.col <- -1

let reify_position () =
  { row = current_position.row; col = current_position.col }

let lex_number chars =
  let rec aux acc rem ~lexing_decimal_part =
    incr_col ();
    match rem with
    | h :: t when Char.is_digit h ->
        aux ((acc * 10) + Char.as_int h) t ~lexing_decimal_part
    | h :: t when Poly.(h = '.') ->
        if not lexing_decimal_part then
          let integer_part = acc in
          let decimal_part, rem = aux 0 t ~lexing_decimal_part:true in
          let float = Int.to_string integer_part ^ "." ^ decimal_part in
          (float, rem)
        else failwith "Error while parsing float"
    | _ -> (Int.to_string acc, rem)
  in
  let number, rem = aux 0 chars ~lexing_decimal_part:false in
  ( { kind = NUMBER_TOKEN (Float.of_string number); pos = reify_position () },
    rem )

let lex_string chars =
  let rec aux acc rem =
    incr_col ();
    match rem with
    | '"' :: t -> ({ kind = STRING_TOKEN acc; pos = reify_position () }, t)
    | h :: t -> aux (acc ^ String.make 1 h) t
    | [] -> failwith "unmatched string"
  in
  aux "" chars

let lex_identifier_or_keyword chars =
  (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem =
    incr_col ();
    match rem with
    | h :: t when Char.is_whitespace h || Char.is_symbol h ->
        ((acc, reify_position ()), h :: t)
    | [] -> ((acc, reify_position ()), [])
    | h :: t -> aux (acc ^ String.make 1 h) t
  in
  let (parsed, pos), remaining = aux "" chars in
  match List.Assoc.find keywords ~equal:String.equal parsed with
  | Some token_kind -> ({ kind = token_kind; pos }, remaining)
  | None -> ({ kind = IDENTIFIER_TOKEN parsed; pos }, remaining)

let rec next_token chars =
  incr_col ();
  match chars with
  | '\n' :: t ->
      incr_row ();
      next_token t
  | h :: t when Char.is_whitespace h -> next_token t
  | '(' :: t -> ({ kind = LPAREN; pos = reify_position () }, t)
  | ')' :: t -> ({ kind = RPAREN; pos = reify_position () }, t)
  | '[' :: t -> ({ kind = LBRACKET; pos = reify_position () }, t)
  | ']' :: t -> ({ kind = RBRACKET; pos = reify_position () }, t)
  | '-' :: '>' :: t -> ({ kind = ARROW; pos = reify_position () }, t)
  | '+' :: t -> ({ kind = PLUS; pos = reify_position () }, t)
  | '-' :: t -> ({ kind = MINUS; pos = reify_position () }, t)
  | '*' :: t -> ({ kind = TIMES; pos = reify_position () }, t)
  | '/' :: t -> ({ kind = DIVISION; pos = reify_position () }, t)
  | '%' :: t -> ({ kind = MOD; pos = reify_position () }, t)
  | '=' :: ' ' :: t -> ({ kind = EQUALS; pos = reify_position () }, t)
  | '<' :: ' ' :: t -> ({ kind = LESS_THAN; pos = reify_position () }, t)
  | '"' :: t -> lex_string t
  | h :: t when not (Char.is_digit h || Char.is_whitespace h || Char.is_symbol h)
    ->
      lex_identifier_or_keyword (h :: t)
  | h :: t when Char.is_digit h -> lex_number (h :: t)
  | h :: t -> ({ kind = UNKNOWN h; pos = reify_position () }, t)
  | [] ->
      let ret = ({ kind = EOF; pos = reify_position () }, []) in
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
    | h :: t when Poly.(h = '.') ->
        if not lexing_decimal_part then
          let integer_part = acc in
          let decimal_part = aux 0 t ~lexing_decimal_part:true in
          let float = Int.to_string integer_part ^ "." ^ decimal_part in
          float
        else failwith "Error while lexig float"
    | _ -> Int.to_string acc
  in
  let number = aux 0 chars ~lexing_decimal_part:false in
  Some (NUMBER_TOKEN (Float.of_string number))

and peek_identifier_or_keyword chars =
  (* allow every symbol except white-space in identifier. *)
  let rec aux acc rem =
    match rem with
    | h :: _ when Char.is_whitespace h || Char.is_symbol h -> acc
    | [] -> acc
    | h :: t -> aux (acc ^ String.make 1 h) t
  in
  let parsed = aux "" chars in
  match List.Assoc.find keywords ~equal:String.equal parsed with
  | Some token_type -> Some token_type
  | None -> Some (IDENTIFIER_TOKEN parsed)
