module Char = struct
  include Char
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
  
  let is_symbol = function
    | '(' | ')' | '+' | '-' | '*' -> true
    | _ -> false

  let as_int char = int_of_char char - 48
end