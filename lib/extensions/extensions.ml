module Char = struct
  include Char

  let is_digit char =
    match int_of_char char with x when x > 47 && x < 58 -> true | _ -> false

  let is_letter char =
    match int_of_char char with x when x > 96 && x < 123 -> true | _ -> false

  let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false
  let is_symbol = function '(' | ')' | '[' | ']' -> true | _ -> false
  let as_int char = int_of_char char - 48
end

module List = struct
  include List

  let overlap a b = List.filter (fun m -> List.mem m a) b
  let unique_right a b = List.filter (fun m -> not (List.mem m a)) b
end

module String = struct
  include String

  let of_char_list c = String.concat "" (List.map (String.make 1) c)
  let to_list str = String.to_seq str |> List.of_seq
end
