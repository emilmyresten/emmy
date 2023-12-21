open Base

module Char = struct
  include Char

  let is_digit char =
    match Char.to_int char with
    | x when Poly.(x > 47 && x < 58) -> true
    | _ -> false

  let is_letter char =
    match Char.to_int char with
    | x when Poly.(x > 96 && x < 123) -> true
    | _ -> false

  let is_whitespace = function ' ' | '\n' | '\r' | '\t' -> true | _ -> false
  let is_symbol = function '(' | ')' | '[' | ']' -> true | _ -> false
  let as_int char = Char.to_int char - 48
end

module List = struct
  include List

  let overlap a b =
    List.filter ~f:(fun m -> List.exists a ~f:(fun a_elt -> Poly.(a_elt = m))) b

  let unique_right a b =
    List.filter
      ~f:(fun m -> not (List.exists a ~f:(fun a_elt -> Poly.(a_elt = m))))
      b
end

module String = struct
  include String

  let of_char_list c = String.concat ~sep:"" (List.map ~f:(String.make 1) c)
  let to_list str = String.to_list str
end
