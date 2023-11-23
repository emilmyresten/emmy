val eval : Expressions.expression -> (string * Expressions.expression) list -> Expressions.expression * (string * Expressions.expression) list

val string_of_val : Expressions.expression -> string

val print_context : (string * Expressions.expression) list -> unit