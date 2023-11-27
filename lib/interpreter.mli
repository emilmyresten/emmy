val eval : Expressions.expression -> (string * Expressions.expression) list -> Expressions.expression * (string * Expressions.expression) list

val eval_program : string -> (Expressions.expression * (string * Expressions.expression) list)