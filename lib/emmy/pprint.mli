val string_of_expr : Expressions.expression -> string
val string_of_expr_list : Expressions.expression list -> string
val string_of_context : (string * Expressions.expression) list -> string
val string_of_bindings : (string * Expressions.expression) list -> string
val string_of_program : Expressions.program -> string
val string_of_dependency_graph : (string, string list) Base.Hashtbl.t -> string
