val eval :
  Expressions.ast_node ->
  (string * Expressions.ast_node) list ->
  Expressions.ast_node * (string * Expressions.ast_node) list

val eval_program :
  string ->
  (string * Expressions.ast_node) list ->
  Expressions.ast_node * (string * Expressions.ast_node) list
