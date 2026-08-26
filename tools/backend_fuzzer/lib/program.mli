type t

val create :
  functions:Ir.Function.t list ->
  main_decls:(Ir.Name.t * Ir.Ty.t * Ir.Expr.t) list ->
  main_statement:Ir.Statement.t ->
  t

val to_code : t -> string
