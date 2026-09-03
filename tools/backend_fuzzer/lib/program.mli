type t

val create :
  functions:Ir.Function.t list ->
  toplevel_decls:(Ir.Name.t * Ir.Ty.t * Ir.Expr.t) list ->
  toplevel_statement:Ir.Statement.t ->
  t

val to_code : t -> string
