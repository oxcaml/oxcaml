type t

val create :
  swarm:Config.Swarm.t ->
  record_types:Ir.Ty.record list ->
  functions:Ir.Function.t list ->
  toplevel_decls:(Ir.Binding.t * Ir.Expr.t) list ->
  toplevel_statement:Ir.Statement.t ->
  t

val to_code : t -> string
