type t

val create :
  swarm:Config.Swarm.t ->
  record_types:Ir.Ty.record list ->
  exceptions:Ir.Exn.t list ->
  variant_types:Ir.Ty.variant list ->
  functions:Ir.Function.t list ->
  toplevel_decls:(Ir.Binding.t * Ir.Expr.t) list ->
  toplevel_statement:Ir.Statement.t ->
  t

(** In [Config.May_raise] mode, reports termination and initialized top-level
    state. Other modes leave exceptions uncaught. *)
val to_code : t -> string
