(** The modules that must {e implement} each module-type declaration of the
    buffer (or the single declaration enclosing [position]), computed from the
    compiler facts in the configured indexes. *)
val query :
  ?position:Lexing.position ->
  Mpipeline.t ->
  Query_protocol.Module_type_impls.response
