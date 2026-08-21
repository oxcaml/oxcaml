val get_mode_doc :
  Mode.Alloc.atom -> Query_protocol.Syntax_doc_result.t option

val get_modality_doc :
  Mode.Modality.atom -> Query_protocol.Syntax_doc_result.t option

val get_syntax_doc :
  Lexing.position ->
  (Env.t * Browse_raw.node) list ->
  Query_protocol.Syntax_doc_result.t option
