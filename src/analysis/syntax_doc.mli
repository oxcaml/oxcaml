val get_syntax_doc :
  Lexing.position ->
  (Env.t * Browse_raw.node) list ->
  Query_protocol.Syntax_doc_result.t option
