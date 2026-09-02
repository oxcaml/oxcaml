val impl_source_of_interface : Mconfig.t -> string -> string option
val own_file : Mconfig.t -> string

val query :
  pipeline:Mpipeline.t ->
  ?position:Lexing.position ->
  Mtyper.typedtree ->
  Query_protocol.Module_type_impls.response
