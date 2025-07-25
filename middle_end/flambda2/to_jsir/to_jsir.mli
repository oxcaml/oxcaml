(** Translate a compilation unit. *)
val unit :
  offsets:Exported_offsets.t ->
  all_code:Exported_code.t ->
  reachable_names:Name_occurrences.t ->
  Flambda_unit.t ->
  Jsir.program
