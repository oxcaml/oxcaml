(** Translate a compilation unit. *)
val unit :
  offsets:Flambda2_simplify_shared.Exported_offsets.t ->
  all_code:Flambda2_cmx.Exported_code.t ->
  reachable_names:Flambda2_nominal.Name_occurrences.t ->
  Flambda2_terms.Flambda_unit.t ->
  Jsir.program
