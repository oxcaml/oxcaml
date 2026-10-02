[@@@ocaml.warning "+a-40-41-42"]

(** The function's contribution to the call graph profile for the linker (see
    [Fdo_call_graph]): every real call instruction in a block with a positive
    count, weighted by that count, to the functions the profile saw the call
    site reach, or to its static callee when the profile knows nothing about it.
    When [dump] is provided, the edges are printed to it (the [-dfdo] flag). *)
val record :
  dump:Format.formatter option ->
  Source_position_profile.t ->
  Cfg_fdo_counts.t ->
  Cfg.t ->
  unit
