[@@@ocaml.warning "+a-40-41-42"]

module DLL = Doubly_linked_list

(** Statistics for peephole optimizations. *)
type peephole_stats

val create_peephole_stats : unit -> peephole_stats

val peephole_stats_to_counters : peephole_stats -> Profile.Counters.t

(** Apply all peephole rewrite rules to a cell. Returns [Matched] with a
    continuation cell if a rule was applied, [No_match] otherwise. *)
val apply :
  peephole_stats ->
  Peephole_utils.asm_line DLL.cell ->
  Peephole_utils.rule_result
