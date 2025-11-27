[@@@ocaml.warning "+a-29-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Statistics for peephole optimizations *)
type peephole_stats

val create_peephole_stats : unit -> peephole_stats

val peephole_stats_to_counters : peephole_stats -> Profile.Counters.t

(** Apply all peephole rewrite rules to a cell.
    Returns Matched with continuation cell if a rule was applied, No_match otherwise. *)
val apply :
  peephole_stats -> asm_line DLL.cell -> X86_peephole_utils.rule_result
