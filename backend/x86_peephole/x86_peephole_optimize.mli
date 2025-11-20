[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Optimize a single asm_program with peephole optimizations.
    Updates the stats parameter with counts of applied rules. *)
val peephole_optimize_asm_program :
  X86_peephole_rules.peephole_stats -> asm_line DLL.t -> unit
