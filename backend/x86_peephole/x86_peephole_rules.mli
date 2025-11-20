[@@@ocaml.warning "+a-29-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Statistics for peephole optimizations *)
type peephole_stats =
  { mutable remove_mov_x_x : int;
    mutable remove_useless_mov : int;
    mutable remove_mov_chain : int;
    mutable remove_mov_to_dead_register : int;
    mutable rewrite_mov_sequence_to_xchg : int;
    mutable rewrite_mov_add_to_lea : int;
    mutable rewrite_mov_add_reg_to_lea : int;
    mutable remove_redundant_cmp : int;
    mutable combine_add_rsp : int
  }

val create_peephole_stats : unit -> peephole_stats

val peephole_stats_to_counters : peephole_stats -> Profile.Counters.t

(** Apply all peephole rewrite rules to a cell.
    Returns Some continuation_cell if a rule was applied, None otherwise. *)
val apply :
  peephole_stats -> asm_line DLL.cell -> asm_line DLL.cell option option
