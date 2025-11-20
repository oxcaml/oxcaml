[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Optimize a single asm_program with peephole optimizations.
    Updates the stats parameter with counts of applied rules. *)
val peephole_optimize_asm_program :
  X86_peephole_rules.peephole_stats -> asm_line DLL.t -> unit

(** Optimize all code sections with peephole optimizations.
    This includes the main code section and all additional sections,
    with profiling and counter tracking.

    @param main_code The main code section to optimize
    @param asm_code_by_section Hash table of additional code sections
    @param delayed_sections Hash table of delayed code sections *)
val optimize_all_sections :
  asm_line DLL.t ->
  asm_line DLL.t X86_section.Section_name.Tbl.t ->
  asm_line DLL.t X86_section.Section_name.Tbl.t ->
  unit
