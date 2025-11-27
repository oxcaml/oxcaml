[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Optimize all code sections with peephole optimizations.
    This includes the main code section and all additional sections,
    with profiling and counter tracking. *)
val optimize_all_sections :
  asm_line DLL.t ->
  asm_line DLL.t X86_section.Section_name.Tbl.t ->
  asm_line DLL.t X86_section.Section_name.Tbl.t ->
  unit
