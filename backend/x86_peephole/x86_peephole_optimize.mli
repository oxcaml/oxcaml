[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Optimize starting from a given cell, walking forward through the DLL. *)
val optimize_from_cell : asm_line DLL.cell option -> unit
