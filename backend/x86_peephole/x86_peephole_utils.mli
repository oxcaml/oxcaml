[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Type for the result of searching for the next occurrence of a register *)
type next_occurrence =
  | WriteFound
  | ReadFound
  | NotFound

(** Equality functions for x86_ast types *)

val equal_reg64 : reg64 -> reg64 -> bool

val equal_reg8h : reg8h -> reg8h -> bool

val equal_regf : regf -> regf -> bool

val equal_arch : arch -> arch -> bool

val equal_data_type : data_type -> data_type -> bool

val equal_addr : addr -> addr -> bool

val equal_args : arg -> arg -> bool

(** Navigation and inspection *)

val get_cells : asm_line DLL.cell -> int -> asm_line DLL.cell list

val is_hard_barrier : asm_line -> bool

val is_control_flow : instruction -> bool

(** Register operations *)

val is_register : arg -> bool

val is_safe_self_move_arg : arg -> bool

val is_safe_for_dead_register_opt : arg -> bool

val underlying_reg64 : arg -> reg64 option

val registers_alias : arg -> arg -> bool

val reg_appears_in_arg : arg -> arg -> bool

val reg_is_written_by_arg : arg -> arg -> bool

val reg_in_memory_address : arg -> arg -> bool

(** Instruction analysis *)

val writes_to_arg : arg -> instruction -> bool

val reads_from_arg : arg -> instruction -> bool

val reads_flags : instruction -> bool

val writes_flags : instruction -> bool

(** Liveness analysis *)

val find_next_occurrence_of_register :
  arg -> asm_line DLL.cell -> next_occurrence

val find_next_flag_use : asm_line DLL.cell -> next_occurrence
