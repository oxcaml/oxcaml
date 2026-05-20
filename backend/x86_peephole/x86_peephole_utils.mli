[@@@ocaml.warning "+a-30-40-41-42"]

open X86_ast
module DLL = Oxcaml_utils.Doubly_linked_list

(** Type for the result of applying a peephole rewrite rule *)
type rule_result =
  | No_match
  | Matched of asm_line DLL.cell option

(** Navigation and inspection *)

val get_cells : asm_line DLL.cell -> int -> asm_line DLL.cell list

val is_hard_barrier : asm_line -> bool

val is_control_flow : instruction -> bool

(** Register operations *)

val is_register : arg -> bool

val underlying_reg64 : arg -> reg64 option

(** Instruction analysis *)

val writes_to_reg64 : reg64 -> instruction -> bool

val reads_from_reg64 : reg64 -> instruction -> bool

val writes_flags : instruction -> bool

(** Liveness analysis *)

val reg64_is_never_read : reg64 -> asm_line DLL.cell -> bool
