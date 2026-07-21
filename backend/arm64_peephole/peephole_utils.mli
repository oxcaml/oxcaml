[@@@ocaml.warning "+a-40-41-42"]

open Arm64_ast.Ast
module DLL = Doubly_linked_list

(** A single line of assembly: either an instruction or a directive (which
    includes labels). *)
type asm_line =
  | Ins of Instruction.t
  | Directive of Asm_targets.Asm_directives.Directive.t

(** Type for the result of applying a peephole rewrite rule. *)
type rule_result =
  | No_match
  | Matched of asm_line DLL.cell option

module Gp : sig
  (** Normalized general-purpose register, resolving the different assembly
      spellings of the same physical register: [lr] normalizes to [Reg64 30] and
      [fp] to [Reg64 29]. *)
  type t =
    | Reg64 of int
    | Reg32 of int
    | Zr64
    | Zr32
    | Sp

  val of_reg : [`GP of 'a] Reg.t -> t

  (** Exact equality, distinguishing [wN] from [xN]. *)
  val equal : t -> t -> bool

  (** Whether two register names overlap the same physical register: [wN] and
      [xN] are the same register, while the zero registers alias nothing. *)
  val same_underlying : t -> t -> bool
end

val directive_is_barrier : Asm_targets.Asm_directives.Directive.t -> bool

(** Whether rewrite rules must not look past the given line: labels and a few
    other directives, control flow, and memory-ordering instructions. *)
val is_hard_barrier : asm_line -> bool

val is_conditional_branch : Instruction.t -> bool

(** Conservative: [true] for control flow. *)
val writes_flags : Instruction.t -> bool

(** Whether the instruction may write to (a register overlapping) any of [regs].
    Conservative: [true] for control flow. *)
val writes_any_gp_reg : Instruction.t -> regs:Gp.t list -> bool

(** The next instruction cell, looking through non-barrier directives such as
    [Loc] and CFI directives. Returns [None] if a hard-barrier directive (in
    particular a label) is reached first. *)
val next_instruction : asm_line DLL.cell -> asm_line DLL.cell option

(** Decode a base-plus-immediate addressing mode (including the plain
    base-register form) to the base register and byte offset. Returns [None] for
    symbolic, literal and pre/post-indexed forms. *)
val decode_simple_mem :
  [`Mem of Addressing_mode.single] Operand.t -> (Gp.t * int) option

(** Delete the instruction following the given cell (as found by
    [next_instruction]), as well as any [Loc] directives before it. *)
val delete_next_instruction_and_locs : asm_line DLL.cell -> unit
