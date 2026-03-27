[@@@ocaml.warning "+a-40-41-42"]

(** Reconstructed SSA information for a CFG.

    The CFG uses pseudo-registers that may be assigned multiple times. In
    practice, however, the CFGs we construct mostly follow SSA form. This module
    reconstructs which registers have a single static assignment (SSA) and
    tracks their defining instruction.

    A register is considered SSA if it has exactly one definition across all
    reachable blocks, or if its multiple definitions follow a phi-node pattern
    (each definition in a distinct predecessor of a common merge block).
    Preassigned (hardware) registers are never tracked. *)

type entry =
  | Output of
      { label : Label.t;
        instr : Cfg.basic Cfg.instruction;
        res_index : int
      }
  | OverwrittenOutput of
      { label : Label.t;
        instr : Cfg.basic Cfg.instruction;
        res_index : int;
        overwritten_input : Reg.t
      }
      (** Some architectures use 2-operand instructions where the result
          overwrites the first argument (e.g. [r := r xor y] on x86-64). The
          instruction selection emits a move into the register immediately
          before the operation:
          {|
            r := x       (* preceding move *)
            r := r xor y (* overwrites r; real input was x *)
          |}
          [OverwrittenOutput] records the overwriting instruction together with
          [overwritten_input] (here [x]), the source of the preceding move. The
          register is still SSA because the move and the operation form an
          atomic pair: no other code can observe the intermediate value. *)
  | Phi of
      { merge_label : Label.t;
        regs : Reg.t array
      }
      (** The register is assigned in all predecessor blocks of [merge_label].
          [regs] has one entry per predecessor (in [Label.Set] order): the
          register carrying the value from that predecessor. Only complete phis
          (where every predecessor has a definition) are retained; incomplete
          ones are demoted to [NotSSA]. *)
  | NotSSA

type t

val build : Cfg.t -> t

val find : t -> Reg.t -> entry option

val iter : t -> f:(Reg.t -> entry -> unit) -> unit

(** [is_ssa t reg] returns [true] if [reg] has a single static assignment
    (Output, OverwrittenOutput, or Phi). *)
val is_ssa : t -> Reg.t -> bool

(** [has_ssa_semantics_at t cfg ~at_block reg] checks whether [reg] has an SSA
    value and if using [reg] in block [block] coincides with SSA semantics. This
    does *NOT* check for dominance. Instead, the one case this filters out is
    that we have a phi node and [block] is one of the predecessor blocks of this
    phi containing an assignment to [reg]. The problem is that in SSA semantics,
    the assignment happens on the control flow edge and is not observable in the
    predecessor block. In our CFG, however, the assignment is an explicit
    instruction and we could well observe the register after the assignment
    while still in the predecessor block, for example in a terminator. *)
val has_ssa_semantics_at : t -> Cfg.t -> at_block:Label.t -> Reg.t -> bool
