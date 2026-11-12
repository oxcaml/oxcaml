[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(** Recognize heap allocation constructions contained within one SSA basic
    block, as emitted by [Ssa_of_cmm]. This analysis does not modify the graph
    or establish that a construction can be moved. *)

open Ssa.Export

type t = private
  { allocation : finished Instruction.op_data;
    initialization : finished Instruction.op_data list
        (** Header and field stores, and address-reset instructions, in program
            order. Independent field computations and debugger markers are not
            included. *)
  }

(** Constructions in program order. A construction must initialize its header
    and its field slots sequentially, before another effect, trap instruction,
    terminator, or non-debug use of its address other than initialization.
    Unsupported and incomplete constructions are omitted. Float32 padding
    follows [Ssa_of_cmm.emit_stores]. *)
val find_in_block : finished Block.t -> t list
