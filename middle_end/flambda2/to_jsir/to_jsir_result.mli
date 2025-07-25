(** Result structure used during Flambda to Js_of_ocaml IR translation. *)

(** An accumulator for the JSIR blocks and instructions.

    Values of type [t] store complete/archived blocks, as well as a stack of
    "current" blocks that is still being worked on. *)

(* CR selee: improve documentation *)

type t

(* CR selee: probably we will end up needing to store more info *)

(** Create a new result structure. It is not initialised with any blocks. *)
val create : unit -> t

(** Add a [Jsir.instr] to the top of the stack of current blocks and return the address of
    the new block. This function raises if there are no blocks being worked on. *)
val add_instr_exn : t -> Jsir.instr -> t

(** Push a new block to the stack of current blocks. *)
val new_block : t -> params:Jsir.Var.t list -> t * Jsir.Addr.t

(** End the block at the top of the current stack, setting [last] to the given argument.
    This function raises if there are no blocks being worked on. *)
val end_block_with_last_exn : t -> Jsir.last -> t

(** Create a [Jsir.program] with the blocks in the result, including the
    current block. This function raises if there are still blocks being
    worked on. *)
val to_program_exn : t -> Jsir.program
