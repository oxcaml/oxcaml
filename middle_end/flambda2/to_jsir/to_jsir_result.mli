(** Result structure used during Flambda to Js_of_ocaml IR translation. *)

(** An accumulator for the JSIR blocks and instructions.

    Values of type [t] store complete/archived blocks, as well as a "current"
    block that is still being worked on. *)

(* CR selee: improve documentation *)

type t

(** Create a new result structure. It is initialised with a block
    with no params (CR selee ???) *)

(* CR selee: probably we will end up needing to store more info *)
val create : unit -> t

(** Add a [Jsir.instr] to the current block. *)
val add_instr : t -> Jsir.instr -> t

(** Set the continuation of the current block to be the given [Jsir.last]. *)
val set_last : t -> Jsir.last -> t

(** Create a new block with the given params, and set it to be current.

    The current block before this call will be archived and cannot be modified
    further. This function raises if [set_branch] has not been set for the
    current block. *)
val new_block_exn : t -> Jsir.Var.t list -> t

(** Create a [Jsir.program] with the blocks in the result, including the
    current block. This function raises if [set_branch] has not been set for
    the current block. *)
val to_program_exn : t -> Jsir.program
