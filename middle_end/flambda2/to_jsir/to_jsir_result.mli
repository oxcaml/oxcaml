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

(** Add debugging information as an [Event]. Raises if there are no blocks being worked
    on. [pos] controls whether we log the start of the region or the end. *)
val add_debuginfo_exn : t -> Debuginfo.t -> pos:[`Start | `End] -> t

(** Push a new block to the stack of current blocks. *)
val new_block : t -> params:Jsir.Var.t list -> t * Jsir.Addr.t

(** Return a new address without actually creating a block corresponding to that address.
    This is used for recursive continuations, where we need to use the address of a
    continuation before actually translating the body. The caller should promise that they
    call [new_block_with_addr] with the returned address at some point before
    [to_program_exn] is called. *)
val reserve_address : t -> t * Jsir.Addr.t

(** Make a new block, but using [addr]. Raises if [addr] is not a reserved address. *)
val new_block_with_addr_exn :
  t -> params:Jsir.Var.t list -> addr:Jsir.Addr.t -> t

(** End the block at the top of the current stack, setting [last] to the given argument.
    This function raises if there are no blocks being worked on. *)
val end_block_with_last_exn : t -> Jsir.last -> t

(** Create a [Jsir.program] with the blocks in the result, including the
    current block. This function raises if there are still blocks being
    worked on, or there is a reserved address that has not been used yet. *)
val to_program_exn : t -> Jsir.program

(** Returns the address for a special block for invalid switches, creating one if it
    doesn't exist already. *)
val invalid_switch_block : t -> t * Jsir.Addr.t
