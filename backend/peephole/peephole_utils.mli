[@@@ocaml.warning "+a-29-40-41-42"]

module DLL = Doubly_linked_list

val are_equal_regs : Reg.t -> Reg.t -> bool

(** [debuginfo_allows_merging fst snd] is [true] when two instructions carrying
    debug info [fst] and [snd] may be replaced by a single one, which can only
    carry one of them: the source location of the other one is then no longer
    reachable by a debugger. This is always allowed unless the user has asked
    for debugging to take precedence over code generation
    ([-gdwarf-may-alter-codegen]), in which case the debug info must be
    identical (as in [Cfg_merge_blocks]). *)
val debuginfo_allows_merging : Debuginfo.t -> Debuginfo.t -> bool

val go_back_const : int

val prev_at_most : int -> 'a DLL.cell -> 'a DLL.cell

val get_cells :
  Cfg.basic Cfg.instruction DLL.cell ->
  int ->
  Cfg.basic Cfg.instruction DLL.cell list

(** The following functions check for overflow and ranges of immediates w.r.t.
    the operation and optionally rewrite the operation. *)
val add_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val sub_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val mul_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

(** [lsl_immediates op imm1 imm2] rewrites [imm1 lsl imm2] as an immediate for
    [op]. [imm1] must be within range for [op] and [imm2] within range for
    [Ilsl]. *)
val lsl_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (Operation.integer_operation * int) option

val bitwise_immediates :
  Operation.integer_operation ->
  int ->
  int ->
  (int -> int -> int) ->
  (Operation.integer_operation * int) option

val assert_within_range : Operation.integer_operation -> int -> unit

val is_immediate_for_intop : Operation.integer_operation -> int -> bool
