[@@@ocaml.warning "+a-30-40-41-42"]

open Regalloc_utils

module type State = sig
  type t

  val stack_slots : t -> Regalloc_stack_slots.t
end

module type Utils = sig
  val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

  val indent : unit -> unit

  val dedent : unit -> unit

  val log_body_and_terminator :
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit
end

(* This is the `rewrite` function from IRC, parametrized by state, functions for
   debugging, and function to test/set the "spilled" state of a register. It
   inserts spills and reloads for registers in the [spilled_nodes] parameter
   (thus basically corresponding to Upstream's [Reload] pass). The returned
   triple contains the list of introduced instruction temporaries (temporaries
   created for the use of a spilled node in a single instruction), the list of
   block temporaries (hold the value of a spilled node for a whole CFG block),
   and a boolean which is `true` iff at least one block was inserted. If the
   [block_temporaries] parameter is set to false, the list of block temporaries
   will be empty. *)
val rewrite_gen :
  (module State with type t = 's) ->
  (module Utils) ->
  's ->
  Cfg_with_infos.t ->
  spilled_nodes:Reg.t list ->
  block_temporaries:bool ->
  Reg.t list * Reg.t list * bool

(* Runs the first steps common to register allocators, reinitializing registers,
   checking preconditions, and collecting information from the CFG. *)
val prelude :
  (module Utils) ->
  on_fatal_callback:(unit -> unit) ->
  Cfg_with_infos.t ->
  cfg_infos * Regalloc_stack_slots.t * Regalloc_affinity.t

(* Inserts dummy temporary uses, so that temporaries which are written but not
   read are live. If such temporaries are never live, then they don't "interact"
   with any other temporary and register allocators may assign any hardware
   register to them. No instructions will be emitted for dummy uses. *)
val insert_dummy_uses : Cfg_with_infos.t -> cfg_infos -> unit

(* Runs the last steps common to register allocators, updating the CFG (stack
   slots, live fields, and prologue), running [f], and checking
   postconditions. *)
val postlude :
  (module State with type t = 's) ->
  (module Utils) ->
  's ->
  f:(unit -> unit) ->
  Cfg_with_infos.t ->
  unit
