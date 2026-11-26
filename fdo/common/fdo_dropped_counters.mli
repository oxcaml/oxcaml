(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** FDO counters dropped by optimizations.

    A counter is dropped when an optimization removes the branch or call it is
    attached to while keeping the underlying code (for example, the entry
    counter of an inlined function). Because no instruction remains to measure
    it, the counter's execution count is unknown rather than zero.

    The counters of code removed as unreachable (e.g. the edges of a test that
    are never taken, or blocks that dead code elimination removes) are not
    dropped: they would never count.

    Dropped counters are recorded with the reason they were dropped and
    registered per compilation unit for reporting. *)

module Reason : sig
  type t =
    | Inlined_call
        (** Entry counters of a function whose call site was inlined. *)
    | Inlined_call_without_counter
        (** Entry counters of a function inlined at a call without a counter. *)
    | Self_tail_call_to_jump
        (** Counter of a self tail call converted to a jump. *)
    | Switch_to_single_arm
        (** Counters of a switch reduced to a single reachable arm. *)
    | Switch_with_identical_arms
        (** Counters of a switch whose arms all jump to the same action. *)
    | Switch_to_lookup_table
        (** Counters of a switch lowered to a lookup table. *)
    | Cmm_switch_collapsed
        (** The counters of the cases of a switch that the translation to Cmm
            turns into a table lookup or a single case. *)
    | Probe_handler_call  (** The counter of the call of a probe handler. *)
    | Cfg_self_tail_call_to_jump
        (** The counter of a self tail call that instruction selection turns
            into a jump. *)
    | Cfg_branch_to_jump
        (** The counters of the edges to its target of a branch that a jump
            replaces because its outcome is known, e.g. when all its edges go to
            the same block. When the jump is that of a predecessor, the branch
            keeps its counters if other code still reaches it, but they miss the
            executions of that predecessor. *)
    | Cfg_blocks_merged
        (** The counters of a block that is merged into an identical one. *)
    | Ssa_self_tail_call_to_jump
        (** The counter of a self tail call that [Ssa_tail_call] turns into a
            jump. *)
    | Linear_branch_to_jump
        (** The counters of the edges of a branch that is linearized to a jump
            or a fallthrough. *)
    | Linear_branch_relaxed
        (** The counters of a conditional branch too far from its target, which
            [Branch_relaxation] replaces by branches without counters. *)

  val to_string : t -> string
end

(* Simplify also loses some counts by conflating counters rather than dropping
   them, which this module does not record. *)

(* CR-someday yusumez: When Simplify runs again on a function of a copy, it does
   not specialize the counters of the sets of closures in the body, as the
   previous run already did. But if the previous run kept a function of such a
   set as the original's code (because simplifying it brought no improvements),
   this run can produce new code for it that still has the original's counters,
   so its counts are merged with the original's. *)

(* CR-someday yusumez: The body of a function inlined at a call without a
   counter (e.g. the second application of a split over-application) gets no
   inlining context, so its counts are merged with those of the callee's
   out-of-line code (see [Inlined_call_without_counter]). *)

(* CR-someday yusumez: A self tail call that becomes a jump still runs once per
   call, and so does the jump to the tail recursion entry point that replaces
   it. Counting that jump as the call would keep the counter instead of dropping
   it ([Cfg_self_tail_call_to_jump], [Ssa_self_tail_call_to_jump]).
   [Tailcall_self] in Cfg would need a counter, and so would the jump to the
   entry block that [Ssa_tail_call] makes in SSA.

   The self tail calls that Simplify turns into loops ([Self_tail_call_to_jump])
   become back edges to a continuation instead: keeping their counters would
   need counters on jumps ([Apply_cont], then [Cexit]). *)

(* CR-someday yusumez: Most counters are dropped because the branch or call they
   are on goes away while the code it leads to stays (e.g. a branch whose
   outcome is known, or an inlined call). A primitive that marks a counter at a
   point of straight-line code would let it keep a count.

   Each LBR entry is a taken branch and its target, and the code from that
   target to the next taken branch runs straight through, so the profile counts
   every address in such a range, not only branches and calls.

   The marker must generate no instruction (only a label in the metadata) and
   must not stop optimizations, such as merging blocks, or the code would differ
   from the code without counters. *)

(** Dropped counters, mapped to the reason each was first dropped. *)
type t

val empty : t

val add : t -> reason:Reason.t -> Fdo_annotation.t -> t

val union : t -> t -> t

(** In the order of the counters. *)
val to_list : t -> (Fdo_counter.t * Reason.t) list

(** One counter per line, with its reason. *)
val print : Format.formatter -> t -> unit

(** {2 The counters dropped in the current compilation unit} *)

val register : reason:Reason.t -> Fdo_annotation.t -> unit

val register_all : t -> unit

val registered : unit -> t

(** To be called before compiling a unit. *)
val reset : unit -> unit

(** [f ()], without registering the counters it drops: for code that is built
    and not compiled, e.g. for a comparison. *)
val without_registering : (unit -> 'a) -> 'a
