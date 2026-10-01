(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The current ("v2") code size model: an approximation of the size of the
   machine code generated for Flambda terms. [Code_size] dispatches between this
   model and the original one ([Code_size_v1]) according to
   [-flambda2-code-size-model]; the representation of sizes defined here is the
   one stored in terms whichever model is selected.

   Sizes are measured in machine instructions rather than bytes. Instruction
   lengths vary widely on x86-64 (typically from one to ten bytes, averaging
   four to five in OCaml-generated code) but the length of a particular
   instruction depends on details that are unknown at this stage, such as which
   registers are chosen (REX prefixes), the width of immediates and the distance
   of branches, so counting bytes would add little real precision. Instruction
   counts also keep these sizes on the same scale as the inlining thresholds and
   cost weights in [Inlining_arguments] (and as the [-inline] flag), all of
   which were calibrated against Closure's instruction-like counts. Four-byte
   jump table entries are counted as one instruction each.

   The estimates depend on the target architecture (x86-64 or arm64, as given by
   [Target_system.architecture]), since for example arm64 needs more
   instructions for tagging, for indexed memory accesses and for allocation. See
   the comments in the implementation for the instruction sequences that each
   estimate is based on. *)

(** Values of type [t] may be negative. A value holds the estimates for both
    x86-64 and arm64, so that Flambda terms (and dumps of them) are independent
    of the target; comparisons use the estimate for the target architecture. *)
type t

val create : x86_64:int -> arm64:int -> t

val x86_64 : t -> int

val arm64 : t -> int

(** Add the cost of the prologue and epilogue of a function whose body has the
    given size, if that body needs a stack frame (because it contains a call, an
    allocation or a poll). The result is a standalone function size: its frame
    requirements and allocation context do not propagate into enclosing code. *)
val add_function_frame : t -> t

(* Both are only there temporarly *)
val of_int : int -> t

val to_int : t -> int

val zero : t

(** The size of two pieces of code whose relative placement is unknown. *)
val ( + ) : t -> t -> t

(** [seq a b] is the size of the code [a] followed, in the same basic block, by
    the code [b] (for example the defining expression of a [Let] and its body).
    Unlike [a + b], this accounts for the combination of an allocation at the
    end of [a] with one at the start of [b]. *)
val seq : t -> t -> t

(** [with_out_of_line t ~out_of_line] is the size of the code [t] together with
    code that is placed elsewhere (for example the handlers of continuations
    bound around [t]), which does not affect how allocations in [t] can be
    combined with code before or after it. *)
val with_out_of_line : t -> out_of_line:t -> t

val ( - ) : t -> t -> t

val ( <= ) : t -> t -> bool

val equal : t -> t -> bool

val print : Format.formatter -> t -> unit

val box_number :
  machine_width:Target_system.Machine_width.t ->
  Flambda_kind.Boxable_number.t ->
  t

val block : int -> t

val array : int -> t

val prim :
  machine_width:Target_system.Machine_width.t -> Flambda_primitive.t -> t

val simple : Simple.t -> t

val static_consts : unit -> t

(** [is_tail] should be [true] when the application is in tail position of the
    enclosing function (return and exception continuations being those of the
    function), in which case it will be compiled as a jump. *)
val apply : is_tail:bool -> Apply_expr.t -> t

val apply_cont : Apply_cont_expr.t -> t

val switch : Switch_expr.t -> t

val invalid : t

val evaluate : args:Inlining_arguments.t -> t -> float

(** The size of the allocation of a set of closures that needs [num_stores]
    stores, including that of the header. *)
val set_of_closures_allocation : num_stores:int -> t

(** The code needed, in the module initialiser, to initialise one field of a
    statically allocated block from a variable ([pointer] says whether the field
    may hold a pointer). *)
val static_field_initialization : pointer:bool -> t
