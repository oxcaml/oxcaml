[@@@ocaml.warning "+a-40-41-42"]

(** Pure decision logic over signed integer comparisons, shared by the SSA loop
    analyses (termination, linear-function test replacement, and the guard-fact
    collection feeding bounds-check elimination).

    This module knows nothing about SSA: it depends only on
    {!Cmm.integer_comparison} and {!Fourier_motzkin.Affine}. Every function is
    over a finite domain, so its intended semantics can be checked by exhaustive
    enumeration (or proved outright). Only one direction of each result is
    load-bearing for the passes: [true] from {!continue_terminates} (and
    friends) licenses a rewrite, while [false] is always conservative. *)

module Affine = Fourier_motzkin.Affine

(** Monotonic direction of a constant-step induction variable. *)
type direction =
  | Up
  | Down

(** [direction_of_step s] is the direction of an IV with signed constant step
    [s] per iteration, or [None] for a zero step. *)
val direction_of_step : int -> direction option

(** Whether [cmp] is a signed order comparison ([<], [<=], [>], [>=]). *)
val is_signed_order : Cmm.integer_comparison -> bool

(** The comparison asserted by the loop's continue edge, normalised so that the
    induction variable is the left operand. [iv_is_left] says which side of
    [cmp] the IV was on; [continue_when_true] says whether the loop continues on
    the true edge of the test. *)
val oriented_continue_comparison :
  iv_is_left:bool ->
  continue_when_true:bool ->
  Cmm.integer_comparison ->
  Cmm.integer_comparison

(** [continue_terminates dir cmp] is [true] iff an IV progressing monotonically
    in direction [dir], compared by [cmp] (oriented IV-left) against a
    loop-invariant bound as the loop's continue-condition, must eventually
    falsify the condition. Unsigned comparisons and [Cne] return [false]
    (unknown). *)
val continue_terminates : direction -> Cmm.integer_comparison -> bool

(** Whether the (IV-left, oriented) continue-condition keeps the IV below a
    loop-invariant upper bound, i.e. is [<] or [<=]. *)
val continues_while_upper_bounded : Cmm.integer_comparison -> bool

(** [facts ~negate cmp la lb] is the list of affine inequalities implied by the
    signed comparison [la cmp lb] (negated first when [negate]). Each returned
    [f] asserts [f >= 0]. Unsigned comparisons and [Cne] cannot be expressed as
    a single affine inequality and contribute nothing. *)
val facts :
  negate:bool -> Cmm.integer_comparison -> Affine.t -> Affine.t -> Affine.t list
