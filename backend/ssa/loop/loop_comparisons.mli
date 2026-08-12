[@@@ocaml.warning "+a-40-41-42"]

(** Pure decision logic over signed integer comparisons, shared by the SSA loop
    analyses (termination, linear-function test replacement, and the guard-fact
    collection feeding bounds-check elimination).

    This module knows nothing about SSA: it depends only on
    {!Cmm.integer_comparison} and {!Fourier_motzkin.Affine}. Every function is
    over a finite domain, so its intended semantics can be checked by exhaustive
    enumeration (or proved outright). Only one direction of each result is
    load-bearing for the passes: a positive result from {!continue_terminates}
    (and friends) licenses a rewrite, while the conservative direction is always
    safe. *)

module Affine = Fourier_motzkin.Affine

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

(** Result of the wrap-aware termination test for a monotone IV. *)
type termination =
  | Terminates  (** the continue-condition must eventually fail *)
  | Terminates_if_bound_in_range
      (** as [Terminates], provided the caller proves the bound's machine value
          [b] satisfies [b <= ocaml_max_int] (positive step) or
          [b >= -ocaml_max_int] (negative step), which excludes the increment
          ever wrapping at 64 bits *)
  | Unknown

(** [continue_terminates ~step cmp] decides whether an IV advanced by the
    constant machine step [step] each iteration, with continue-condition
    [iv cmp bound] (oriented IV-left) against a loop-invariant bound, must
    eventually exit. Wrap-around at the 64-bit machine width is accounted for:
    the cases that could spin forever by wrapping (e.g. [<=] against a bound at
    the numeric limit) are only conditionally terminating, with the no-wrap side
    condition returned for the caller to discharge. Unsigned comparisons and
    [Cne] give [Unknown]. *)
val continue_terminates : step:int -> Cmm.integer_comparison -> termination

(** Whether the (IV-left, oriented) continue-condition keeps the IV below a
    loop-invariant upper bound, i.e. is [<] or [<=]. *)
val continues_while_upper_bounded : Cmm.integer_comparison -> bool

(** [facts ~negate cmp la lb] is the list of affine inequalities implied by the
    signed comparison [la cmp lb] (negated first when [negate]). Each returned
    [f] asserts [f >= 0]. Unsigned comparisons and [Cne] cannot be expressed as
    a single affine inequality and contribute nothing. *)
val facts :
  negate:bool -> Cmm.integer_comparison -> Affine.t -> Affine.t -> Affine.t list
