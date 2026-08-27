(******************************************************************************
 *                                  OxCaml                                    *
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

[@@@ocaml.warning "+a-40-41-42"]

(** A small expression AST reifying the affine view of a machine-integer value,
    with a pure evaluator and the static value-range arithmetic used to certify
    that a decomposition is machine-exact.

    {!Affine_ssa} recognises SSA values into this AST (the only step that
    depends on SSA operation shapes); everything here is pure arithmetic over
    interned atoms, depending only on {!Fourier_motzkin}.

    Soundness contract: machine arithmetic wraps at 64 bits, while {!to_affine}
    denotes expressions over unbounded integers. A decomposed form therefore
    models the machine value only when its integer evaluation is guaranteed to
    stay within signed 64-bit range for every valuation of its atoms within
    their known ranges — this is what {!Range} decides, and the recogniser
    atomizes any node it cannot certify. *)

module Affine = Fourier_motzkin.Affine

type t =
  | Const of int
  | Atom of int  (** an opaque value, interned by the caller *)
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of
      { atom : int;
        arg : t;
        bits : int
      }
      (** An atomized arithmetic right shift: [atom] is the interned atom
          standing for the shift's result, [arg] the shifted value. The value is
          [Atom atom]; the relation to [arg] is only expressible as the pair of
          bounds emitted by {!to_affine}. *)
  | Lsr_atom of
      { atom : int;
        bits : int
      }
      (** An atomized logical right shift. Not affine in its argument over
          signed integers, so no argument relation is kept — only the result's
          range [0 <= atom <= 2^(64-bits) - 1], emitted by {!to_affine}. For
          [bits >= 10] this is the shape of an OCaml array-length load, whose
          boundedness lets the loop analyses discharge no-overflow obligations.
      *)
  | Or_atom of
      { atom : int;
        arg : t;
        mask : int
      }
      (** An atomized bitwise-or with a non-negative constant, related to its
          argument by [arg <= atom <= arg + mask] (emitted by {!to_affine}).
          This is the tagging shape [len lor 1] of e.g. array lengths. *)

(** Static signed-64 value intervals, used by the recogniser to certify that a
    decomposition's integer evaluation cannot escape machine range. All
    operations return [None] when the resulting interval would not fit in signed
    64-bit — the caller must then atomize instead of decompose. *)
module Range : sig
  type t =
    { lo : int64;
      hi : int64
    }

  (** Any 64-bit register value. *)
  val full : t

  val const : int -> t

  (** The range of [r asr bits] for an arbitrary register [r]. *)
  val shr_signed : int -> t

  (** The range of [r lsr bits] for an arbitrary register [r]. *)
  val shr_logical : int -> t

  (** [or_mask mask r] is the range of [x lor mask] for [x] in [r] and
      [mask >= 0]; [None] for negative masks or on overflow. *)
  val or_mask : int -> t -> t option

  val add : t -> t -> t option

  val sub : t -> t -> t option

  val scale : int -> t -> t option
end

(** The affine form of the expression, together with the side inequalities (each
    [f] asserting [f >= 0]) contributed by atomized shifts: the argument
    relation [2^bits * atom <= arg <= 2^bits * atom + 2^bits - 1] for arithmetic
    shifts, and the result-range bounds for both kinds (emitted only when the
    bound fits an OCaml [int] constant). The form only models the expression
    together with these facts, so callers must include them among their
    hypotheses.

    Coefficient and constant arithmetic is overflow-checked: raises
    {!Fourier_motzkin.Overflow} rather than silently wrapping (the caller should
    then atomize the whole value). *)
val to_affine : t -> Affine.t * Affine.t list
