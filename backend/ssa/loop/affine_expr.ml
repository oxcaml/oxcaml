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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

(* See [affine_expr.mli] for the interface. *)

module Affine = Fourier_motzkin.Affine

type t =
  | Const of int
  | Atom of int
  | Add of t * t
  | Sub of t * t
  | Scale of int * t
  | Shr_atom of
      { atom : int;
        arg : t;
        bits : int
      }
  | Lsr_atom of
      { atom : int;
        bits : int
      }
  | Or_atom of
      { atom : int;
        arg : t;
        mask : int
      }

module Range = struct
  type t =
    { lo : int64;
      hi : int64
    }

  let full = { lo = Int64.min_int; hi = Int64.max_int }

  let const c =
    let c = Int64.of_int c in
    { lo = c; hi = c }

  let shr_signed bits =
    { lo = Int64.shift_right Int64.min_int bits;
      hi = Int64.shift_right Int64.max_int bits
    }

  let shr_logical bits =
    if bits <= 0
    then full
    else { lo = 0L; hi = Int64.shift_right_logical (-1L) bits }

  (* Checked signed-64 arithmetic: [None] when the exact result does not fit,
     which for interval endpoints means the interval escapes the certifiable
     range. *)
  let add64 a b =
    let s = Int64.add a b in
    if Int64.compare (Int64.logand (Int64.logxor a s) (Int64.logxor b s)) 0L < 0
    then None
    else Some s

  let mul64 a b =
    if Int64.equal a 0L || Int64.equal b 0L
    then Some 0L
    else if
      (Int64.equal a (-1L) && Int64.equal b Int64.min_int)
      || (Int64.equal b (-1L) && Int64.equal a Int64.min_int)
    then None
    else
      let p = Int64.mul a b in
      if Int64.equal (Int64.div p a) b then Some p else None

  let add a b =
    match add64 a.lo b.lo, add64 a.hi b.hi with
    | Some lo, Some hi -> Some { lo; hi }
    | (None | Some _), _ -> None

  let neg r =
    if Int64.equal r.lo Int64.min_int
    then None
    else Some { lo = Int64.neg r.hi; hi = Int64.neg r.lo }

  let sub a b = Option.bind (neg b) (add a)

  (* The range of [x lor mask] for [x] in [r] and [mask >= 0]: [x <= x lor mask
     <= x + mask] in two's complement (the [or] can only set bits of [mask] not
     already in [x], adding at most [mask]). *)
  let or_mask mask r =
    if mask < 0
    then None
    else
      Option.map (fun hi -> { lo = r.lo; hi }) (add64 r.hi (Int64.of_int mask))

  let scale k r =
    if k = 0
    then Some { lo = 0L; hi = 0L }
    else
      let k = Int64.of_int k in
      match mul64 k r.lo, mul64 k r.hi with
      | Some a, Some b ->
        if Int64.compare a b <= 0
        then Some { lo = a; hi = b }
        else Some { lo = b; hi = a }
      | (None | Some _), _ -> None
end

(* Facts bounding an atomized shift's result, expressible only when the bound
   fits in an OCaml [int] constant. *)
let shr_range_facts ~signed ~atom ~bits : Affine.t list =
  if signed
  then
    if bits < 1
    then []
    else begin
      (* atom in [min_int64 asr bits, max_int64 asr bits]. The upper bound's
         magnitude fits an OCaml [int] for bits >= 1; the lower bound's only for
         bits >= 2 (for bits = 1 it is [min_int], whose negation is not
         representable as an affine constant). *)
      let hi = Int64.to_int (Int64.shift_right Int64.max_int bits) in
      let upper = Affine.add_const (Affine.scale (-1) (Affine.var atom)) hi in
      if bits >= 2
      then
        let lo = Int64.to_int (Int64.shift_right Int64.min_int bits) in
        [upper; Affine.add_const (Affine.var atom) (-lo)]
      else [upper]
    end
  else if bits < 1
  then []
  else begin
    (* atom in [0, 2^(64-bits) - 1]; the upper bound fits an OCaml [int] for
       bits >= 2. *)
    let lower = Affine.var atom in
    if bits >= 2
    then
      let hi = (1 lsl (64 - bits)) - 1 in
      [Affine.add_const (Affine.scale (-1) (Affine.var atom)) hi; lower]
    else [lower]
  end

let rec to_affine (e : t) : Affine.t * Affine.t list =
  match e with
  | Const c -> Affine.const c, []
  | Atom id -> Affine.var id, []
  | Add (a, b) ->
    let fa, sa = to_affine a in
    let fb, sb = to_affine b in
    Affine.add_checked fa fb, sa @ sb
  | Sub (a, b) ->
    let fa, sa = to_affine a in
    let fb, sb = to_affine b in
    Affine.add_checked fa (Affine.scale_checked (-1) fb), sa @ sb
  | Scale (k, a) ->
    let fa, sa = to_affine a in
    Affine.scale_checked k fa, sa
  | Shr_atom { atom; arg; bits } ->
    (* [atom] stands for [arg asr bits]; the exact value satisfies [2^bits *
       atom <= arg <= 2^bits * atom + 2^bits - 1], plus the range bounds implied
       by shifting a 64-bit register right. *)
    let fa, sa = to_affine arg in
    let v = Affine.var atom in
    let pow = 1 lsl bits in
    let sides =
      Affine.add_checked fa (Affine.scale_checked (-pow) v)
      :: Affine.add_checked
           (Affine.add_const_checked (Affine.scale_checked pow v) (pow - 1))
           (Affine.scale_checked (-1) fa)
      :: (shr_range_facts ~signed:true ~atom ~bits @ sa)
    in
    v, sides
  | Lsr_atom { atom; bits } ->
    (* A logical shift is not affine in its argument over signed integers, so
       only the range of the result is stated. For [bits >= 10] this is the
       shape of an OCaml array-length load, whose bound (at most [2^54 - 1]) is
       what lets the loop analyses discharge no-overflow obligations. *)
    Affine.var atom, shr_range_facts ~signed:false ~atom ~bits
  | Or_atom { atom; arg; mask } ->
    (* [atom = arg lor mask] with [mask >= 0]: in two's complement [arg <= atom
       <= arg + mask] (the [or] can only set bits of [mask] not already in
       [arg]). This is the tagging shape [len lor 1]. *)
    let fa, sa = to_affine arg in
    let v = Affine.var atom in
    let sides =
      Affine.add_checked v (Affine.scale_checked (-1) fa)
      :: Affine.add_checked
           (Affine.add_const_checked fa mask)
           (Affine.scale_checked (-1) v)
      :: sa
    in
    v, sides
