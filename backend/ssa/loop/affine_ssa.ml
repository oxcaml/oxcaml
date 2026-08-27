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

(* See [affine_ssa.mli] for the interface. *)

open Ssa.Export
module Affine = Fourier_motzkin.Affine

(* === Atom interner === *)

type ctx =
  { mutable atoms : (int * finished Value.t) list;
    mutable next : int
  }

let new_ctx () = { atoms = []; next = 0 }

let intern ctx (v : finished Value.t) : int =
  match List.find_opt (fun (_, w) -> Value.equal v w) ctx.atoms with
  | Some (id, _) -> id
  | None ->
    let id = ctx.next in
    ctx.next <- id + 1;
    ctx.atoms <- (id, v) :: ctx.atoms;
    id

(* === Recognition into {!Affine_expr} === *)

(* [Nativeint.to_int] truncates to the OCaml [int] width; the value fits iff
   converting back is the identity. *)
let fits_int (n : nativeint) =
  Nativeint.equal (Nativeint.of_int (Nativeint.to_int n)) n

let max_shift_bits = 16

(* The affine expression of [v]'s machine-integer value: decomposition through
   add/sub/shift shapes, with anything unrecognised interned as an atom.

   Each node carries its static value interval ([None] = not known to fit signed
   64-bit). A composite whose interval is unknown is atomized instead of
   decomposed: machine arithmetic wraps at 64 bits while the affine denotation
   is over unbounded integers, so a decomposition only models the machine value
   soundly when its integer evaluation provably stays in signed 64-bit range for
   every valuation of its atoms within their known ranges. A bare atom is always
   exact, whatever its range. *)
let rec expr_of_value ctx (v : finished Value.t) :
    Affine_expr.t * Affine_expr.Range.t option =
  let module E = Affine_expr in
  let module R = E.Range in
  let atomize v = E.Atom (intern ctx v), Some R.full in
  (* Keep a composite node only if certification succeeded; otherwise atomize
     [v] itself. *)
  let keep v node range =
    match range with Some _ -> node, range | None -> atomize v
  in
  let radd a b =
    match a, b with Some a, Some b -> R.add a b | (None | Some _), _ -> None
  in
  let rsub a b =
    match a, b with Some a, Some b -> R.sub a b | (None | Some _), _ -> None
  in
  let rscale k r = Option.bind r (R.scale k) in
  match[@warning "-fragile-match"] v with
  | Res ({ op = Const_int n; _ }, _) when fits_int n ->
    let c = Nativeint.to_int n in
    E.Const c, Some (R.const c)
  | Res ({ op = Intop Iadd; args = [| a; b |]; _ }, _) ->
    let ea, ra = expr_of_value ctx a in
    let eb, rb = expr_of_value ctx b in
    keep v (E.Add (ea, eb)) (radd ra rb)
  | Res ({ op = Intop Isub; args = [| a; b |]; _ }, _) ->
    let ea, ra = expr_of_value ctx a in
    let eb, rb = expr_of_value ctx b in
    keep v (E.Sub (ea, eb)) (rsub ra rb)
  | Res ({ op = Intop_imm (Iadd, k); args = [| a |]; _ }, _) ->
    let ea, ra = expr_of_value ctx a in
    keep v (E.Add (ea, E.Const k)) (radd ra (Some (R.const k)))
  | Res ({ op = Intop_imm (Isub, k); args = [| a |]; _ }, _) ->
    let ea, ra = expr_of_value ctx a in
    keep v (E.Sub (ea, E.Const k)) (rsub ra (Some (R.const k)))
  | Res ({ op = Intop_imm (Ilsl, k); args = [| a |]; _ }, _)
    when k >= 0 && k < max_shift_bits ->
    let ea, ra = expr_of_value ctx a in
    keep v (E.Scale (1 lsl k, ea)) (rscale (1 lsl k) ra)
  | Res ({ op = Intop_imm (Iasr, k); args = [| a |]; _ }, _)
    when k >= 0 && k < max_shift_bits -> (
    (* Atomized arithmetic right shift: the atom's relation to the shifted value
       and its range are expressed by the side bounds {!Affine_expr.to_affine}
       emits. Shifting any register right yields a range-certified result. The
       relation facts are only sound against a machine-exact argument form,
       which [expr_of_value] guarantees. *)
    match expr_of_value ctx a with
    | ea, Some _ ->
      ( E.Shr_atom { atom = intern ctx v; arg = ea; bits = k },
        Some (R.shr_signed k) )
    | _, None -> atomize v)
  | Res ({ op = Intop_imm (Ilsr, k); args = [| _ |]; _ }, _)
    when k >= 1 && k < 64 ->
    (* Atomized logical right shift: not affine in its argument over signed
       integers, so only the result's range is kept. This is the shape of an
       array-length load (header word shifted right), whose boundedness the loop
       analyses rely on to discharge no-overflow obligations. *)
    E.Lsr_atom { atom = intern ctx v; bits = k }, Some (R.shr_logical k)
  | Res ({ op = Intop_imm (Ior, m); args = [| a |]; _ }, _) when m >= 0 -> (
    (* Atomized or-with-mask (the tagging shape [x lor 1]): related to its
       argument by [arg <= atom <= arg + mask], which again is only sound
       against a machine-exact argument form. *)
    match expr_of_value ctx a with
    | ea, Some r ->
      E.Or_atom { atom = intern ctx v; arg = ea; mask = m }, R.or_mask m r
    | _, None -> atomize v)
  | _ -> atomize v

(* === Linearization === *)

(* Push [sides] onto [side], skipping facts already present: repeated
   linearizations of the same value re-emit identical shift/or bounds, and
   duplicated facts inflate the Fourier-Motzkin cascade quadratically. *)
let push_sides side sides =
  List.iter
    (fun f ->
      if not (List.exists (Affine.equal f) !side) then side := f :: !side)
    sides

(* Affine form of [v]'s machine-integer value, for building facts: sound to
   combine with the side facts pushed onto [side], for any runtime values. Right
   shifts and or-with-mask are atomized with their relation/range bounds;
   decompositions that cannot be certified machine-exact, and anything not
   decomposed, become atoms. *)
let linearize ctx side (v : finished Value.t) : Affine.t =
  let e, _range = expr_of_value ctx v in
  match Affine_expr.to_affine e with
  | form, sides ->
    push_sides side sides;
    form
  | exception Fourier_motzkin.Overflow ->
    (* A coefficient escaped the OCaml [int] range while building the form; fall
       back to an (always exact) atom. *)
    Affine.var (intern ctx v)

(* === Guard facts from dominating branches === *)

let cond_facts ctx side ~negate (cond : finished Value.t) : Affine.t list =
  match[@warning "-fragile-match"] cond with
  | Res ({ op = Intop (Icomp cmp); args = [| a; b |]; _ }, _) ->
    Loop_comparisons.facts ~negate cmp (linearize ctx side a)
      (linearize ctx side b)
  | Res ({ op = Intop_imm (Icomp cmp, k); args = [| a |]; _ }, _) ->
    Loop_comparisons.facts ~negate cmp (linearize ctx side a) (Affine.const k)
  | _ -> []

(* Facts that hold at entry to [target], gathered from the two-target branches
   on its immediate-dominator chain. In a two-target [Switch], [targets.(0)] is
   the false edge and [targets.(1)] the true edge. *)
let guards_at ctx side (target : finished Block.t) : Affine.t list =
  let acc = ref [] in
  let rec walk (block : finished Block.t) =
    let idom = Block.immediate_dominator block in
    if not (Block.equal idom block)
    then begin
      (match[@warning "-fragile-match"] Block.terminator idom with
      | Switch { index; targets = [| ifnot; ifso |] } ->
        (* [index] (or its negation) is a fact at [target] only if the taken
           edge [idom -> ifso] (resp. [idom -> ifnot]) *dominates* [target] —
           i.e. every path from entry to [target] traverses that specific edge;
           see {!Natural_loop.edge_dominates}. *)
        let edge_dominates (succ : finished Block.t) =
          Natural_loop.edge_dominates ~src:idom ~succ ~target
        in
        if edge_dominates ifso
        then acc := cond_facts ctx side ~negate:false index @ !acc
        else if edge_dominates ifnot
        then acc := cond_facts ctx side ~negate:true index @ !acc
      | _ -> ());
      walk idom
    end
  in
  walk target;
  !acc
