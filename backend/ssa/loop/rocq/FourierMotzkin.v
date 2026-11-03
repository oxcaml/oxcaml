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

(* Affine inequalities over interned integer atoms, with Fourier-Motzkin
   feasibility and entailment, for the loop analyses. The extraction of this
   file, FourierMotzkin.ml next to it, is the implementation; it needs no
   review. What the functions mean, and the proofs that they compute it, are
   in FourierMotzkinProofs.v (rocq/HACKING.md, "The layout of a model").

   Every function that can overflow returns an [option], [None] standing for
   the overflow (see OxCamlOption.v); nothing wraps silently.

   Trusted parts: the [Int64] model in OxCamlInt64.v, the [sort_uniq]
   assumptions in OxCamlList.v and the overflow tests of the checked arithmetic
   in IntArith.v (each is a few lines of OxCaml to review). *)

From Stdlib Require Import ZArith Lia Bool List Recdef.
From OxCamlStdlib Require Import OxCamlLogic.
From OxCamlStdlib Require Import OxCamlOption OxCamlInt OxCamlInt64 OxCamlList
  IntArith.
Import ListNotations.

Local Open Scope sint63_scope.

(* ---- [module Affine] -------------------------------------------------- *)

Module Affine.
  Record t := { const : int; terms : list (int * int) }.

  (* The form with no terms ([const] being the field). *)
  Definition constant (c : int) : t := {| const := c; terms := [] |}.

  (* The form of one atom. *)
  Definition var (id : int) : t := {| const := zero; terms := [(id, one)] |}.

  (* The coefficient of [id] in [f], zero when absent. *)
  Definition coeff (id : int) (f : t) : int := assoc_default zero id f.(terms).

  Fixpoint terms_equal (l1 l2 : list (int * int)) : bool :=
    match l1, l2 with
    | [], [] => true
    | (i1, c1) :: l1, (i2, c2) :: l2 =>
      (i1 =? i2) && (c1 =? c2) && terms_equal l1 l2
    | _, _ => false
    end.

  Definition equal (a b : t) : bool :=
    (a.(const) =? b.(const)) && terms_equal a.(terms) b.(terms).

  Definition add_const (f : t) (c : int) : t :=
    {| const := f.(const) + c; terms := f.(terms) |}.

  Definition scale (k : int) (f : t) : t :=
    if k =? zero
    then constant zero
    else
      {| const := f.(const) * k;
         terms := map (fun '(id, c) => (id, c * k)) f.(terms) |}.

  (* The checked operations: [None] when a coefficient or the constant would
     overflow. *)
  Definition scale_checked (k : int) (f : t) : option t :=
    if k =? zero
    then Some (constant zero)
    else
      let* c := mul_ovf f.(const) k in
      let* ts :=
        map_opt (fun '(id, c) => let* c := mul_ovf c k in Some (id, c))
          f.(terms)
      in
      Some {| const := c; terms := ts |}.

  Definition add_checked (a b : t) : option t :=
    let ids := sort_uniq (map fst a.(terms) ++ map fst b.(terms)) in
    let* ts :=
      filter_map_opt
        (fun id =>
          let* c := add_ovf (coeff id a) (coeff id b) in
          Some (if c =? zero then None else Some (id, c)))
        ids
    in
    let* c := add_ovf a.(const) b.(const) in
    Some {| const := c; terms := ts |}.

  Definition add_const_checked (f : t) (c : int) : option t :=
    let* k := add_ovf f.(const) c in
    Some {| const := k; terms := f.(terms) |}.

  Definition sub_checked (a b : t) : option t :=
    let* nb := scale_checked minus_one b in
    add_checked a nb.
End Affine.

(* ---- Resolvents ------------------------------------------------------- *)

(* The local [coeff] of [resolvent], over [Int64] coefficients. *)
Definition coeff64 := assoc_default Int64.zero.

(* The local [scale_to_int64] of [resolvent]. *)
Definition scale_to_int64 (factor : Int64.t) (form : Affine.t)
    : option (Int64.t * list (int * Int64.t)) :=
  let* c := mul_ovf64 factor (Int64.of_int form.(Affine.const)) in
  let* ts :=
    map_opt
      (fun '(id, c) =>
        let* c := mul_ovf64 factor (Int64.of_int c) in Some (id, c))
      form.(Affine.terms)
  in
  Some (c, ts).

(* The resolvent of [positive_form], in which the atom has the positive
   coefficient [positive_coeff], and [negative_form], in which it has the
   negative coefficient [- negative_coeff_magnitude]: the combination
   [negative_coeff_magnitude * positive_form + positive_coeff * negative_form],
   computed in [Int64], divided by the gcd of its coefficients and read back as
   [int]s. A resolvent with no terms is normalized to the constant 0 or -1.
   [None] on overflow. *)
Definition resolvent (positive_coeff negative_coeff_magnitude : int)
    (positive_form negative_form : Affine.t) : option Affine.t :=
  let cp64 := Int64.of_int positive_coeff in
  let cn64 := Int64.of_int negative_coeff_magnitude in
  match scale_to_int64 cn64 positive_form,
        scale_to_int64 cp64 negative_form with
  | Some (pc, pterms), Some (nc, nterms) =>
    let ids := sort_uniq (map fst pterms ++ map fst nterms) in
    let* ts :=
      filter_map_opt
        (fun id =>
          let* c := add_ovf64 (coeff64 id pterms) (coeff64 id nterms) in
          Some (if Int64.equal c Int64.zero then None else Some (id, c)))
        ids
    in
    let* c := add_ovf64 pc nc in
    match ts with
    | [] =>
      Some {| Affine.const :=
                if zero <=? Int64.compare c Int64.zero then zero
                else minus_one;
              Affine.terms := [] |}
    | _ :: _ =>
      let normalization_factor :=
        fold_left
          (fun normalization_factor '(_, c) =>
            gcd64 normalization_factor (Int64.abs c))
          Int64.zero ts
      in
      let normalization_factor :=
        if zero <? Int64.compare normalization_factor Int64.one
        then normalization_factor else Int64.one
      in
      let* k := to_int_ovf (floor_div64 c normalization_factor) in
      let* ts :=
        map_opt
          (fun '(id, c) =>
            let* c := to_int_ovf (Int64.div c normalization_factor) in
            Some (id, c))
          ts
      in
      Some {| Affine.const := k; Affine.terms := ts |}
    end
  | _, _ => None
  end.

(* ---- [feasible] and [entails] ----------------------------------------- *)

(* Size limits beyond which [feasible] gives up, answering "feasible", the safe
   side. One elimination step can produce up to a quarter of the squared number
   of inequalities as resolvents, so [max_ineqs] bounds the work of a step; the
   systems the loop analyses build have a few dozen inequalities at most.
   FourierMotzkinExtract.v repeats the two values as literals. *)
Definition max_atoms : int := 64.
Definition max_ineqs : int := 500.

(* One step of [feasible]: eliminate the atom [atom]. [None] when the system is
   over the size limit; an overflowing resolvent is dropped. *)
Definition eliminate_atom (atom : int) (ineqs : list Affine.t)
    : option (list Affine.t) :=
  if zero <? compare_length_with ineqs max_ineqs
  then None
  else
    let cf f := Affine.coeff atom f in
    let pos := filter (fun f => zero <? cf f) ineqs in
    let neg := filter (fun f => cf f <? zero) ineqs in
    let zero_coeff := filter (fun f => cf f =? zero) ineqs in
    let resolvents :=
      concat_map
        (fun p =>
          filter_map
            (fun n => let* cn := neg_ovf (cf n) in resolvent (cf p) cn p n)
            neg)
        pos
    in
    Some (rev_append zero_coeff resolvents).

(* Eliminate the atoms in turn, stopping at the first [None]. *)
Fixpoint eliminate_atoms (atoms : list int) (ineqs : list Affine.t)
    : option (list Affine.t) :=
  match atoms with
  | [] => Some ineqs
  | atom :: atoms => let* ineqs := eliminate_atom atom ineqs in eliminate_atoms
    atoms ineqs
  end.

(* Whether the system may have a solution: eliminate every atom and check that
   no constant form is negative. [true], the safe answer, over the size limits
   or on overflow. *)
Definition feasible (ineqs : list Affine.t) : bool :=
  let atoms :=
    sort_uniq (concat_map (fun f => map fst f.(Affine.terms)) ineqs) in
  if zero <? compare_length_with atoms max_atoms
  then true
  else
    match eliminate_atoms atoms ineqs with
    | Some reduced => forallb (fun f => zero <=? f.(Affine.const)) reduced
    | None => true
    end.

(* Whether [goal >= 0] follows from [facts]: the facts together with [goal <
   0], i.e. [- goal - 1 >= 0], are infeasible. *)
Definition entails (facts : list Affine.t) (goal : Affine.t) : bool :=
  let negated :=
    let* ng := Affine.scale_checked minus_one goal in
    Affine.add_const_checked ng minus_one
  in
  match negated with
  | Some ng => negb (feasible (ng :: facts))
  | None => false
  end.
