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

(* Proof helpers for boolean comparisons on [Z]. *)

From Stdlib Require Import ZArith Lia.

Local Open Scope Z_scope.

(* Split on every boolean comparison in the goal, turning [x <? y] into the
   cases [x < y] / [y <= x] etc., then let [lia] (a decision procedure for
   linear integer arithmetic) finish. *)
Ltac case_zcmp :=
  repeat match goal with
         | |- context [?x <? ?y] => destruct (Z.ltb_spec x y)
         | |- context [?x <=? ?y] => destruct (Z.leb_spec x y)
         | |- context [?x =? ?y] => destruct (Z.eqb_spec x y)
         end; simpl; first [ reflexivity | exfalso; lia ].

(* Turn a hypothesis [(x <? y) = true] into [x < y], etc. *)
Ltac zcmp_hyp H :=
  first [ apply Z.ltb_lt in H | apply Z.leb_le in H | apply Z.eqb_eq in H ].
