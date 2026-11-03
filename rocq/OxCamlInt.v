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

(* OxCaml's [int], modelled by Rocq's primitive signed 63-bit integer [int]
   (from the standard library's Sint63), which is exactly the same type. [to_Z]
   reads an [int] as a mathematical integer ([Z]).

   This file re-exports Sint63, names the literals the models use, and collects
   the basic facts about [int] that the proofs need. *)

From Stdlib Require Import ZArith Lia Bool.
From OxCamlStdlib Require Import OxCamlLogic.
From Stdlib Require Export Sint63.

(* The literals are named so that extraction can print them as OxCaml literals
   (a primitive-int literal would otherwise be printed as a call into Rocq's
   runtime). *)
Local Open Scope sint63_scope.
Definition zero : int := 0.
Definition one : int := 1.
Definition minus_one : int := -1.
Definition two : int := 2.
Local Close Scope sint63_scope.

Local Open Scope Z_scope.

Definition two62 : Z := 4611686018427387904.

(* OxCaml's [max_int], as a mathematical integer. *)
Definition ocaml_max_int : Z := two62 - 1.

(* Whether a mathematical integer is representable as an [int]. *)
Definition fits_int (z : Z) : bool := (- two62 <=? z) && (z <=? two62 - 1).
