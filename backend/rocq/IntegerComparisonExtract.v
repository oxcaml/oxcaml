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

(* Extraction directives for IntegerComparison.v, trusted by inspection:

   - [integer_comparison] is [Cmm.integer_comparison]; the constructor lists
     must be in the same order (compare with typing/scalar.ml).
   - [negate] and [swap] are [Cmm.negate_integer_comparison] and
     [Cmm.swap_integer_comparison]; the model's definitions are copies of the
     ones in typing/scalar.ml, so the extracted code calls the originals. *)

From Stdlib Require Import Extraction.
From OxCamlBackend Require Import IntegerComparison.

Extract Inductive integer_comparison => "Cmm.integer_comparison"
  [ "Cmm.Ceq" "Cmm.Cne" "Cmm.Clt" "Cmm.Cgt" "Cmm.Cle" "Cmm.Cge"
    "Cmm.Cult" "Cmm.Cugt" "Cmm.Cule" "Cmm.Cuge" ].
Extract Inlined Constant negate => "Cmm.negate_integer_comparison".
Extract Inlined Constant swap => "Cmm.swap_integer_comparison".
