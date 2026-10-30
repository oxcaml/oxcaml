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

(* Modelling exceptions with [option].

   OxCaml code raises an exception where an operation cannot produce a result,
   for instance where a coefficient would wrap. The models return [None]
   instead, and [let* x := m in k] runs [k] on the result of [m] unless [m]
   returned [None], in which case the whole computation returns [None]: the
   same propagation an uncaught exception has. The extracted code keeps the
   [option]s. *)

Notation "'let*' x ':=' m 'in' k" :=
  (match m with Some x => k | None => None end)
  (at level 10, x name, m at level 100, k at level 200).

(* The same with a pattern, as in [let* '(a, b) := m in k]. *)
Notation "'let*' ' p ':=' m 'in' k" :=
  (match m with Some p => k | None => None end)
  (at level 10, p pattern, m at level 100, k at level 200).

(* [Option.is_some]. *)
Definition is_some {A} (o : option A) : bool :=
  match o with Some _ => true | None => false end.
