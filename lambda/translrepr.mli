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

(* Translation of type-level representations to Lambda *)

(* Translate record representations to Lambda, defaulting unfilled
   sorts and turning generalized sorts into splices. *)
val transl_record_representation :
  Env.t ->
  Location.t ->
  Types.record_representation ->
  Lambda.record_representation

(* As [transl_record_representation], also returning the fields' (now
   defaulted) sorts if the representation was variable. [None] means the
   representation was already final, so the field sorts are on the
   declaration ([lbl_sort]). *)
val transl_record_representation_and_sorts :
  Env.t ->
  Location.t ->
  Types.record_representation ->
  Lambda.record_representation * variable_sorts:Jkind.Sort.Const.t array option

(* As [transl_record_representation], for [Constructor_variable]. *)
val transl_constructor_representation :
  Env.t ->
  Location.t ->
  Types.constructor_representation ->
  Lambda.constructor_representation

(* Compute a label's sort given the representation of its record *)
val label_sort_for_representation :
  Data_types.label_description ->
  Lambda.record_representation ->
  record_sort:Jkind.Sort.Const.t ->
  variable_sorts:Jkind.Sort.Const.t array option ->
  Jkind.Sort.Const.t
