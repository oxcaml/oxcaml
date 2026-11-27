(******************************************************************************
 *                                  OxCaml                                    *
 *                        Guillaume Bury, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2024 Jane Street Group LLC                                   *
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

type ('toplevel, 'in_a_closure) t =
  | At_toplevel of 'toplevel
  | In_a_closure of 'in_a_closure

val print :
  toplevel:(Format.formatter -> 'toplevel -> unit) ->
  in_a_closure:(Format.formatter -> 'in_a_closure -> unit) ->
  Format.formatter ->
  ('toplevel, 'in_a_closure) t ->
  unit

val map :
  toplevel:('toplevel_in -> 'toplevel_out) ->
  in_a_closure:('in_a_closure_in -> 'in_a_closure_out) ->
  ('toplevel_in, 'in_a_closure_in) t ->
  ('toplevel_out, 'in_a_closure_out) t
