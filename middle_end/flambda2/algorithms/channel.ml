(******************************************************************************
 *                                  OxCaml                                    *
 *                        Basile Clément, OCamlPro                            *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 OCamlPro SAS                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
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

type 'a sender = 'a ref

type 'a receiver = 'a ref

let create v =
  let r = ref v in
  r, r

external send : 'a sender -> 'a -> unit = "%setfield0"

external recv : 'a receiver -> 'a = "%field0"

(* In OxCaml ['a Or_null.t] has layout [value_or_null] but type variables by
   default are inferred to have layout [value]. Since there is no
   upstream-compatible syntax for annotating a type variable with layout
   [value_or_null], we need to have a separate copy of the API for the [or_null]
   case -- fortunately, it is tiny. *)

type 'a or_null_ref = 'a Or_null.t ref

type 'a or_null_sender = 'a or_null_ref

type 'a or_null_receiver = 'a or_null_ref

let create_or_null (v : _ Or_null.t) =
  let r = ref v in
  r, r

external send_or_null : 'a or_null_sender -> 'a Or_null.t -> unit = "%setfield0"

external recv_or_null : 'a or_null_receiver -> 'a Or_null.t = "%field0"
