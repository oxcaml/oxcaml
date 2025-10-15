(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
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

(* CR metaprogramming jrickard: This file has not been code reviewed *)

(* [eval] must be the first item in this file as transl emits lambda referencing
   it as element 0 in this module. *)

(** Evaluate a quoted OCaml expression at runtime. *)
val eval : CamlinternalQuote.Code.t -> Obj.t

module type Jit_intf = sig
  val jit_load
    :  phrase_name:string
    -> Format.formatter
    -> Lambda.program
    -> (Obj.t, exn) Result.t

  val jit_lookup_symbol : string -> Obj.t option
end

(** Use the given JIT instead of the compiler's one.  This will also suppress
    reading of bundles from the executable itself.  You must then use
    [set_bundled_cmis_and_cmxs], below, instead. *)
val set_jit : (module Jit_intf) -> unit

(** Provide new .cmi and .cmx bundles to use.  Will cause an error if [set_jit]
    has not been called first.
    Calling this from an actual quotation during evaluation will result in
    a deadlock, so don't do that. *)
(* CR mshinwell: we could actually skip the (de)marshalling here *)
val set_bundled_cmis_and_cmxs
   : marshalled_cmi_bundle:string
  -> marshalled_cmx_bundle:string
  -> unit

