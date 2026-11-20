(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
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

(** The FDO state of the downward traversal of Simplify (see [Downwards_env]),
    besides the inlining context of the counters (in [Inlined_debuginfo]). *)

type t

(** At the unit toplevel. *)
val none : t

val enter_set_of_closures : t -> inlined_debuginfo:Inlined_debuginfo.t -> t

(** For the body of [called_code], inlined at the call with [callsite_counter].
*)
val enter_inlined_apply :
  t -> called_code:Code.t -> callsite_counter:Fdo_counter.t option -> t

(** For terms whose counters are already in the current copy context. Also
    clears the stub callsite counter. *)
val clear_copy_context : t -> t

(** When inside an inlined stub, returns the callsite counter of the call to the
    stub, to be inherited by a compiler-generated call in the stub that has no
    counter of its own. *)
val stub_callsite_counter : t -> Fdo_counter.t option

(** Record that [code_id], the new code id of a function of a set of closures
    being simplified, is bound to the original code, so that its counters get
    the current copy context if it is inlined. *)
val add_unspecialized_copy : t -> Code_id.t -> t

(** A counter of the code being simplified, specialized if that code is a copy
    (see [Fdo_counter.specialize]). *)
val specialize_fdo_counter : t -> Fdo_counter.t -> Fdo_counter.t
