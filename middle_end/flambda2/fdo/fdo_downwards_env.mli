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

(** For the body of an inlined function. The call site's counter already carries
    the copy context of the function the call is in, unless [called_code] is the
    code of a set being simplified (see [add_copied_code]). *)
val enter_inlined_apply :
  t -> called_code:Code.t -> callsite_counter:Fdo_counter.t option -> t

(** For terms whose counters are already in the current copy context. Also
    clears the stub callsite counter. *)
val clear_copy_context : t -> t

(** If we are in the body of an inlined stub, this returns the counter of the
    call it was inlined at. The calls of the stub with no counter of their own
    stand for that call (e.g. the call of an unboxed function wrapper to the
    main function). Other calls keep their own counters (e.g. in the default
    arguments of an optional argument wrapper, or the call of a partial
    application wrapper, which has the counter of the partial application). *)
val stub_callsite_counter : t -> Fdo_counter.t option

(** Record that the code bound under [code_id] (the new code id of a function of
    the set of closures being simplified) is a copy made where [t] is, with the
    original's counters. *)
val add_copied_code : t -> Code_id.t -> t

(** A counter of the code being simplified, renamed if that code is a copy (see
    [Fdo_counter.specialize]) so that it is not conflated with the original's or
    other copies'. *)
val specialize_fdo_counter : t -> Fdo_counter.t -> Fdo_counter.t
