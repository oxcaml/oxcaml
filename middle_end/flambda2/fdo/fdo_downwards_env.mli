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

(** For the functions of a set of closures met where [t] is, in code whose
    inlining context is [inlined_debuginfo]. The sets of closures met in an
    inlined body, or in the body of a function of such a set, are copies of the
    original definitions (e.g. by an inlined functor application). *)
val enter_set_of_closures : t -> inlined_debuginfo:Inlined_debuginfo.t -> t

(** For the body of an inlined function. The call site's counter already carries
    the copy context of the function the call is in. *)
val enter_inlined_apply :
  t -> stub:bool -> callsite_counter:Fdo_counter.t option -> t

(** [t] with the copy context and {!stub_callsite_counter} of [from]. *)
val with_copy_context_of : t -> from:t -> t

(** Also clears {!stub_callsite_counter}. *)
val clear_copy_context : t -> t

(** In the body of an inlined stub, the counter of the call it was inlined at
    (already in context). The calls in a stub that have no counter of their own
    (e.g. the call of an unboxed function wrapper to the main function) stand
    for that call. *)
val stub_callsite_counter : t -> Fdo_counter.t option

(** The FDO counters of the code of a function of a set of closures met where
    [t] is, renamed (see [Fdo_counter.specialize]) if the set is a copy so that
    they are not conflated with the original's or other copies'. To be applied
    once per copy, before its code is simplified. *)
val specialize_counters_of_copied_code :
  t -> inlined_debuginfo:Inlined_debuginfo.t -> Code.t -> Code.t

(** The region (see [Fdo_region_counters]) being traversed: [None] at the unit
    toplevel, or when FDO counters are disabled. *)
val region : t -> Fdo_region_counters.Region.t option

val set_region : t -> Fdo_region_counters.Region.t -> t
