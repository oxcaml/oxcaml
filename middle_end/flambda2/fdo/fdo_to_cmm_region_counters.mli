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

(** FDO counters that lose their edges in the translation to Cmm, e.g. those of
    the arms of a switch that becomes a table lookup. As in Simplify (see
    [Fdo_region_counters]), they are attached instead to the edges into their
    region: the stretch of straight-line code they are in, which starts at a
    function entry, a switch arm or a continuation handler.

    The translation is done downwards, so regions are entered and left as they
    are translated. A region that jumps to a handler continues into it, and gets
    its counters. *)

(* CR yusumez: The jumps translated before their handler, such as the back edges
   of a loop, miss the counters of the handler, as in Simplify. *)

type t

(** In the region of the unit's toplevel code. *)
val empty : t

(** Start translating a region inside the current one. *)
val enter_region : t -> t

(** The counters to attach to the edges into the current region. *)
val region_counters : t -> Fdo_annotation.t

(** Go back to the enclosing region, forgetting the counters of the current one.
*)
val leave_region : t -> t

(** [leave_region], keeping the counters of the current region, the handler of
    the given continuation, for the jumps to it. *)
val leave_handler : t -> Continuation.t -> t

(** The given counters lost their edges in the current region. *)
val add_counters : t -> Fdo_annotation.t -> t

(** The current region jumps to the handler of the given continuation. *)
val jump_to_handler : t -> Continuation.t -> t
