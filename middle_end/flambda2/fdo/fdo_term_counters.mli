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

(** The FDO counters (see [Fdo_counter]) carried by Flambda terms: on calls
    ([Apply_expr.callsite_counter]), on the edges of switches
    ([Switch_arm.annotations]) and on function entries
    ([Code_metadata.fdo_entry_annotation]). *)

(** [code] with [f] applied to each of its counters: those of its entry, and
    those of the calls, switch arms and code bindings in its body. *)
val map_counters_of_code :
  f:(Fdo_counter.t -> Fdo_counter.t) -> Code.t -> Code.t

(** The counters of a call to [code] that is inlined out, as the call graph
    decoded from a profile would count the call had it not been: the entry
    counters of the callee (its own, then those of the calls inlined at its
    head), in the context of the call site. *)
val of_inlined_call :
  callsite_counter:Fdo_counter.t option -> Code.t -> Fdo_annotation.t

(** The counters of the edges of the given switch arms. *)
val of_switch_arms : Switch_arm.t Target_ocaml_int.Map.t -> Fdo_annotation.t
